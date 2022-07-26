library(microbenchmark)
library(profmem)
library(readr)
library(dplyr)
library(data.table)

psps_2019_path <- file.path(".", "000_data_sets", "PSPS", "psps_2019.csv")

column_classes <-
  c(
  "HCPCS_CD"                  = "character",
  "HCPCS_INITIAL_MODIFIER_CD" = "character",
  "PROVIDER_SPEC_CD"          = "character",
  "CARRIER_NUM"               = "integer",
  "PRICING_LOCALITY_CD"       = "character",
  "TYPE_OF_SERVICE_CD"        = "character",
  "PLACE_OF_SERVICE_CD"       = "integer",
  "HCPCS_SECOND_MODIFIER_CD"  = "character",
  "SUBMITTED_SERVICE_CNT"     = "numeric",
  "SUBMITTED_CHARGE_AMT"      = "numeric",
  "ALLOWED_CHARGE_AMT"        = "numeric",
  "DENIED_SERVICES_CNT"       = "numeric",
  "DENIED_CHARGE_AMT"         = "numeric",
  "ASSIGNED_SERVICES_CNT"     = "numeric",
  "NCH_PAYMENT_AMT"           = "numeric",
  "HCPCS_ASC_IND_CD"          = "character",
  "ERROR_IND_CD"              = "integer",
  "BETOS_CD"                  = "character")

# read in the data
psps_2019_base <- read.csv(psps_2019_path, colClasses = column_classes)
psps_2019_tidy <- read_csv(psps_2019_path, col_type = column_classes)
psps_2019_dt   <- fread(psps_2019_path, colClasses = column_classes)

# read in all the place of services
pos_df <- read.csv("000_data_sets/cms_place_of_service.cvs"
                   , col.names = c("code", "name", "description")
                   , colClasses = c("integer", "character", "character"))
pos_tbl <- read_csv("000_data_sets/cms_place_of_service.cvs"
                    , col_names = c("code", "name", "description")
                    , col_types = list("integer", "character", "character"))
pos_dt  <- fread("000_data_sets/cms_place_of_service.cvs"
                 , col.names = c("code", "name", "description")
                 , colClasses = c("integer", "character", "character"))

str(pos_dt)

################################################################################
# set key for the data.table

data.table::setkey(pos_dt, "code")
data.table::setkey(psps_2019_dt, "PLACE_OF_SERVICE_CD")

################################################################################
# I want the example here to include a nested ifelse statement.  However, I do
# not want to write it explicitly because it is just too long.  So, here is some
# R code to generate the needed R code.  This code is evaluated and then copied
# into the examples below.

# start with the last entry in the mapping data.frame
i <- nrow(pos_df)
nested_ifelse <-
  substitute(ifelse(PLACE_OF_SERVICE_CD == code, nm, x),
             list(code = pos_df$code[i], nm = pos_df$name[i], x = NA_character_)
             )

# build the rest of the nested_ifelse
for(i in rev(seq.int(nrow(pos_df) - 1L))) {
  nested_ifelse <-
    substitute(ifelse(PLACE_OF_SERVICE_CD == code, nm, x),
               list(code = pos_df$code[i], nm = pos_df$name[i], x = nested_ifelse))
}

# transition what happens when we try to evaluate this?
psps_2019_base[1:6, "PLACE_OF_SERVICE_CD"]
with(psps_2019_base[1:6, ], eval(nested_ifelse))
tryCatch(with(psps_2019_base, eval(nested_ifelse)), error = function(e) {e})

# what about case_when in dplyr
e <- paste("case_when(",
           paste(paste0("PLACE_OF_SERVICE_CD == ", pos_df$code, " ~ '", pos_df$name, "'"), collapse = ", ")
           , ")")
e <- parse(text = e)

x <- try(psps_2019_tidy %>% mutate(pos = eval(e)), silent = TRUE)
str(x)

# All left joins are viable in this case (limited iterations for memory
# concerns)
microbenchmark(
               base = merge(psps_2019_base, pos_df, all.x = TRUE, all.y = FALSE, by.x = "PLACE_OF_SERVICE_CD", by.y = "code"),
               tidy = left_join(psps_2019_tidy, pos_tbl, by = c("PLACE_OF_SERVICE_CD" = "code")),
               data.table = merge(psps_2019_dt, pos_dt, all.x = TRUE, all.y = FALSE, by.x = "PLACE_OF_SERVICE_CD", by.y = "code"),
               times = 3
               )

mem <- list(
            base       = profmem::profmem(merge(psps_2019_base, pos_df, all.x = TRUE, all.y = FALSE, by.x = "PLACE_OF_SERVICE_CD", by.y = "code")),
            tidy       = profmem::profmem(left_join(psps_2019_tidy, pos_tbl, by = c("PLACE_OF_SERVICE_CD" = "code"))),
            data.table = profmem::profmem(merge(psps_2019_dt, pos_dt, all.x = TRUE, all.y = FALSE, by.x = "PLACE_OF_SERVICE_CD", by.y = "code"))
               )

# total number of bytes allocated
# format via floats to avoid integer overflow
sapply(mem, profmem::total) |>
  sapply(formatC, format = "f", big.mark = ",", digits = 0)

################################################################################
# Working nested examples....
#
# These might be a lot better when a join isn't a viable alternative option.
# That said, I think with some planning a good link table can be built.  The
# case_when, or nested ifelse is better for when a preference is wanted for
# similar, or when a link table isn't an option.

# use the diamonds data set for the example, set up three (deep) copies for
# different paradigms
data(diamonds, package = "ggplot2")
diamonds_df <- data.frame(diamonds)
diamonds_dt <- setDT(copy(diamonds_df))

# build price categories:
# [0, 1000)
# [1000, 2500)
# [2500, 5000)
# [5000, 10000)
# [10000, 15000)
# [15000, Inf)
base_ifelse <- expression({
  with(diamonds_df,
       ifelse(price < 1000, "[$0, $1,000)",
         ifelse(price < 2500, "[$1,000, $2,500)",
           ifelse(price < 5000, "[$2,500, $5,000)",
             ifelse(price < 10000, "[$5,000, $10,000)",
               ifelse(price < 15000, "[$10,000, $15,000)", "Over $15,000")))))
       )
})

tidy_if_else <- expression({
  with(diamonds_df,
       if_else(price < 1000, "[$0, $1,000)",
         if_else(price < 2500, "[$1,000, $2,500)",
           if_else(price < 5000, "[$2,500, $5,000)",
             if_else(price < 10000, "[$5,000, $10,000)",
               if_else(price < 15000, "[$10,000, $15,000)", "Over $15,000")))))
       )
})


tidy_case_when <- expression({
  with(diamonds_df,
       case_when(price < 1000 ~ "[$0, $1,000)",
                 price < 2500 ~ "[$1,000, $2,500)",
                 price < 5000 ~ "[$2,500, $5,000)",
                 price < 10000 ~ "[$5,000, $10,000)",
                 price < 15000 ~ "[$10,000, $15,000)",
                 TRUE ~ "Over $15,000")
       )
})


dt_fifelse <- expression({
  with(diamonds_dt,
       fifelse(price < 1000, "[$0, $1,000)",
         fifelse(price < 2500, "[$1,000, $2,500)",
           fifelse(price < 5000, "[$2,500, $5,000)",
             fifelse(price < 10000, "[$5,000, $10,000)",
               fifelse(price < 15000, "[$10,000, $15,000)", "Over $15,000")))))
       )
})

dt_fcase <- expression({
  with(diamonds_dt,
       fcase(price < 1000, "[$0, $1,000)",
             price < 2500, "[$1,000, $2,500)",
             price < 5000, "[$2,500, $5,000)",
             price < 10000, "[$5,000, $10,000)",
             price < 15000, "[$10,000, $15,000)",
             default = "Over $15,000")
       )
})

# use cut in base R
base_cut <- expression({
  cut(diamonds_df$price
      , breaks = c(0, 1000, 2500, 5000, 10000, 15000, Inf)
      , right = FALSE
      , labels = c("[$0, $1,000)",
                   "[$1,000, $2,500)",
                   "[$2,500, $5,000)",
                   "[$5,000, $10,000)",
                   "[$10,000, $15,000)",
                   "Over $15,000")
  )
})


identical(as.character(eval(base_cut)), eval(base_ifelse))
identical(as.character(eval(base_cut)), eval(tidy_if_else))
identical(as.character(eval(base_cut)), eval(tidy_case_when))
identical(as.character(eval(base_cut)), eval(dt_fifelse))
identical(as.character(eval(base_cut)), eval(dt_fcase))

microbenchmark(
    eval(base_cut)
  , eval(base_ifelse)
  , eval(tidy_if_else)
  , eval(tidy_case_when)
  , eval(dt_fifelse)
  , eval(dt_fcase)
  , times = 25
  )



# Another Example
# example of case_when in tidyverse
diamonds %>%
  mutate(buy_it =
         case_when(
                   clarity == "IF" ~ "Yes, it's flawless",
                   color %in% c("D", "E", "F") ~ "Yeah, it's colorless",
                   clarity == "IF" & color %in% c("G", "H", "I", "J") ~ "Sure, it's flawless, but only nearly colorless",
                   color %in% c("S", "T", "U", "V", "X", "Y", "Z") ~ "No, light yellow color",
                   clarity %in% c("VVS1", "VVS2") ~ "Yeah, nearly perfect",
                   clarity %in% c("VS1", "VS2") ~ "Yeah, you can't see any imprefection without 10k magnification",
                   clarity %in% c("SI1", "SI2") ~ "Maybe, take a good look",
                   clarity %in% c("I1", "I2", "I3") ~ "You are going to see the flaws.",
                   TRUE ~ "maybe?" # default
                   )
         ) %>%
  group_by(buy_it) %>%
  summarize(n())

# Where are the "Sure, it's flawless, but only nearly colorless"? diamonds?
diamonds %>%
  dplyr::filter(clarity == "IF") %>%
  dplyr::group_by(clarity, color) %>%
  dplyr::summarize(n())

# similar code with nest ifelse

base_nested_ifelse <- expression({
  with(diamonds_df,
       ifelse(clarity == "IF", "Yes, it's flawless",
              ifelse(color %in% c("D", "E", "F"), "Yeah, it's colorless",
                     ifelse(clarity == "IF" & color %in% c("G", "H", "I", "J"), "Sure, it's flawless, but only nearly colorless",
                            ifelse(color %in% c("S", "T", "U", "V", "X", "Y", "Z"), "No, light yellow color",
                                   ifelse(clarity %in% c("VVS1", "VVS2"), "Yeah, nearly perfect",
                                          ifelse(clarity %in% c("VS1", "VS2"), "Yeah, you can't see any imprefection without 10k magnification",
                                                 ifelse(clarity %in% c("SI1", "SI2"), "Maybe, take a good look",
                                                        ifelse(clarity %in% c("I1", "I2", "I3") , "You are going to see the flaws.", "maybe?"))))))))
       )
})

tidy_nested_ifelse <- expression({
  with(diamonds,
       ifelse(clarity == "IF", "Yes, it's flawless",
              ifelse(color %in% c("D", "E", "F"), "Yeah, it's colorless",
                     ifelse(clarity == "IF" & color %in% c("G", "H", "I", "J"), "Sure, it's flawless, but only nearly colorless",
                            ifelse(color %in% c("S", "T", "U", "V", "X", "Y", "Z"), "No, light yellow color",
                                   ifelse(clarity %in% c("VVS1", "VVS2"), "Yeah, nearly perfect",
                                          ifelse(clarity %in% c("VS1", "VS2"), "Yeah, you can't see any imprefection without 10k magnification",
                                                 ifelse(clarity %in% c("SI1", "SI2"), "Maybe, take a good look",
                                                        ifelse(clarity %in% c("I1", "I2", "I3") , "You are going to see the flaws.", "maybe?"))))))))
       )
})

tidy_nested_if_else <- expression({
  with(diamonds,
       if_else(clarity == "IF", "Yes, it's flawless",
              if_else(color %in% c("D", "E", "F"), "Yeah, it's colorless",
                     if_else(clarity == "IF" & color %in% c("G", "H", "I", "J"), "Sure, it's flawless, but only nearly colorless",
                            if_else(color %in% c("S", "T", "U", "V", "X", "Y", "Z"), "No, light yellow color",
                                   if_else(clarity %in% c("VVS1", "VVS2"), "Yeah, nearly perfect",
                                          if_else(clarity %in% c("VS1", "VS2"), "Yeah, you can't see any imprefection without 10k magnification",
                                                 if_else(clarity %in% c("SI1", "SI2"), "Maybe, take a good look",
                                                        if_else(clarity %in% c("I1", "I2", "I3") , "You are going to see the flaws.", "maybe?"))))))))
       )
})

tidy_case_when <- expression({
  diamonds %>%
    mutate(buy_it =
           case_when(
                     clarity == "IF" ~ "Yes, it's flawless",
                     color %in% c("D", "E", "F") ~ "Yeah, it's colorless",
                     clarity == "IF" & color %in% c("G", "H", "I", "J") ~ "Sure, it's flawless, but only nearly colorless",
                     color %in% c("S", "T", "U", "V", "X", "Y", "Z") ~ "No, light yellow color",
                     clarity %in% c("VVS1", "VVS2") ~ "Yeah, nearly perfect",
                     clarity %in% c("VS1", "VS2") ~ "Yeah, you can't see any imprefection without 10k magnification",
                     clarity %in% c("SI1", "SI2") ~ "Maybe, take a good look",
                     clarity %in% c("I1", "I2", "I3") ~ "You are going to see the flaws.",
                     TRUE ~ "maybe?" # default
                     )
           )
})

dt_nested_ifelse <- expression({
  with(diamonds_dt,
       ifelse(clarity == "IF", "Yes, it's flawless",
              ifelse(color %in% c("D", "E", "F"), "Yeah, it's colorless",
                     ifelse(clarity == "IF" & color %in% c("G", "H", "I", "J"), "Sure, it's flawless, but only nearly colorless",
                            ifelse(color %in% c("S", "T", "U", "V", "X", "Y", "Z"), "No, light yellow color",
                                   ifelse(clarity %in% c("VVS1", "VVS2"), "Yeah, nearly perfect",
                                          ifelse(clarity %in% c("VS1", "VS2"), "Yeah, you can't see any imprefection without 10k magnification",
                                                 ifelse(clarity %in% c("SI1", "SI2"), "Maybe, take a good look",
                                                        ifelse(clarity %in% c("I1", "I2", "I3") , "You are going to see the flaws.", "maybe?"))))))))
       )
})

dt_nested_fifelse <- expression({
  with(diamonds_dt,
       fifelse(clarity == "IF", "Yes, it's flawless",
              fifelse(color %in% c("D", "E", "F"), "Yeah, it's colorless",
                     fifelse(clarity == "IF" & color %in% c("G", "H", "I", "J"), "Sure, it's flawless, but only nearly colorless",
                            fifelse(color %in% c("S", "T", "U", "V", "X", "Y", "Z"), "No, light yellow color",
                                   fifelse(clarity %in% c("VVS1", "VVS2"), "Yeah, nearly perfect",
                                          fifelse(clarity %in% c("VS1", "VS2"), "Yeah, you can't see any imprefection without 10k magnification",
                                                 fifelse(clarity %in% c("SI1", "SI2"), "Maybe, take a good look",
                                                        fifelse(clarity %in% c("I1", "I2", "I3") , "You are going to see the flaws.", "maybe?"))))))))
       )
})

dt_fcase <- expression({
  with(diamonds_dt,
           fcase(
                     clarity == "IF"                                     , "Yes, it's flawless",
                     color %in% c("D", "E", "F")                         , "Yeah, it's colorless",
                     clarity == "IF" & color %in% c("G", "H", "I", "J")  , "Sure, it's flawless, but only nearly colorless",
                     color %in% c("S", "T", "U", "V", "X", "Y", "Z")     , "No, light yellow color",
                     clarity %in% c("VVS1", "VVS2")                      , "Yeah, nearly perfect",
                     clarity %in% c("VS1", "VS2")                        , "Yeah, you can't see any imprefection without 10k magnification",
                     clarity %in% c("SI1", "SI2")                        , "Maybe, take a good look",
                     clarity %in% c("I1", "I2", "I3")                    , "You are going to see the flaws.",
                     default =  "maybe?"
                     )
           )
})


microbenchmark(
    eval(base_nested_ifelse)
  , eval(tidy_nested_ifelse)
  , eval(tidy_nested_if_else)
  , eval(tidy_case_when)
  , eval(dt_nested_ifelse)
  , eval(dt_nested_fifelse)
  , eval(dt_fcase)
  , times = 100)

mem <-
  list(
      base_nested_ifelse  = profmem::profmem(eval(base_nested_ifelse))
    , tidy_nested_ifelse  = profmem::profmem(eval(tidy_nested_ifelse))
    , tidy_nested_if_else = profmem::profmem(eval(tidy_nested_if_else))
    , tidy_case_when      = profmem::profmem(eval(tidy_case_when))
    , dt_nested_ifelse    = profmem::profmem(eval(dt_nested_ifelse))
    , dt_nested_fifelse   = profmem::profmem(eval(dt_nested_fifelse))
    , dt_fcase            = profmem::profmem(eval(dt_fcase))
  )

# total number of bytes allocated
# format via floats to avoid integer overflow
sapply(mem, profmem::total) |>
  sapply(formatC, format = "f", big.mark = ",", digits = 0)

################################################################################

sessionInfo()

