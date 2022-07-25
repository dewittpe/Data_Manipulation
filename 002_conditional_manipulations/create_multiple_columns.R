# multiple conditions for data manipulation

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

################################################################################
# Create an indicator column for place of service being a hospitial
#    Place of Service Code                        Place of Service Name
#                      19               Off Campus-Outpatient Hospital
#                      21                           Inpatient Hospital
#                      22                On Campus-Outpatient Hospital
#
# Build a column with a flag for hospital/non-hospital
# Build a column with NA for non-hospital, 1 for inpatient, 0 for outpatient
#
# *** will use explicit column constructions and merges ***
#

################################################################################
# Build a "link table" to map from the codes to the needed flags.  One option is
# to use just three rows but you'll have to deal with NA values after a merge
# option one
map <- data.frame(code      = c(19, 21, 22),
                  hospital  = c(1L, 1L, 1L),
                  inpatient = c(0L, 1L, 0L))

# Another map that could be built to cover all the codes in the data set:
map_df  <- data.frame(code = unique(psps_2019_base$PLACE_OF_SERVICE_CD))
map_tbl <- tibble(code     = unique(psps_2019_base$PLACE_OF_SERVICE_CD))
map_dt  <- data.table(code = unique(psps_2019_base$PLACE_OF_SERVICE_CD))

map_df$hospital  <- as.integer(map_df$code %in% c(19, 21, 22))
map_tbl$hospital <- as.integer(map_tbl$code %in% c(19, 21, 22))
map_dt$hospital  <- as.integer(map_dt$code %in% c(19, 21, 22))

map_df$inpatient <- NA_integer_
map_df[map_df$code %in% c(19, 22), "inpatient"] <- 0L
map_df[map_df$code %in% c(21),     "inpatient"] <- 1L

map_tbl$inpatient <- NA_integer_
map_tbl[map_tbl$code %in% c(19, 22), "inpatient"] <- 0L
map_tbl[map_tbl$code %in% c(21),     "inpatient"] <- 1L

map_dt$inpatient <- NA_integer_
map_dt[map_dt$code %in% c(19, 22), "inpatient"] <- 0L
map_dt[map_dt$code %in% c(21),     "inpatient"] <- 1L

data.table::setkey(map_dt, "code")
data.table::setkey(psps_2019_dt, "PLACE_OF_SERVICE_CD")

################################################################################
base_v1 <- expression({
  psps_2019_base$hospital <- as.integer(psps_2019_base$PLACE_OF_SERVICE_CD %in% c(19, 21, 22))

  psps_2019_base$inpatient <- NA_integer_
  psps_2019_base$inpatient[psps_2019_base$hospital == 1L] <-
    as.integer(psps_2019_base$PLACE_OF_SERVICE_CD[psps_2019_base$hospital == 1L] == 19)
})

base_v2 <- expression({
  psps_2019_base$hospital <- as.integer(psps_2019_base$PLACE_OF_SERVICE_CD %in% c(19, 21, 22))
  psps_2019_base$inpatient <-
    ifelse(psps_2019_base$hospital == 0L, NA_integer_, as.integer(psps_2019_base$PLACE_OF_SERVICE_CD == 19))
})

base_merge <- expression({
  # A left merge
  base::merge(
                x = psps_2019_base
              , y = map_df
              , all.x = TRUE
              , all.y = FALSE
              , by.x  = "PLACE_OF_SERVICE_CD"
              , by.y = "code"
              )
})

tidy_v1 <- expression({
  psps_2019_tidy %>%
    mutate(hospital = as.integer(.data$PLACE_OF_SERVICE_CD %in% c(19, 21, 22)),
           inpatient = NA_integer_,
           inpatient = as.integer(.data$hospital == 1 & .data$PLACE_OF_SERVICE_CD == 19)
           )
})

tidy_v2 <- expression({
  psps_2019_tidy %>%
    mutate(hospital = as.integer(.data$PLACE_OF_SERVICE_CD %in% c(19, 21, 22)),
           inpatient = ifelse(.data$hospital == 0L, NA_integer_, as.integer(.data$PLACE_OF_SERVICE_CD == 19))
           )
})

tidy_merge <- expression({
  dplyr::left_join(
                     x = psps_2019_tidy
                   , y = map_tbl
                   , by = c("PLACE_OF_SERVICE_CD" = "code")
  )
})

dt_v1 <- expression({
  psps_2019_dt[, `:=`(hospital = as.integer(PLACE_OF_SERVICE_CD %in% c(19, 21, 22)), inpatient = NA_integer_)]
  psps_2019_dt[PLACE_OF_SERVICE_CD == 19, inpatient := 1L]
})

dt_v2 <- expression({
  psps_2019_dt[, hospital := as.integer(PLACE_OF_SERVICE_CD %in% c(19, 21, 22))]
  psps_2019_dt[, inpatient := fifelse(hospital == 0L, NA_integer_, as.integer(PLACE_OF_SERVICE_CD == 19))]
})

dt_merge <- expression({
  # S3 method for data.table
  # data.table:::merge.data.table
  merge(
          x = psps_2019_dt
        , y = map_dt
        , by.x = "PLACE_OF_SERVICE_CD"
        , by.y = "code"
        , all.x = TRUE
        , all.y = FALSE
  )
})


microbenchmark(
    eval(base_v1)
  , eval(base_v2)
  , eval(base_merge)
  , eval(tidy_v1)
  , eval(tidy_v2)
  , eval(tidy_merge)
  , eval(dt_v1)
  , eval(dt_v2)
  , eval(dt_merge)
  , times = 10
  )

# memory use
mem <-
  list(
           base_v1    = profmem::profmem(eval(base_v1))
         , base_v2    = profmem::profmem(eval(base_v2))
         , base_merge = profmem::profmem(eval(base_merge))
         , tidy_v1    = profmem::profmem(eval(tidy_v1))
         , tidy_v2    = profmem::profmem(eval(tidy_v2))
         , tidy_merge = profmem::profmem(eval(tidy_merge))
         , dt_v1      = profmem::profmem(eval(dt_v1))
         , dt_v2      = profmem::profmem(eval(dt_v2))
         , dt_merge   = profmem::profmem(eval(dt_merge))
       )

# total number of bytes allocated
# format via floats to avoid integer overflow
sapply(mem, profmem::total) |>
  sapply(formatC, format = "f", big.mark = ",", digits = 0)


sessionInfo()

