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

# read in a smaller subset of data
psps_2019_df  <- read.csv(psps_2019_path, colClasses = column_classes, nrows = 1000)
psps_2019_tbl <- read_csv(psps_2019_path, col_type = column_classes, n_max = 1000)
psps_2019_dt  <- fread(psps_2019_path, colClasses = column_classes, nrows = 1000)

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
base_left <- expression({
  merge(x = psps_2019_df, y = pos_df, by.x = "PLACE_OF_SERVICE_CD", by.y = "code", all.x = TRUE, all.y = FALSE)
})
tidy_left <- expression({
  left_join(x = psps_2019_tbl, y = pos_tbl, by = c("PLACE_OF_SERVICE_CD" = "code"))
})
dt_left <- expression({
  merge(x = psps_2019_dt, y = pos_dt, by.x = "PLACE_OF_SERVICE_CD", by.y = "code", all.x = TRUE, all.y = FALSE)
})
dt_left_v2 <- expression({
  pos_dt[psps_2019_dt, on = c("code" = "PLACE_OF_SERVICE_CD")]
})

microbenchmark(eval(base_left), eval(tidy_left), eval(dt_left), eval(dt_left_v2))

# For data.table, why use `[` instead of merge?
# Read FAQ 1.11 https://cran.r-project.org/web/packages/data.table/vignettes/datatable-faq.html#MergeDiff
#

################################################################################
base_inner <- expression({
  merge(x = psps_2019_df, y = pos_df, by.x = "PLACE_OF_SERVICE_CD", by.y = "code", all = FALSE)
})
tidy_inner <- expression({
  inner_join(x = psps_2019_tbl, y = pos_tbl, by = c("PLACE_OF_SERVICE_CD" = "code"))
})
dt_inner <- expression({
  merge(x = psps_2019_dt, y = pos_dt, by.x = "PLACE_OF_SERVICE_CD", by.y = "code", all = FALSE)
})
dt_inner_v2 <- expression({
  psps_2019_dt[pos_dt, on = c("PLACE_OF_SERVICE_CD" = "code"), nomatch = NULL]
})
dt_inner_v3 <- expression({
  pos_dt[psps_2019_dt, on = c("code" = "PLACE_OF_SERVICE_CD"), nomatch = NULL]
})

microbenchmark(eval(base_inner), eval(tidy_inner), eval(dt_inner), eval(dt_inner_v2), eval(dt_inner_v3))

mem <- list(bi = profmem::profmem(eval(base_inner)),
            ti = profmem::profmem(eval(tidy_inner)),
            d1 = profmem::profmem(eval(dt_inner)),
            d2 = profmem::profmem(eval(dt_inner_v2)),
            d3 = profmem::profmem(eval(dt_inner_v3))
            )

# total number of bytes allocated
# format via floats to avoid integer overflow
sapply(mem, profmem::total) |>
  sapply(formatC, format = "f", big.mark = ",", digits = 0)


################################################################################
base_full <- expression({
  merge(x = psps_2019_df, y = pos_df, by.x = "PLACE_OF_SERVICE_CD", by.y = "code", all = TRUE)
})
tidy_full <- expression({
  full_join(x = psps_2019_tbl, y = pos_tbl, by = c("PLACE_OF_SERVICE_CD" = "code"))
})
dt_full <- expression({
  merge(x = psps_2019_dt, y = pos_dt, by.x = "PLACE_OF_SERVICE_CD", by.y = "code", all = TRUE)
})

microbenchmark(eval(base_full), eval(tidy_full), eval(dt_full))

mem <- list(bf = profmem::profmem(eval(base_full)),
            tf = profmem::profmem(eval(tidy_full)),
            df = profmem::profmem(eval(dt_full)))
# total number of bytes allocated
# format via floats to avoid integer overflow
sapply(mem, profmem::total) |>
  sapply(formatC, format = "f", big.mark = ",", digits = 0)


################################################################################

sessionInfo()

