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
psps_2019_df  <- read.csv(psps_2019_path, colClasses = column_classes)
psps_2019_tbl <- read_csv(psps_2019_path, col_type = column_classes)
psps_2019_dt  <- fread(psps_2019_path, colClasses = column_classes)

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
# order _can_ matter

base_small_big_outer <- expression({
  merge(x = pos_df, y = psps_2019_df, by.x = "code", by.y = "PLACE_OF_SERVICE_CD", all = TRUE)
})

base_big_small_outer <- expression({
  merge(x = psps_2019_df, y = pos_df, by.x = "PLACE_OF_SERVICE_CD", by.y = "code", all = TRUE)
})

tidy_small_big_outer <- expression({
  full_join(x = pos_tbl, y = psps_2019_tbl, by = c("code" = "PLACE_OF_SERVICE_CD"))
})

tidy_big_small_outer <- expression({
  full_join(x = psps_2019_tbl, y = pos_tbl, by = c("PLACE_OF_SERVICE_CD" = "code"))
})

dt_small_big_outer <- expression({
  merge(x = pos_dt, y = psps_2019_dt, by.x = "code", by.y = "PLACE_OF_SERVICE_CD", all = TRUE)
})

dt_big_small_outer <- expression({
  merge(x = psps_2019_dt, y = pos_dt, by.x = "PLACE_OF_SERVICE_CD", by.y = "code", all = TRUE)
})

microbenchmark(eval(base_small_big_outer), eval(base_big_small_outer),
               eval(tidy_small_big_outer), eval(tidy_big_small_outer),
               eval(dt_small_big_outer), eval(dt_big_small_outer),
               times = 5)

mem <- list(
              bsb = profmem::profmem(eval(base_small_big_outer))
            , bbs = profmem::profmem(eval(base_big_small_outer))
            , tsb = profmem::profmem(eval(tidy_small_big_outer))
            , tbs = profmem::profmem(eval(tidy_big_small_outer))
            , dsb = profmem::profmem(eval(dt_small_big_outer))
            , dbs = profmem::profmem(eval(dt_big_small_outer))
)

# total number of bytes allocated
# format via floats to avoid integer overflow
sapply(mem, profmem::total) |>
  sapply(formatC, format = "f", big.mark = ",", digits = 0)




################################################################################

sessionInfo()

