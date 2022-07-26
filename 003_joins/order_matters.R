source("utilities.R")

# read in the data
psps_2019_df  <- read.csv(psps_2019_path, colClasses = psps_column_classes)
psps_2019_tbl <- read_csv(psps_2019_path, col_types = psps_column_classes)
psps_2019_dt  <- fread(psps_2019_path, colClasses = psps_column_classes)

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
calls <- alist(

  base_small_big_outer = {
    merge(x = pos_df, y = psps_2019_df, by.x = "code", by.y = "PLACE_OF_SERVICE_CD", all = TRUE)
  },

  base_big_small_outer = {
    merge(x = psps_2019_df, y = pos_df, by.x = "PLACE_OF_SERVICE_CD", by.y = "code", all = TRUE)
  },

  tidy_small_big_outer = {
    full_join(x = pos_tbl, y = psps_2019_tbl, by = c("code" = "PLACE_OF_SERVICE_CD"))
  },

  tidy_big_small_outer = {
    full_join(x = psps_2019_tbl, y = pos_tbl, by = c("PLACE_OF_SERVICE_CD" = "code"))
  },

  dt_small_big_outer = {
    merge(x = pos_dt, y = psps_2019_dt, by.x = "code", by.y = "PLACE_OF_SERVICE_CD", all = TRUE)
  },

  dt_big_small_outer = {
    merge(x = psps_2019_dt, y = pos_dt, by.x = "PLACE_OF_SERVICE_CD", by.y = "code", all = TRUE)
  }

)

benchmark(calls, times = 5)





################################################################################

sessionInfo()

