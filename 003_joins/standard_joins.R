source("utilities.R")

# read in a smaller subset of data
psps_2019_df  <- read.csv(psps_2019_path, colClasses = psps_column_classes, nrows = 1000)
psps_2019_tbl <- read_csv(psps_2019_path, col_types  = psps_column_classes, n_max = 1000)
psps_2019_dt  <- fread(psps_2019_path,    colClasses = psps_column_classes, nrows = 1000)

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
# Left Joins
calls <- alist(
  base_left = {
    merge(x = psps_2019_df, y = pos_df, by.x = "PLACE_OF_SERVICE_CD", by.y = "code", all.x = TRUE, all.y = FALSE)
  },
  tidy_left = {
    left_join(x = psps_2019_tbl, y = pos_tbl, by = c("PLACE_OF_SERVICE_CD" = "code"))
  },
  dt_left = {
    merge(x = psps_2019_dt, y = pos_dt, by.x = "PLACE_OF_SERVICE_CD", by.y = "code", all.x = TRUE, all.y = FALSE)
  },
  dt_left_v2 = {
    pos_dt[psps_2019_dt, on = c("code" = "PLACE_OF_SERVICE_CD")]
  }
)

benchmark(calls)

# For data.table, why use `[` instead of merge?
# Read FAQ 1.11 https://cran.r-project.org/web/packages/data.table/vignettes/datatable-faq.html#MergeDiff
#

################################################################################
# Inner Joins
calls <- alist(
  base_inner = {
    merge(x = psps_2019_df, y = pos_df, by.x = "PLACE_OF_SERVICE_CD", by.y = "code", all = FALSE)
  },
  tidy_inner = {
    inner_join(x = psps_2019_tbl, y = pos_tbl, by = c("PLACE_OF_SERVICE_CD" = "code"))
  },
  dt_inner = {
    merge(x = psps_2019_dt, y = pos_dt, by.x = "PLACE_OF_SERVICE_CD", by.y = "code", all = FALSE)
  },
  dt_inner_v2 = {
    psps_2019_dt[pos_dt, on = c("PLACE_OF_SERVICE_CD" = "code"), nomatch = NULL]
  },
  dt_inner_v3 = {
    pos_dt[psps_2019_dt, on = c("code" = "PLACE_OF_SERVICE_CD"), nomatch = NULL]
  }
)

benchmark(calls)

################################################################################
# Full Outer Joins
calls <- alist(
  base_full = {
    merge(x = psps_2019_df, y = pos_df, by.x = "PLACE_OF_SERVICE_CD", by.y = "code", all = TRUE)
  },
  tidy_full = {
    full_join(x = psps_2019_tbl, y = pos_tbl, by = c("PLACE_OF_SERVICE_CD" = "code"))
  },
  dt_full = {
    merge(x = psps_2019_dt, y = pos_dt, by.x = "PLACE_OF_SERVICE_CD", by.y = "code", all = TRUE)
  }
)

benchmark(calls)

################################################################################

sessionInfo()

