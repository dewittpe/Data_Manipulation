source("utilities.R")

psps_2019_df  <- read.csv(psps_2019_path, colClasses = psps_column_classes)
psps_2019_tbl <- read_csv(psps_2019_path, col_types  = psps_column_classes)
psps_2019_dt  <- fread(psps_2019_path,    colClasses = psps_column_classes)

# Let's define a provider group to make the reshaping easier to view
psps_2019_df$PROVIDER_GROUP <-
  with(psps_2019_df,
       fcase(
             PROVIDER_SPEC_CD %in% c("30", "36", "94"),                   "Radiology",
             PROVIDER_SPEC_CD %in% c("06", "11", "21", "76", "C3", "C7"), "Cardiology",
             PROVIDER_SPEC_CD %in% c("02", "28", "33", "77", "78"),       "Surgery",
             default = "Other"
       )
  )

psps_2019_dt$PROVIDER_GROUP <- psps_2019_df$PROVIDER_GROUP
psps_2019_tbl$PROVIDER_GROUP <- psps_2019_df$PROVIDER_GROUP


# to the best of my knowlege, aggregation, if needed, needs to be done before
# reshaping with stats::reshape.

calls <- alist(
  base = {
    agg <- aggregate(SUBMITTED_SERVICE_CNT ~ HCPCS_CD + PLACE_OF_SERVICE_CD + PROVIDER_GROUP,
                     data = psps_2019_df,
                     FUN = function(x) {sum(x, na.rm = TRUE) })
    reshape(
            data = agg,
            direction = "wide",
            idvar = c("HCPCS_CD", "PLACE_OF_SERVICE_CD"),
            timevar = "PROVIDER_GROUP"
            )
  }
  ,
  tidyverse = {
    psps_2019_tbl %>%
      tidyr::pivot_wider(
                         id_cols = c(HCPCS_CD, PLACE_OF_SERVICE_CD),
                         names_from = PROVIDER_GROUP,
                         values_from = SUBMITTED_SERVICE_CNT,
                         values_fn = ~ sum(.x, na.rm = TRUE)
      )
  }
  ,
  dt = {
    data.table::dcast(
                      data          = psps_2019_dt,
                      formula       = HCPCS_CD + PLACE_OF_SERVICE_CD ~ PROVIDER_GROUP,
                      value.var     = "SUBMITTED_SERVICE_CNT",
                      fun.aggregate = function(x) {sum(x, na.rm = TRUE)},
                      )
  })

eval(calls$base) |> head()
eval(calls$tidyverse)
eval(calls$dt)

benchmark(calls, times = 25)


