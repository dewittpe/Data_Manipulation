source("utilities.R")

psps_2019_df  <- read.csv(psps_2019_path, colClasses = psps_column_classes)
psps_2019_tbl <- read_csv(psps_2019_path, col_types = psps_column_classes)
psps_2019_dt  <- fread(psps_2019_path, colClasses = psps_column_classes)

pos_dt  <- fread("000_data_sets/cms_place_of_service.cvs"
                 , select = c("Place of Service Code" = "integer",
                              "Place of Service Name" = "character"))
data.table::setnames(pos_dt
                     , old = c("Place of Service Code", "Place of Service Name")
                     , new = c("PLACE_OF_SERVICE_CD", "PLACE_OF_SERVICE")
)

################################################################################
# Data Set up
psps_2019_df <-
  merge(psps_2019_df
        , pos_dt
        , all.x = TRUE
        , all.y = FALSE
        , by = "PLACE_OF_SERVICE_CD"
        )

psps_2019_tbl <-
  left_join(psps_2019_tbl, pos_dt, by = "PLACE_OF_SERVICE_CD")

psps_2019_dt <-
  merge(psps_2019_dt
        , pos_dt
        , all.x = TRUE
        , all.y = FALSE
        , by = "PLACE_OF_SERVICE_CD")

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

# mapping to PAD (periferial artery disease) related procedures procedures
pad_procedures <-
  rbind(
    c(HCPCS_CD = "37220", ANATOMIC_SEGMENT = "Iliac"             , PROCEDURE = "PTA"                         , PLACEMENT = "initial"),
    c(HCPCS_CD = "0238T", ANATOMIC_SEGMENT = "Iliac"             , PROCEDURE = "atherectomy"                 , PLACEMENT = "each vessel"),
    c(HCPCS_CD = "37221", ANATOMIC_SEGMENT = "Iliac"             , PROCEDURE = "stent"                       , PLACEMENT = "initial"),
    c(HCPCS_CD = "37222", ANATOMIC_SEGMENT = "Iliac"             , PROCEDURE = "PTA"                         , PLACEMENT = "each additional"),
    c(HCPCS_CD = "37223", ANATOMIC_SEGMENT = "Iliac"             , PROCEDURE = "stent"                       , PLACEMENT = "each additional"),
    c(HCPCS_CD = "37224", ANATOMIC_SEGMENT = "Femoral/Popliteal" , PROCEDURE = "PTA"                         , PLACEMENT = ""),
    c(HCPCS_CD = "37225", ANATOMIC_SEGMENT = "Femoral/Popliteal" , PROCEDURE = "atherectomy +/- PTA"         , PLACEMENT = ""),
    c(HCPCS_CD = "37226", ANATOMIC_SEGMENT = "Femoral/Popliteal" , PROCEDURE = "stent"                       , PLACEMENT = ""),
    c(HCPCS_CD = "37227", ANATOMIC_SEGMENT = "Femoral/Popliteal" , PROCEDURE = "atherectomy w/stent +/- PTA" , PLACEMENT = ""),
    c(HCPCS_CD = "37228", ANATOMIC_SEGMENT = "Tibial/Peroneal"   , PROCEDURE = "PTA"                         , PLACEMENT = "initial"),
    c(HCPCS_CD = "37229", ANATOMIC_SEGMENT = "Tibial/Peroneal"   , PROCEDURE = "atherectomy +/- PTA"         , PLACEMENT = "initial"),
    c(HCPCS_CD = "37230", ANATOMIC_SEGMENT = "Tibial/Peroneal"   , PROCEDURE = "stent"                       , PLACEMENT = "initial"),
    c(HCPCS_CD = "37231", ANATOMIC_SEGMENT = "Tibial/Peroneal"   , PROCEDURE = "atherectomy w/stent +/- PTA" , PLACEMENT = "initial"),
    c(HCPCS_CD = "37232", ANATOMIC_SEGMENT = "Tibial/Peroneal"   , PROCEDURE = "PTA"                         , PLACEMENT = "each additional"),
    c(HCPCS_CD = "37233", ANATOMIC_SEGMENT = "Tibial/Peroneal"   , PROCEDURE = "atherectomy +/- PTA"         , PLACEMENT = "each additional"),
    c(HCPCS_CD = "37234", ANATOMIC_SEGMENT = "Tibial/Peroneal"   , PROCEDURE = "stent"                       , PLACEMENT = "each additional"),
    c(HCPCS_CD = "37235", ANATOMIC_SEGMENT = "Tibial/Peroneal"   , PROCEDURE = "atherectomy w/stent +/- PTA" , PLACEMENT = "each additional")
)
pad_procedures <- as.data.frame(pad_procedures)

# use inner merge to get only the pad procedures
pad_2019_df <- merge(psps_2019_df, pad_procedures, all = FALSE, by = "HCPCS_CD")
pad_2019_tbl <- inner_join(psps_2019_tbl, pad_procedures, by = "HCPCS_CD")
pad_2019_dt <- merge(psps_2019_dt, pad_procedures, all = FALSE, by = "HCPCS_CD")

# We need to get several values for the aggregation, sums for the 
# submitted, services and dollar amounts.
# 
# Aggregations: for the year 2019
# by provider
# by anatomic segment
# by PLACE_OF_SERVICE
# all combinations thereof (eight total aggregations)

calls <- alist(
  base = {
    list(
      "overall" =
        aggregate(cbind(SUBMITTED_SERVICE_CNT, SUBMITTED_CHARGE_AMT) ~ 1, data = pad_2019_df, FUN = sum),
      "by provider" =
        aggregate(cbind(SUBMITTED_SERVICE_CNT, SUBMITTED_CHARGE_AMT) ~ PROVIDER_GROUP, data = pad_2019_df, FUN = sum),
      "by anatomic segment" =
        aggregate(cbind(SUBMITTED_SERVICE_CNT, SUBMITTED_CHARGE_AMT) ~ ANATOMIC_SEGMENT, data = pad_2019_df, FUN = sum),
      "by place of service" =
        aggregate(cbind(SUBMITTED_SERVICE_CNT, SUBMITTED_CHARGE_AMT) ~ PLACE_OF_SERVICE, data = pad_2019_df, FUN = sum),
      "by provider and anatomic segment" =
        aggregate(cbind(SUBMITTED_SERVICE_CNT, SUBMITTED_CHARGE_AMT) ~ PROVIDER_GROUP + ANATOMIC_SEGMENT, data = pad_2019_df, FUN = sum),
      "by provider, place of service" =
        aggregate(cbind(SUBMITTED_SERVICE_CNT, SUBMITTED_CHARGE_AMT) ~ PROVIDER_GROUP + PLACE_OF_SERVICE, data = pad_2019_df, FUN = sum),
      "by segment, place of service" =
        aggregate(cbind(SUBMITTED_SERVICE_CNT, SUBMITTED_CHARGE_AMT) ~ ANATOMIC_SEGMENT + PLACE_OF_SERVICE, data = pad_2019_df, FUN = sum),
      "by provider, segment, place of service" =
        aggregate(cbind(SUBMITTED_SERVICE_CNT, SUBMITTED_CHARGE_AMT) ~ PROVIDER_GROUP + ANATOMIC_SEGMENT + PLACE_OF_SERVICE, data = pad_2019_df, FUN = sum)
    )
  }
  ,
  tidyverse = {
    list(
         "overall" = 
            pad_2019_tbl %>%
              summarize(SUBMITTED_SERVICE_CNT = sum(SUBMITTED_SERVICE_CNT, na.rm = TRUE),
                        SUBMITTED_CHARGE_AMT  = sum(SUBMITTED_CHARGE_AMT, na.rm = TRUE)),
         "by provider" = 
           pad_2019_tbl %>%
             group_by(.data$PROVIDER_GROUP) %>%
             summarize(SUBMITTED_SERVICE_CNT = sum(SUBMITTED_SERVICE_CNT, na.rm = TRUE),
                       SUBMITTED_CHARGE_AMT  = sum(SUBMITTED_CHARGE_AMT, na.rm = TRUE)),
         "by segment" = 
           pad_2019_tbl %>%
             group_by(.data$ANATOMIC_SEGMENT) %>%
             summarize(SUBMITTED_SERVICE_CNT = sum(SUBMITTED_SERVICE_CNT, na.rm = TRUE),
                       SUBMITTED_CHARGE_AMT  = sum(SUBMITTED_CHARGE_AMT, na.rm = TRUE)),
         "by place of service" = 
           pad_2019_tbl %>%
             group_by(.data$PLACE_OF_SERVICE) %>%
             summarize(SUBMITTED_SERVICE_CNT = sum(SUBMITTED_SERVICE_CNT, na.rm = TRUE),
                       SUBMITTED_CHARGE_AMT  = sum(SUBMITTED_CHARGE_AMT, na.rm = TRUE)),
         "by provide, segment" = 
           pad_2019_tbl %>%
             group_by(.data$PROVIDER_GROUP, .data$ANATOMIC_SEGMENT) %>%
             summarize(SUBMITTED_SERVICE_CNT = sum(SUBMITTED_SERVICE_CNT, na.rm = TRUE),
                       SUBMITTED_CHARGE_AMT  = sum(SUBMITTED_CHARGE_AMT, na.rm = TRUE)),
         "by provide, place of service" = 
           pad_2019_tbl %>%
             group_by(.data$PROVIDER_GROUP, .data$PLACE_OF_SERVICE) %>%
             summarize(SUBMITTED_SERVICE_CNT = sum(SUBMITTED_SERVICE_CNT, na.rm = TRUE),
                       SUBMITTED_CHARGE_AMT  = sum(SUBMITTED_CHARGE_AMT, na.rm = TRUE)),
         "by segment, place of service" = 
           pad_2019_tbl %>%
             group_by(.data$ANATOMIC_SEGMENT, .data$PLACE_OF_SERVICE) %>%
             summarize(SUBMITTED_SERVICE_CNT = sum(SUBMITTED_SERVICE_CNT, na.rm = TRUE),
                       SUBMITTED_CHARGE_AMT  = sum(SUBMITTED_CHARGE_AMT, na.rm = TRUE)),
         "by provider, segment, place of service" = 
           pad_2019_tbl %>%
             group_by(.data$PROVIDER_GROUP, .data$ANATOMIC_SEGMENT, .data$PLACE_OF_SERVICE) %>%
             summarize(SUBMITTED_SERVICE_CNT = sum(SUBMITTED_SERVICE_CNT, na.rm = TRUE),
                       SUBMITTED_CHARGE_AMT  = sum(SUBMITTED_CHARGE_AMT, na.rm = TRUE))
      )
  }
  ,
  dt_by = {
    this_summary <- quote(
                     .(SUBMITTED_SERVICE_CNT = sum(SUBMITTED_SERVICE_CNT, na.rm = TRUE),
                       SUBMITTED_CHARGE_AMT  = sum(SUBMITTED_CHARGE_AMT, na.rm = TRUE)
                       )
                     )
    list(
      "overall"                                = pad_2019_dt[, eval(this_summary)],
      "by provider"                            = pad_2019_dt[, eval(this_summary), by = .(PROVIDER_GROUP)],
      "by segment"                             = pad_2019_dt[, eval(this_summary), by = .(ANATOMIC_SEGMENT)],
      "by place of service"                    = pad_2019_dt[, eval(this_summary), by = .(PLACE_OF_SERVICE)],
      "by provider, segment"                   = pad_2019_dt[, eval(this_summary), by = .(PROVIDER_GROUP, ANATOMIC_SEGMENT)],
      "by provider, place of service"          = pad_2019_dt[, eval(this_summary), by = .(PROVIDER_GROUP, PLACE_OF_SERVICE)],
      "by segment, place of service"           = pad_2019_dt[, eval(this_summary), by = .(ANATOMIC_SEGMENT, PLACE_OF_SERVICE)],
      "by provider, segment, place of service" = pad_2019_dt[, eval(this_summary), by = .(PROVIDER_GROUP, ANATOMIC_SEGMENT, PLACE_OF_SERVICE)]
      )
  }
  ,
  dt_keyby = {
    this_summary <- quote(
                     .(SUBMITTED_SERVICE_CNT = sum(SUBMITTED_SERVICE_CNT, na.rm = TRUE),
                       SUBMITTED_CHARGE_AMT  = sum(SUBMITTED_CHARGE_AMT, na.rm = TRUE)
                       )
                     )
    list(
      "overall"                                = pad_2019_dt[, eval(this_summary)],
      "by provider"                            = pad_2019_dt[, eval(this_summary), keyby = .(PROVIDER_GROUP)],
      "by segment"                             = pad_2019_dt[, eval(this_summary), keyby = .(ANATOMIC_SEGMENT)],
      "by place of service"                    = pad_2019_dt[, eval(this_summary), keyby = .(PLACE_OF_SERVICE)],
      "by provider, segment"                   = pad_2019_dt[, eval(this_summary), keyby = .(PROVIDER_GROUP, ANATOMIC_SEGMENT)],
      "by provider, place of service"          = pad_2019_dt[, eval(this_summary), keyby = .(PROVIDER_GROUP, PLACE_OF_SERVICE)],
      "by segment, place of service"           = pad_2019_dt[, eval(this_summary), keyby = .(ANATOMIC_SEGMENT, PLACE_OF_SERVICE)],
      "by provider, segment, place of service" = pad_2019_dt[, eval(this_summary), keyby = .(PROVIDER_GROUP, ANATOMIC_SEGMENT, PLACE_OF_SERVICE)]
      )
  }
)



eval(calls$base)
eval(calls$tidyverse)
eval(calls$dt_by)
eval(calls$dt_keyby)

benchmark(calls)


