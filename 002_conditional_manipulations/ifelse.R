# ifelse data manipulations.
source("utilities.R")


psps_2019_base <- read.csv(psps_2019_path, colClasses = psps_column_classes)
psps_2019_tidy <- read_csv(psps_2019_path, col_types = psps_column_classes)
psps_2019_dt   <- fread(psps_2019_path, colClasses = psps_column_classes)

################################################################################
# Create an indicator column for place of service being a hospitial 
#    Place of Service Code                        Place of Service Name
#                      19               Off Campus-Outpatient Hospital
#                      21                           Inpatient Hospital
#                      22                On Campus-Outpatient Hospital

################################################################################
# First, how to find the affected rows?
calls <- alist(
  use_in = psps_2019_base$PLACE_OF_SERVICE_CD %in% c("19", "21", "22")
  ,
  use_or =
    psps_2019_base$PLACE_OF_SERVICE_CD == "19" |
    psps_2019_base$PLACE_OF_SERVICE_CD == "21" | 
    psps_2019_base$PLACE_OF_SERVICE_CD == "22"
  ,
  use_grepl_v1 =
    grepl("^(19)|(21)|(22)", psps_2019_base$PLACE_OF_SERVICE_CD)
  ,
  use_grepl_v2 =
    grepl("^(19)|(2[1-2])", psps_2019_base$PLACE_OF_SERVICE_CD)
)

with(calls, identical(eval(use_in), eval(use_or)))
with(calls, identical(eval(use_in), eval(use_grepl_v1)))
with(calls, identical(eval(use_in), eval(use_grepl_v2)))

benchmark(calls)

################################################################################
calls <- alist(
  base_ifelse_integer = {
    psps_2019_base$hospital_integer <- ifelse(psps_2019_base$PLACE_OF_SERVICE_CD %in% c("19", "21", "22"), 1L, 0L)
  }
  ,
  base_ifelse_string = {
    psps_2019_base$hospital_string <- ifelse(psps_2019_base$PLACE_OF_SERVICE_CD %in% c("19", "21", "22"), "Hospital", "Non-Hospital")
  }
  ,
  base_logical_coercion = {
    psps_2019_base$hospital_coercion <- as.integer(psps_2019_base$PLACE_OF_SERVICE_CD %in% c("19", "21", "22"))
  }
  ,
  base_row_replace = {
    psps_2019_base$hospital_row_replace <- "Non-Hospital"
    psps_2019_base[psps_2019_base$PLACE_OF_SERVICE_CD %in% c("19", "21", "22"), "hospital_row_replace"] <- "Hospital"
  }
  ,
  tidy_ifelse_integer = {
    psps_2019_tidy <-
      psps_2019_tidy %>%
      dplyr::mutate(hospital_integer = if_else(.data$PLACE_OF_SERVICE_CD %in% c("19", "21", "22"), 1L, 0L))
  }
  ,
  tidy_ifelse_string = {
    psps_2019_tidy <-
      psps_2019_tidy %>%
      dplyr::mutate(hospital_string = if_else(.data$PLACE_OF_SERVICE_CD %in% c("19", "21", "22"), "Hospital", "Non-Hospital"))
  }
  ,
  tidy_logical_coercion = {
    psps_2019_tidy <-
      psps_2019_tidy %>%
      dplyr::mutate(hospital_coercion = as.integer(.data$PLACE_OF_SERVICE_CD %in% c("19", "21", "22")))
  }
  ,
  dt_ifelse_integer = {
    psps_2019_dt[, hospital_integer := fifelse(PLACE_OF_SERVICE_CD %in% c("19", "21", "22"), 1L, 0L)]
  }
  ,
  dt_ifelse_string  = {
    psps_2019_dt[, hospital_string := fifelse(PLACE_OF_SERVICE_CD %in% c("19", "21", "22"), "Hospital", "Non-Hospital")]
  }
  ,
  dt_logical_coercion  = {
    psps_2019_dt[, hospital_coercion := as.integer(PLACE_OF_SERVICE_CD %in% c("19", "21", "22"))]
  }
  ,
  dt_row_replace = {
    psps_2019_dt[, hospital_row_replace := "Non-Hospital"]
    psps_2019_dt[PLACE_OF_SERVICE_CD %in% c("19", "21", "22"), hospital_row_replace := "Hospital"]
  }
)


benchmark(calls, times = 10)



sessionInfo()

