# ifelse data manipulations.

library(microbenchmark)
library(profmem)
library(readr)
library(dplyr)
library(data.table)

psps_2019_path <- file.path(".", "000_data_sets", "PSPS", "psps_2019.csv")
psps_2019_base <- read.csv(psps_2019_path)
psps_2019_tidy <- read_csv(psps_2019_path, show_col_types = FALSE)
psps_2019_dt   <- fread(psps_2019_path)

################################################################################
# Create an indicator column for place of service being a hospitial 
#    Place of Service Code                        Place of Service Name
#                      19               Off Campus-Outpatient Hospital
#                      21                           Inpatient Hospital
#                      22                On Campus-Outpatient Hospital

################################################################################
# First, how to find the affected rows?
use_in <- expression({psps_2019_base$PLACE_OF_SERVICE_CD %in% c("19", "21", "22")})
use_or <- expression({
  psps_2019_base$PLACE_OF_SERVICE_CD == "19" |
  psps_2019_base$PLACE_OF_SERVICE_CD == "21" | 
  psps_2019_base$PLACE_OF_SERVICE_CD == "22"
})
use_grepl_v1 <- expression({
  grepl("^(19)|(21)|(22)", psps_2019_base$PLACE_OF_SERVICE_CD)
})
use_grepl_v2 <- expression({
  grepl("^(19)|(2[1-2])", psps_2019_base$PLACE_OF_SERVICE_CD)
})

identical(eval(use_in), eval(use_or))
identical(eval(use_in), eval(use_grepl_v1))
identical(eval(use_in), eval(use_grepl_v2))

microbenchmark(eval(use_in),
               eval(use_or),
               eval(use_grepl_v1),
               eval(use_grepl_v2),
               times = 10
          )

mem <-
  list(
       use_in       = profmem::profmem(eval(use_in)),
       use_or       = profmem::profmem(eval(use_or)),
       use_grepl_v1 = profmem::profmem(eval(use_grepl_v1)),
       use_grepl_v2 = profmem::profmem(eval(use_grepl_v2))
       )

# total number of bytes allocated
sapply(mem, profmem::total) |>
  sapply(formatC, format = "d", big.mark = ",")


################################################################################
base_ifelse_integer <- expression({
  psps_2019_base$hospital_integer <- ifelse(psps_2019_base$PLACE_OF_SERVICE_CD %in% c("19", "21", "22"), 1L, 0L)
})

base_ifelse_string <- expression({
  psps_2019_base$hospital_string <- ifelse(psps_2019_base$PLACE_OF_SERVICE_CD %in% c("19", "21", "22"), "Hospital", "Non-Hospital")
})

base_logical_coercion <- expression({
  psps_2019_base$hospital_coercion <- as.integer(psps_2019_base$PLACE_OF_SERVICE_CD %in% c("19", "21", "22"))
})

base_row_replace <- expression({
  psps_2019_base$hospital_row_replace <- "Non-Hospital"
  psps_2019_base[psps_2019_base$PLACE_OF_SERVICE_CD %in% c("19", "21", "22"), "hospital_row_replace"] <- "Hospital"
})

tidy_ifelse_integer <- expression({
  psps_2019_tidy <-
    psps_2019_tidy %>%
    dplyr::mutate(hospital_integer = if_else(.data$PLACE_OF_SERVICE_CD %in% c("19", "21", "22"), 1L, 0L))
})

tidy_ifelse_string <- expression({
  psps_2019_tidy <-
    psps_2019_tidy %>%
    dplyr::mutate(hospital_string = if_else(.data$PLACE_OF_SERVICE_CD %in% c("19", "21", "22"), "Hospital", "Non-Hospital"))
})

tidy_logical_coercion <- expression({
  psps_2019_tidy <-
    psps_2019_tidy %>%
    dplyr::mutate(hospital_coercion = as.integer(.data$PLACE_OF_SERVICE_CD %in% c("19", "21", "22")))
})

dt_ifelse_integer <- expression({
  psps_2019_dt[, hospital_integer := fifelse(PLACE_OF_SERVICE_CD %in% c("19", "21", "22"), 1L, 0L)]
})
dt_ifelse_string  <- expression({
  psps_2019_dt[, hospital_string := fifelse(PLACE_OF_SERVICE_CD %in% c("19", "21", "22"), "Hospital", "Non-Hospital")]
})
dt_logical_coercion  <- expression({
  psps_2019_dt[, hospital_coercion := as.integer(PLACE_OF_SERVICE_CD %in% c("19", "21", "22"))]
})
dt_row_replace <- expression({
  psps_2019_dt[, hospital_row_replace := "Non-Hospital"]
  psps_2019_dt[PLACE_OF_SERVICE_CD %in% c("19", "21", "22"), hospital_row_replace := "Hospital"]
})


microbenchmark(
  eval(base_ifelse_integer),
  eval(base_ifelse_string),
  eval(base_logical_coercion),
  eval(base_row_replace),
  eval(tidy_ifelse_integer),
  eval(tidy_ifelse_string),
  eval(tidy_logical_coercion),
  eval(dt_ifelse_integer),
  eval(dt_ifelse_string),
  eval(dt_logical_coercion),
  eval(dt_row_replace),
  times = 10
  )

# memory use
mem <-
  list(
       base_ifelse_integer   = profmem::profmem(eval(base_ifelse_integer)),
       base_ifelse_string    = profmem::profmem(eval(base_ifelse_string)),
       base_logical_coercion = profmem::profmem(eval(base_logical_coercion)),
       base_row_replace      = profmem::profmem(eval(base_row_replace)),
       tidy_ifelse_integer   = profmem::profmem(eval(tidy_ifelse_integer)),
       tidy_ifelse_string    = profmem::profmem(eval(tidy_ifelse_string)),
       tidy_logical_coercion = profmem::profmem(eval(tidy_logical_coercion)),
       dt_ifelse_integer     = profmem::profmem(eval(dt_ifelse_integer)),
       dt_ifelse_string      = profmem::profmem(eval(dt_ifelse_string)),
       dt_logical_coercion   = profmem::profmem(eval(dt_logical_coercion)),
       dt_row_replace        = profmem::profmem(eval(dt_row_replace))
       )

# total number of bytes allocated
# format via floats to avoid integer overflow
sapply(mem, profmem::total) |>
  sapply(formatC, format = "f", big.mark = ",", digits = 0)

sessionInfo()

