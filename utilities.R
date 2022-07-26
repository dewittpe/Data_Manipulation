################################################################################
# Load the needed packages
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(data.table))

################################################################################
# Define some useful paths and objects
recs_2009_path <-
  file.path(".", "000_data_sets", "RECS", "2009", "recs2009_public.csv")
recs_2015_path <-
  file.path(".", "000_data_sets", "RECS", "2015", "recs2015_public_v4.csv")
recs_2020_path <-
  file.path(".", "000_data_sets", "RECS", "2020", "recs2020_public_v1.csv")

psps_2019_path <-
  file.path(".", "000_data_sets", "PSPS", "psps_2019.csv")

psps_column_classes <-
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

################################################################################
#                               HELPER FUNCTIONS                               #
################################################################################
#' Benchmarking and Memory Use
#'
#' @param x a list of calls, expected to be constructed via alist
#' @param ... additional arguments passed to microbenchmark::microbenchmark
#'
#' @return a list with the benchmark results and memory use, in bytes, for each
#' element of x
#'
#' @examples
#'
#' calls <- alist(fit1 = lm(mpg ~ wt, data = mtcars),
#'                fit2 = lm(mpg ~ wt + hp, data = mtcars),
#'                fit3 = lm(mpg ~ am*wt + hp, data = mtcars)
#'                )
#'
#' benchmark(calls, times = 10)
#'
#' @export
benchmark <- function(x, ...) {
  stopifnot(sapply(x, typeof) == "language")
  bm <- microbenchmark::microbenchmark(list = x, ...)
  mem <-
    x |>
    lapply(profmem::profmem, substitute = FALSE) |>
    sapply(profmem::total) |>
    sapply(formatC, format = "f", big.mark = ",", digits = 0) |>
    data.frame(bytes = _)

  list(benchmark = bm, "profmem" = mem)
}

################################################################################
#                                 end of file                                  #
################################################################################

