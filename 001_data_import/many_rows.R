# Reading in "long" data.
#
# The three data sets
#                                       Rows  Columms
# ./000_data_sets/psps_2019.csv   14,174,975       18 

library(microbenchmark)
library(profmem)
library(readr)
library(data.table)

psps_2019_path <- 
  file.path(".", "000_data_sets", "PSPS", "psps_2019.csv")

base <- read.csv(psps_2019_path)
tidy <- read_csv(psps_2019_path)
DT   <- fread(psps_2019_path)

# DEFAULT BEHAVIOR IS DIFFERENT!!!!

# base and data.table have the same column class, tidy does not
sapply(base, class) == sapply(DT, class)
sapply(base, class) == sapply(tidy, class)

# look for leading zeros and integer vs numeric
str(base[c("CARRIER_NUM", "PLACE_OF_SERVICE_CD", "ERROR_IND_CD")])
str(tidy[c("CARRIER_NUM", "PLACE_OF_SERVICE_CD", "ERROR_IND_CD")])

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
baseR <- expression({
  psps_2019 <- read.csv(psps_2019_path, colClasses = column_classes)
})

tidyverse <- expression({
  psps_2019 <- read_csv(psps_2019_path, col_type = column_classes)
})

data.table <- expression({
  psps_2019 <- fread(psps_2019_path, colClasses = column_classes)
})

# benchmark
mb <- microbenchmark(eval(baseR), eval(tidyverse), eval(data.table), times = 5)
mb

# memory use
mem <-
  list(
       baseR      = profmem::profmem(eval(baseR)),
       tidyverse  = profmem::profmem(eval(tidyverse)),
       data.table = profmem::profmem(eval(data.table))
       )

# total number of bytes allocated
# format via floats to avoid integer overflow
sapply(mem, profmem::total) |>
  sapply(formatC, format = "f", big.mark = ",", digits = 0)

sessionInfo()

