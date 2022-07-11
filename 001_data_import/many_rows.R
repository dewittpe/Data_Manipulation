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

baseR <- expression({
  psps_2019 <- read.csv(psps_2019_path)
})

tidyverse <- expression({
  psps_2019 <- read_csv(psps_2019_path, show_col_types = FALSE)
})

data.table <- expression({
  psps_2019 <- fread(psps_2019_path)
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

