# Reading in "wide" data.
#
# The three data sets
#                                             Rows  Columms
# ./000_data_sets/recs2009_public.csv       12,083      940
# ./000_data_sets/recs2015_public_v4.csv     5,686      759
# ./000_data_sets/recs2020_public_v1.csv    18,496      601

library(microbenchmark)
library(profmem)
library(readr)
library(data.table)

recs_2009_path <- 
  file.path(".", "000_data_sets", "RECS", "2009", "recs2009_public.csv")
recs_2015_path <- 
  file.path(".", "000_data_sets", "RECS", "2015", "recs2015_public_v4.csv")
recs_2020_path <- 
  file.path(".", "000_data_sets", "RECS", "2020", "recs2020_public_v1.csv")

baseR <- expression({
  recs_2009 <- read.csv(recs_2009_path)
  recs_2015 <- read.csv(recs_2015_path)
  recs_2020 <- read.csv(recs_2020_path)
})

tidyverse <- expression({
  recs_2009 <- read_csv(recs_2009_path, show_col_types = FALSE)
  recs_2015 <- read_csv(recs_2015_path, show_col_types = FALSE)
  recs_2020 <- read_csv(recs_2020_path, show_col_types = FALSE)
})

data.table <- expression({
  recs_2009 <- fread(recs_2009_path)
  recs_2015 <- fread(recs_2015_path)
  recs_2020 <- fread(recs_2020_path)
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
sapply(mem, profmem::total) |>
  sapply(formatC, format = "d", big.mark = ",")

sessionInfo()

