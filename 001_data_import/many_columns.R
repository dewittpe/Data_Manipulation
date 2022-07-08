# Reading in "wide" data.
#
# The three data sets
#                                             Rows  Columms
# ./000_data_sets/recs2009_public.csv       12,083      940
# ./000_data_sets/recs2015_public_v4.csv     5,686      759
# ./000_data_sets/recs2020_public_v1.csv    18,496      601

library(microbenchmark)
library(profmem)

baseR <- expression({
  recs_2009 <- read.csv("./000_data_sets/recs2009_public.csv")
  recs_2015 <- read.csv("./000_data_sets/recs2015_public_v4.csv")
  recs_2020 <- read.csv("./000_data_sets/recs2020_public_v1.csv")
})

tidyverse <- expression({
  recs_2009 <- readr::read_csv("./000_data_sets/recs2009_public.csv")
  recs_2015 <- readr::read_csv("./000_data_sets/recs2015_public_v4.csv")
  recs_2020 <- readr::read_csv("./000_data_sets/recs2020_public_v1.csv")
})

data.table <- expression({
  recs_2009 <- data.table::fread("./000_data_sets/recs2009_public.csv")
  recs_2015 <- data.table::fread("./000_data_sets/recs2015_public_v4.csv")
  recs_2020 <- data.table::fread("./000_data_sets/recs2020_public_v1.csv")
})

# timeing
microbenchmark(eval(baseR), eval(tidyverse), eval(data.table))

# memory use
mem <-
  list(
       baseR      = profmem::profmem(eval(baseR)),
       tidyverse  = profmem::profmem(eval(tidyverse)),
       data.table = profmem::profmem(eval(data.table))
       )

sapply(mem, profmem::total) |> sapply(formatC, format = "d", big.mark = ",")

sessionInfo()

