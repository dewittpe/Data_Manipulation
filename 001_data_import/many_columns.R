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

baseR <- expression({
  recs_2009 <- read.csv("./000_data_sets/recs2009_public.csv")
  recs_2015 <- read.csv("./000_data_sets/recs2015_public_v4.csv")
  recs_2020 <- read.csv("./000_data_sets/recs2020_public_v1.csv")
})

tidyverse <- expression({
  recs_2009 <- read_csv("./000_data_sets/recs2009_public.csv",    show_col_types = FALSE)
  recs_2015 <- read_csv("./000_data_sets/recs2015_public_v4.csv", show_col_types = FALSE)
  recs_2020 <- read_csv("./000_data_sets/recs2020_public_v1.csv", show_col_types = FALSE)
})

data.table <- expression({
  recs_2009 <- fread("./000_data_sets/recs2009_public.csv")
  recs_2015 <- fread("./000_data_sets/recs2015_public_v4.csv")
  recs_2020 <- fread("./000_data_sets/recs2020_public_v1.csv")
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

