# Reading in "wide" data.
#
# The three data sets
#                                             Rows  Columms
# ./000_data_sets/recs2009_public.csv       12,083      940
# ./000_data_sets/recs2015_public_v4.csv     5,686      759
# ./000_data_sets/recs2020_public_v1.csv    18,496      601
source("utilities.R")

calls <- alist(
  recs_2009_df = read.csv(recs_2009_path),
  recs_2015_df = read.csv(recs_2015_path),
  recs_2020_df = read.csv(recs_2020_path),
  recs_2009_tbl = read_csv(recs_2009_path),
  recs_2015_tbl = read_csv(recs_2015_path),
  recs_2020_tbl = read_csv(recs_2020_path),
  recs_2009_dt = fread(recs_2009_path),
  recs_2015_dt = fread(recs_2015_path),
  recs_2020_dt = fread(recs_2020_path)
               )

benchmark(calls[c(1, 4, 7)])
benchmark(calls[c(1, 4, 7) + 1])
benchmark(calls[c(1, 4, 7) + 2])

sessionInfo()

