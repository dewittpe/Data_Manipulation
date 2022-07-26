# The three data sets
#                                             Rows  Columms
# ./000_data_sets/recs2009_public.csv       12,083      940
# ./000_data_sets/recs2015_public_v4.csv     5,686      759
# ./000_data_sets/recs2020_public_v1.csv    18,496      601

source("utilities.R")

recs_2009_df <- read.csv(recs_2009_path, colClasses = "character")
recs_2015_df <- read.csv(recs_2015_path, colClasses = "character")
recs_2020_df <- read.csv(recs_2020_path, colClasses = "character")

recs_2009_tbl <- read_csv(recs_2009_path, col_types = cols(.default = "character"))
recs_2015_tbl <- read_csv(recs_2015_path, col_types = cols(.default = "character"))
recs_2020_tbl <- read_csv(recs_2020_path, col_types = cols(.default = "character"))

recs_2009_dt <- fread(recs_2009_path, colClasses = "character")
recs_2015_dt <- fread(recs_2015_path, colClasses = "character")
recs_2020_dt <- fread(recs_2020_path, colClasses = "character")


################################################################################
# stack a data frame ontop of itself
calls <-
  alist(
    base_09 = rbind(recs_2009_df, recs_2009_df),
    base_15 = rbind(recs_2015_df, recs_2015_df),
    base_20 = rbind(recs_2020_df, recs_2020_df),
    tidy_09 = bind_rows(recs_2009_tbl, recs_2009_tbl),
    tidy_15 = bind_rows(recs_2015_tbl, recs_2015_tbl),
    tidy_20 = bind_rows(recs_2020_tbl, recs_2020_tbl),
    dt_09   = rbind(recs_2009_dt, recs_2009_dt),
    dt_15   = rbind(recs_2015_dt, recs_2015_dt),
    dt_20   = rbind(recs_2020_dt, recs_2020_dt)
  )

benchmark(calls[c(1, 4, 7)])
benchmark(calls[c(1, 4, 7) + 1])
benchmark(calls[c(1, 4, 7) + 2])

################################################################################
# stack up a unknown number of data.frames

dfs  <- split(recs_2020_df,  f = recs_2020_df$IECC_climate_code)
tbls <- split(recs_2020_tbl, f = recs_2020_tbl$IECC_climate_code)
dts  <- split(recs_2020_dt,  f = recs_2020_dt$IECC_climate_code)

calls <- alist(
               base = do.call(rbind, dfs),
               tidy = bind_rows(tbls),
               dt   = rbindlist(dts)
               )

benchmark(calls)

################################################################################
# Stack data.frames with different column names and orders
mtcar_sets <-
  list(
       mtcars1 = mtcars[c("mpg", "wt", "hp")],
       mtcars2 = mtcars[c("wt", "disp")],
       mtcars3 = mtcars[c("hp", "disp", "mpg")]
)

with(mtcar_sets, tryCatch(rbind(mtcars1, mtcars2), error = function(e) {e}))
with(mtcar_sets, tryCatch(rbind(mtcars1, mtcars3), error = function(e) {e}))

# How to do this in base R????  I don't know of a easy tool in base R.  All
# alternatives are in other packages such as gtools::smartbind

calls <- alist(
  tidy = bind_rows(mtcar_sets, .id = "set"),
  dt   = data.table::rbindlist(mtcar_sets, use.names = TRUE, fill = TRUE, idcol = "set")
  )

lapply(calls, eval)

benchmark(calls)

# with a lot of data
calls <- alist(
  tidy = bind_rows(list(recs_2009_tbl, recs_2015_tbl, recs_2020_tbl), .id = "set"),
  dt   = data.table::rbindlist(list(recs_2009_dt, recs_2015_dt, recs_2020_dt), use.names = TRUE, fill = TRUE, idcol = "set")
  )

benchmark(calls)

