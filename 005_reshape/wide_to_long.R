source('utilities.R')

recs_2009_df  <- read.csv(recs_2009_path)
recs_2009_tbl <- read_csv(recs_2009_path)
recs_2009_dt  <- fread(recs_2009_path)

# "Variable Name" , "Variable Label"                       
# "DOEID"         , "Unique identifier for each respondent"
# "REGIONC"       , "Census Region"                        
# "NWEIGHT"       , "Final sample weight"                  
# "HDD65"         , "Heating degree days in 2009, base temperature 65F"
# "CDD65"         , "Cooling degree days in 2009, base temperature 65F"
# "HDD30YR"       , "Heating degree days, 30-year average 1981-2010, base 65F"
# "CDD30YR"       , "Cooling degree days, 30-year average 1981-2010, base 65F"

# base R, stats::reshape
reshape(data = recs_2009_df[1:10, c("DOEID", "REGIONC", "NWEIGHT", "HDD65", "HDD30YR", "CDD65", "CDD30YR")]
        , direction = "long"
        , idvar = c("DOEID", "REGIONC", "NWEIGHT")
        , ids = recs_2020_dt[c("DOEID", "REGIONC", "NWEIGHT")]
        , times = c("HDD65", "HDD30YR", "CDD65", "CDD30YR")
        , timevar = "variable"
        , varying = list(c("HDD65", "HDD30YR", "CDD65", "CDD30YR"))
) |>
subset(x = _, DOEID == 1)

# tidyverse
recs_2009_tbl %>%
  dplyr::select(.data$DOEID, .data$REGIONC, .data$NWEIGHT,
                .data$HDD65, .data$HDD30YR, .data$CDD65, .data$CDD30YR) %>%
  tidyr::pivot_longer(cols = c("HDD65", "HDD30YR", "CDD65", "CDD30YR")) %>%
  dplyr::filter(DOEID == "00001")

# data.table
melt(recs_2009_dt
     , id.vars = c("DOEID", "REGIONC", "NWEIGHT")
     , measure.vars = c("HDD65", "HDD30YR", "CDD65", "CDD30YR"))[DOEID == 1]

################################################################################
calls <- alist(
  base = {
    reshape(data = recs_2009_df[1:10, c("DOEID", "REGIONC", "NWEIGHT", "HDD65", "HDD30YR", "CDD65", "CDD30YR")]
            , direction = "long"
            , idvar = c("DOEID", "REGIONC", "NWEIGHT")
            , ids = recs_2020_dt[c("DOEID", "REGIONC", "NWEIGHT")]
            , times = c("HDD65", "HDD30YR", "CDD65", "CDD30YR")
            , timevar = "variable"
            , varying = list(c("HDD65", "HDD30YR", "CDD65", "CDD30YR"))
    )
  }
  ,
  tidyverse = {
    recs_2009_tbl %>%
      dplyr::select(.data$DOEID, .data$REGIONC, .data$NWEIGHT,
                    .data$HDD65, .data$HDD30YR, .data$CDD65, .data$CDD30YR) %>%
      tidyr::pivot_longer(cols = c("HDD65", "HDD30YR", "CDD65", "CDD30YR"))
  }
  ,
  dt1 = {
    melt(recs_2009_dt
         , id.vars = c("DOEID", "REGIONC", "NWEIGHT")
         , measure.vars = c("HDD65", "HDD30YR", "CDD65", "CDD30YR"))
  }
  ,
  dt2 = {
    melt(recs_2009_dt[, c("DOEID", "REGIONC", "NWEIGHT", "HDD65", "HDD30YR", "CDD65", "CDD30YR")]
         , id.vars = c("DOEID", "REGIONC", "NWEIGHT")
         , measure.vars = c("HDD65", "HDD30YR", "CDD65", "CDD30YR"))
  }
)

benchmark(calls)



