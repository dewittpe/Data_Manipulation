# Data Manipulation
A comparison of [Base R](https://www.r-project.org/),
[Tidyverse](https://www.tidyverse.org),
[data.table](https://rdatatable.gitlab.io/data.table/),
and (maybe) python's pandas for data manipulation.

Each directory contains examples for common data manipulation tasks using
different dialects.

NOTE: all the example scripts are exptected to be evaluated from the project
root directory.

## R Dependencies

You will need the following packages to reproduce the examples here:
* tidyverse
* data.table
* microbenchmark
* profmem

Check for, and install if needed, the packages via the following:

```r
pkgs <- c("notapkg", "tidyverse", "data.table", "microbenchmark", "profmem")
for (p in pkgs[!(pkgs %in% installed.packages())]) {
  install.packages(p)
}
```

## Data Sets

Data sets used in the examples are provided in the `000_data_sets` directory.

Please refer to the noted sources for additional detail on the data sets.

* Residential Energy Consumption Survey (RECS)
    * Data from the 2009, 2015, and 2020 surveys
    * Downloaded from https://www.eia.gov/consumption/residential/data/2020/
        (last accessed 8 July 2022)

* PSPS_2020

# ./000_data_sets/recs2009_public.csv       12,083      940
# ./000_data_sets/recs2015_public_v4.csv     5,686      759
# ./000_data_sets/recs2020_public_v1.csv    18,496      601

