# Data Manipulation
A comparison of [Base R](https://www.r-project.org/),
[Tidyverse](https://www.tidyverse.org),
[data.table](https://rdatatable.gitlab.io/data.table/),
and (maybe) python's pandas for data manipulation.

Each directory contains examples for common data manipulation tasks using
different dialects.

NOTE: all the example scripts are exptected to be evaluated from the project
root directory.

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

