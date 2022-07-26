# Data Manipulation
A comparison of [Base R](https://www.r-project.org/),
[Tidyverse](https://www.tidyverse.org),
[data.table](https://rdatatable.gitlab.io/data.table/).

Each directory contains examples for common data manipulation tasks using
different dialects.

NOTE: all the example scripts are expected to be evaluated from the project
root directory.

## R Dependencies

You will need the following packages to reproduce the examples here:
* tidyverse
* data.table
* microbenchmark
* profmem

Check for, and install if needed, the packages via the following:

```r
pkgs <- c("tidyverse", "data.table", "microbenchmark", "profmem")
for (p in pkgs[!(pkgs %in% installed.packages())]) {
  install.packages(p)
}
```

## Data Sets

Data sets used in the examples are provided in the `000_data_sets` directory.

Please refer to the noted sources for additional detail on the data sets.

| File                                             | Rows   | Columns | 
| :----------------------------------------------- | -----: | ------: | 
| ./000_data_sets/RECS/2009/recs2009_public.csv    | 12,083 | 940     | 
| ./000_data_sets/RECS/2015/recs2015_public_v4.csv |  5,686 | 759     | 
| ./000_data_sets/RECS/2020/recs2020_public_v1.csv | 18,496 | 601     | 

* Residential Energy Consumption Survey (RECS)
    * Data from the 2009, 2015, and 2020 surveys
    * Downloaded from https://www.eia.gov/consumption/residential/data/2020/
        (last accessed 8 July 2022)

* Physician/Supplier Procedure Summary (PSPS)
    * The Physician/Supplier Procedure Summary (PSPS) LDS file is a summary of
      calendar year Medicare Part B carrier and durable medical equipment
      fee-for-service claims. The file is organized by carrier, pricing
      locality, Healthcare Common Procedure Coding System (HCPCS) code, HCPCS
      modifier, provider specialty, type of service, and place of service. The
      summarized fields are total submitted services and charges, total allowed
      services and charges, total denied services and charges, and total payment
      amounts.
    * Counts under 11 are censored from this data
    * Download from https://data.cms.gov/summary-statistics-on-use-and-payments/physiciansupplier-procedure-summary
      (last accessed 8 July 2022)
    * Additional Details: https://www.cms.gov/research-statistics-data-systems/limited-data-set-lds-files/physiciansupplier-procedure-summary-psps-limited-data-set-lds

    * The size of this data is large, about 9GB when fully decompressed.  To
        store that much data in a "reasonable" way on github the download csv
        files were split into smaller chunks and then compressed with gzip.


