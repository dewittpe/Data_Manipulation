#'---
#'title: "Data Manipulation: Comparing Tidyverse, data.table, and Base R"
#'subtitle: "Denver R User Group"
#'author: "Peter E. DeWitt, Ph.D.<br><peter.dewitt@cuanschutz.edu>"
#'date: 27 July 2022
#'output:
#'  ioslides_presentation:
#'    keep_md: false
#'    widescreen: true
#'    logo: drug-logo.jpeg
#'    css: style.css
#'    self_contained: true
#'---
#'
#+ label = "setup", include = FALSE
knitr::opts_chunk$set(collapse = TRUE)

#'
#' # We're Back
#'
#' ## Meetups
#'
#' * Location(s)
#'   - Galvanized not an option anymore
#'   - Free-use vs Fee-for-use
#'   - Do you have suggestions for locations?
#'
#' * Monthly-ish meetups
#'   - Would like to have about 10 meetups a year
#'
#' * Format
#'   - Time to socialize and
#'     - One big talk, or
#'     - Two shorter talks, or
#'     - Lighting talks
#'     - Other formats
#'
#' ## New in R
#'
#' * Highlights of new features/tools in R
#'
#'   - March 2020 (Last Meetup) version 3.6.3 (2020-02-29)
#'   - April 2020 4.0.0
#'   - March 2021 4.0.5
#'   - May 2021   4.1.0
#'   - April 2022 4.2.0
#'   - Current R version 4.2.1 (2022-06-23)
#'
#'   - NEWS:  https://cran.r-project.org/doc/manuals/r-release/NEWS.html
#'
#' ## Highlights of new features/tools in R 4.0.x
#'
#' * Significant User-Visible Changes
#'   - matrix objects now also inherit from class "array", so e.g., class(diag(1)) is c("matrix", "array"). This invalidates code incorrectly assuming that class(matrix_obj)) has length one.
#'   - R now uses a `stringsAsFactors = FALSE` default
#'   - The plot() S3 generic function is now in package base rather than package graphics
#'
#' * Other Notable Changes
#'   - Functions rbinom(), rgeom(), rhyper(), rpois(), rnbinom(), rsignrank() and rwilcox() which have returned integer since R 3.0.0 and hence NA when the numbers would have been outside the integer range, now return double vectors (without NAs, typically) in these cases. 
#'   - Migration to PCRE2
#'
#' ## Highlights of R 4.1.x
#'
#'   - 4.1.x
#'     - Last series to support 32-bit Windows
#'     - A lot of compiled code level changes
#'
#'   - 4.1.0
#'     - Introduces the base R version of a pipe `|>`
#'     - example on next slide
#'
#' ##
#+ label = "pipe_example"
mtcars |> str()

#'
#' ## Highlights of R 4.2.x
#'   - 4.2.0
#'     - Calling && or || with either argument of length greater than one now gives a warning (which it is intended will become an error).
#'     - Calling if() or while() with a condition of length greater than one gives an error rather than a warning. Consequently, environment variable _R_CHECK_LENGTH_1_CONDITION_ no longer has any effect.
#'     - In a forward pipe |> expression it is now possible to use a named argument with the placeholder _ in the rhs call to specify where the lhs is to be inserted. The placeholder can only appear once on the rhs. 
#'       - example on next slide
#'     - _and a lot more_
#'
#' ##
#+ label = 'pipe_example_with_placeholder'
mtcars |>
  lm(mpg ~ wt, data = _) |>
  summary()

#'
# /*
2 + 2 # line to help my text editor indent correctly after the piping
# */
#'
#' # Data Manipulation
#'
#' ## Three Dialects
#'
#' |   | Pros | Cons |
#' |---|:---- |:---- |
#' |Base R| Long term stability | slow;<br> memory inefficient (compared to alternatives) |
#' |Tidyverse | popular<br>Intuitive (for some) | popular<br>API changes<br>_lots_ of packages |
#' |data.table | One package<br>Assignment by reference | Compiling on Mac is non-trivial <br> Assignment by reference |
#'
#' * Best tool?
#'   - The one that gets the job done
#'   - I prefer....
#'
#' ## Comparing Dialects
#'
#' * Assessments:
#'   - Syntax
#'   - Timing benchmarking via [microbenchmark](https://CRAN.R-project.org/package=microbenchmark)
#'   - Memory use via [profmem](https://cran.r-project.org/package=profmem)
#'
#' * Data sets and examples on github
#'   - My github: https://github.com/dewittpe/Data_Manipulation
#'   - Denver R User Group: https://github.com/DenverRUG/2022-07-27-Data-Manipulation
#'
#' ## Data Import: Reading in a lot of columns
#' <div class="box">
#' <iframe src="001_data_import/many_columns.Rout.html" style="height:500px;"></iframe>
#' </div>
#'
#' ## Data Import: Reading in a lot of rows
#' <div class="box">
#' <iframe src="001_data_import/many_rows.Rout.html" style="height:500px;"></iframe>
#' </div>
#'
#' ## Data Import: Thoughts
#'
#' - Use the same import tools as the rest of your work
#' - Consider defaults and regions across the world
args(read.csv)        # you have control over sep = ","
args(readr::read_csv) # comma separated.  use read_csv2 for ";" sep
args(data.table::fread)

#'
#' ## Conditional Manipulation: ifelse
#' <div class="box">
#' <iframe src="002_conditional_manipulations/ifelse.Rout.html" style="height:500px;"></iframe>
#' </div>
#'
#' ## Conditional Manipulation: case_when
#' <div class="box">
#' <iframe src="002_conditional_manipulations/case_when.Rout.html" style="height:500px;"></iframe>
#' </div>
#'
#' ## Conditional Manipulation: create multiple columns
#' <div class="box">
#' <iframe src="002_conditional_manipulations/create_multiple_columns.Rout.html" style="height:500px;"></iframe>
#' </div>
#'
#' ## Joins: Order _can_ matter
#' <div class="box">
#' <iframe src="003_joins/order_matters.Rout.html" style="height:500px;"></iframe>
#' </div>
#'
#' ## Joins: Standard Joins
#' <div class="box">
#' <iframe src="003_joins/standard_joins.Rout.html" style="height:500px;"></iframe>
#' </div>
#'
#' ## Joins: 
#'
# /* end of file */
