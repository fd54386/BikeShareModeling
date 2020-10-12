Readme
================

# Overview

This project is for the analysis of the [UCI Bike Sharind
Dataset](https://archive.ics.uci.edu/ml/datasets/Bike+Sharing+Dataset),
with a focus on generating separate output files in R Markdown. In
particular, we are trying to separately model the daily rider count
`cnt` for each day of the week.

This file contains the script to generate multiple reports. The report
script itself is located [here](Daily%20Modeling%20Report.Rmd)

# Reports

Reports can be found at the links below. Format is structured
identically across reports, with data and statistics corresponding to
the day itself.

[Sunday](Sunday.md) [Monday](Monday.md) [Tuesday](Tuesday.md)
[Wednesday](Wednesday.md) [Thursday](Thursday.md) [Friday](Friday.md)
[Saturday](Saturday.md)

## Required Packages

There are two scripts for this project. The first, this readme, requires
the `tidyverse`, `knitr`, and `rmarkdown` packages. The second, the
report generator, also requires the `caret` package to run the modeling.

``` r
library(tidyverse)
library(knitr)
library(rmarkdown)

options(dplyr.print_min = 5)
options(tibble.print_min = 5)
opts_chunk$set(message = FALSE, cache = TRUE)
```

## Rendering the 7 Reports

The below code generates parameters to pass into the render function so
that we can name our file and evaluate for the desired day. The `DOW`
parameter is used in the Daily Modeling Report.Rmd to determine which
day is being modeled.

This code is run [separately](Render%20Script.R), outside of this
markdown file, but with the above packages. There is interference
between the setup call in this file and that of the Daily Modeling
Report that otherwise prevents the extra outputs.

``` r
#Create a vector of filenames for outputs
filenames <- c('Sunday.md','Monday.md','Tuesday.md', 
              'Wednesday.md', 'Thursday.md', 'Friday.md', 'Saturday.md')

#For parametric script evaluation, We need individual lists
#Need a labelled DOW field scaling from 0 (Sunday) to 6 (Saturday)
params = lapply(0:6, FUN = function(x){list(DOW = x)})

#Create a tibble so we can iterate along both filenames and parametric values together.
dfDailyReport<- tibble(filenames,params)

#Generate the reports
apply(dfDailyReport, MARGIN=1, FUN = function(x){
  render(input = 'Daily Modeling Report.Rmd', output_file = x[[1]], params = x[[2]])
})
```
