library(tidyverse)
library(knitr)
library(rmarkdown)

options(dplyr.print_min = 5)
options(tibble.print_min = 5)
opts_chunk$set(message = FALSE, cache = TRUE)

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