## code to prepare `mydataset` dataset goes here

# Data from here: https://www.rba.gov.au/about-rba/history/governors/
library(dplyr)
library(lubridate)
library(magrittr)



usethis::use_data(governor_tenure, overwrite = TRUE)
