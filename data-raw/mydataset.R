## code to prepare `mydataset` dataset goes here

# Data from here: https://www.rba.gov.au/about-rba/history/governors/
library(readr)
library(dplyr)
library(lubridate)
library(magrittr)

governor_tenure <- read_csv("data-raw/governor_tenure.csv") %>%
  mutate(start_date = start_date %>% as_date(format = "%d/%m/%Y"),
         end_date = end_date %>% as_date(format = "%d/%m/%Y")
         )

usethis::use_data(governor_tenure, overwrite = TRUE)
