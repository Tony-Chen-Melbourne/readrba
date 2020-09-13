## code to prepare package data files
library(readr)
library(dplyr)
library(tidyr)
library(readxl)
library(lubridate)
library(magrittr)

## RBA governor tenure
# Data from here: https://www.rba.gov.au/about-rba/history/governors/
governor_tenure <- read_csv("data-raw/governor_tenure.csv") %>%
  mutate(start_date = start_date %>% as_date(format = "%d/%m/%Y"),
         end_date = end_date %>% as_date(format = "%d/%m/%Y")
         )

usethis::use_data(governor_tenure, overwrite = TRUE)


## Loughran McDonald sentiment data
# Downloaded from here: https://sraf.nd.edu/textual-analysis/
positive_lm <- read_excel("LoughranMcDonald_SentimentWordLists_2018.xlsx",
                          sheet = "Positive",
                          range = "A1:A354",
                          col_names = FALSE) %>%
  rename(word = 1) %>%
  mutate(sentiment = "positive")

negative_lm <- read_excel("LoughranMcDonald_SentimentWordLists_2018.xlsx",
                          sheet = "Positive",
                          range = "A1:A2355",
                          col_names = FALSE) %>%
  rename(word = 1) %>%
  mutate(sentiment = "negative")

uncertain_lm <- read_excel("LoughranMcDonald_SentimentWordLists_2018.xlsx",
                          sheet = "Positive",
                          range = "A1:A297",
                          col_names = FALSE) %>%
  rename(word = 1) %>%
  mutate(sentiment = "uncertain")

# Joining data
sentiment_data_lm <- rbind(positive_lm, negative_lm, uncertain_lm) %>%
   mutate(word = word %>% tolower())

usethis::use_data(sentiment_data_lm, overwrite = TRUE)

## Correa et al sentiment data
# See: https://www.federalreserve.gov/econres/ifdp/files/ifdp1203.pdf
# This data is supposed to be used for FSR analysis
sentiment_data_correa <- read_excel("ifdp1203-appendix.xlsx",
                                    sheet = "1 FS Dictionary",
                                    range = "A1:C392") %>%
  pivot_longer(cols = c("Positive","Negative")) %>%
  drop_na() %>%
  select(-value) %>%
  rename(word = 1, sentiment = 2) %>%
  mutate(sentiment = sentiment %>% tolower())

usethis::use_data(sentiment_data_correa, overwrite = TRUE)

