#' @name add_sentiment
#' @param keep_all Parameter to keep all words or only those marked as sentiment
#' @param text Text to analyse
#' @param sentiment_data Sentiment data to use
#' @import rvest
#' @import stringr
#' @import dplyr
#' @import xml2
#' @import tibble
#' @import purrr
#' @import lubridate
#' @import tm
#' @import tidytext
#' @export
add_sentiment <- function(text) {

  # Following Correa et al and looking for negation words within 3 characters
  within_3_words <- str_c(lag(pdf_df$word,3), " ", lag(pdf_df$word,2), " ",
                          lag(pdf_df$word,1), " ", lead(pdf_df$word,1), " ",
                          lead(pdf_df$word,2), " ", lead(pdf_df$word,3)
  )

  negation_set <-"not|no|nobody|none|never|neither|cannot"

  negation_flag <- ifelse(str_detect(within_3_words, negation_set),
                          "negation", "no negation")

  # Sentiment analysis
  pdf_df <- pdf_df %>%
    mutate(negation_flag = negation_flag) %>%
    inner_join(sentiment_dataset) %>%
    mutate(negation_sentiment = str_c(negation_flag," ", sentiment),
           sentiment = dplyr::recode(negation_sentiment,
                                     `no negation positive` = "positive",
                                     `negation positive` = "negative",
                                     `no negation negative` = "negative",
                                     `negation negative` = "neutral")
    ) %>%
    select(word, sentiment) %>%
    filter(sentiment == "positive"|sentiment == "negative")


}

