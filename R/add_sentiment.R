#' @name add_sentiment
#' @param keep_all Parameter to keep all words or only those marked as sentiment
#' @param text Text to analyse
#' @param sentiment_data Sentiment data to use. Either sentiment_data_correa or sentiment_data_lm
#' @import stringr
#' @import dplyr
#' @export
add_sentiment <- function(text,
                          sentiment_data) {

  # Following Correa et al and looking for negation words within 3 words
  within_3_words <- stringr::str_c(
    dplyr::lag(text$word,3), " ", dplyr::lag(text$word,2), " ",
    dplyr::lag(text$word,1), " ", dplyr::lead(text$word,1), " ",
    dplyr::lead(text$word,2), " ", dplyr::lead(text$word,3)
  )

  negation_set <-"not|no|nobody|none|never|neither|cannot|can't|don't"

  negation_flag <- ifelse(stringr::str_detect(within_3_words, negation_set),
                          "negation", "no negation")

  # Sentiment analysis
  ret <- text %>%
    dplyr::mutate(negation_flag = negation_flag) %>%
    dplyr::left_join(sentiment_data) %>%
    dplyr::mutate(
      negation_sentiment = stringr::str_c(negation_flag," ", sentiment),
      sentiment = dplyr::recode(negation_sentiment,
                                `no negation positive` = "positive",
                                `negation positive` = "negative",
                                `no negation negative` = "negative",
                                `negation negative` = "neutral")
    )

return(ret)

}

