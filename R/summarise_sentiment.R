#' @name summarise_sentiment
#' @param text Text to analyse, the output of add_sentiment()
#' @import dplyr
#' @export
summarise_sentiment <- function(text) {

  ret <- text %>% dplyr::group_by(date) %>%
    dplyr::summarise(neg_count = sum(sentiment == "negative", na.rm = TRUE),
              pos_count = sum(sentiment == "positive", na.rm = TRUE),
              total_count = dplyr::n())
return(ret)

}

