#' Read all the text from RBA minutes
#' @name get_speech
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
get_speech <- function() {

  main_stub <- "https://www.rba.gov.au/speeches/"

  today <- Sys.Date()
  years <- seq(1990, today %>% lubridate::year(), by = 1)

  main_url <- paste0(main_stub, years,"/")


  # Creating the links
link_fn <- function(url) {

    html <- xml2::read_html(url) %>%
      rvest::html_nodes("a") %>%
      rvest::html_attr("href") %>%
      dplyr::as_tibble() %>%
      dplyr::filter(stringr::str_detect(value,"html")) %>%   # HTMLs are our only friend
      dplyr::filter(stringr::str_detect(value,"speech")) %>%   # HTMLs are our only friend
      dplyr::filter(!stringr::str_detect(value,"list"))   # HTMLs are our only friend

    ret <- paste0("https://www.rba.gov.au/",html$value)
    return(ret)

  }


url_links <- lapply(main_url, link_fn) %>% unlist() %>% dplyr::as_tibble()

# Scraping the text

scrape_fn <- function(url) {

  html <- xml2::read_html(url)

  speech_text <- html %>%
    rvest::html_nodes("p") %>%
    rvest::html_text()

  # Get the speaker and position
  information <- speech_text[2] %>%
    stringr::str_split("\n", simplify = TRUE)

  speaker <- information[,2] %>% tm::stripWhitespace() %>% trimws()
  position <- information[,3] %>% tm::stripWhitespace() %>% trimws()

  # Get the date
  date <- speech_text[3] %>%
    stringr::str_split("\n", simplify = TRUE)

  date <- date[,4] %>% tm::stripWhitespace() %>% trimws() %>%
    lubridate::as_date(format = "%d %B %Y")

  # Clean text function
  speech_text <- speech_text %>%
    tm::removePunctuation() %>%
    tolower() %>%
    tm::stripWhitespace()

  speech_df <- speech_text %>% dplyr::as_tibble()

  # Dropping the first four rows, which are throat clearing
  speech_df <- speech_df[-(1:4),]

  # Un-nesting the tokens into individual words
  speech_df <- speech_df %>%
    tidytext::unnest_tokens(word, value)

  # Adding in the date and location
  speech_df <- speech_df %>%
    dplyr::mutate(speaker = speaker,
                  position = position,
                  date = date)

  return(speech_df)

}

ret <- purrr::map_dfr(url_list$value, scrape_fn)


return(ret)

}

