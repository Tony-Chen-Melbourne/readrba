#' Read all the text from RBA minutes
#' @name get_minutes
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
get_minutes <- function() {

  ### Creating URLs ---------------------------------------

  link_fn <- function(year) {
    # Creating the link
    url <- stringr::str_c("https://www.rba.gov.au/monetary-policy/rba-board-minutes/",
                 year, "/")

    # Reading the html
    html <- xml2::read_html(url) %>%
      rvest::html_nodes("a") %>%
      rvest::html_attr("href") %>%
      dplyr::as_tibble()

    # Creating the individual link for statements
    stub <- stringr::str_c("/monetary-policy/rba-board-minutes/",
                           year, "/")

    html <- html %>%
      dplyr::filter(stringr::str_detect(value, stub)) %>%
      dplyr::filter(value != stub) %>% # Cutting out the stub choice for the year
      dplyr::filter(!stringr::str_detect(value,"pdf")) # PDFs are not our friend here

    ret <- tibble::tibble(url = str_c("https://rba.gov.au",html$value))

    return(ret)
  }

  # Applying the function to get the links
  today <- Sys.Date() %>% lubridate::year()

  years <- seq(2006, today, by =1) %>% as.character()
  url_list <- lapply(years, link_fn) %>% unlist() %>% as_tibble()

  # Writing a function to purely scrape the url_list
  scrape_fn <- function(url) {

    html <- xml2::read_html(url)

    speech_text <- html %>%
      rvest::html_nodes("p") %>%
      rvest::html_text()

    # Get the date and location
    date <- speech_text[2] %>% stringr::str_split(" ", simplify = TRUE)
    location <- date[,1] %>% tm::stripWhitespace()
    date <- paste(date[,3], date[,4], date[,5])
    date <- date %>% lubridate::as_date(format = "%d %B %Y")

    # Clean text function
    speech_text <- speech_text %>%
      tm::removePunctuation() %>%
      tolower() %>%
      tm::stripWhitespace()

    speech_df <- speech_text %>% dplyr::as_tibble()

    # Dropping the first row, which is html nonsense
    speech_df <- speech_df[-1,]

    # Un-nesting the tokens into individual words
    speech_df <- speech_df %>%
      tidytext::unnest_tokens(word, value)

    # Adding in the date and location
    speech_df <- speech_df %>%
      dplyr::mutate(location = location,
             date = date)

    return(speech_df)

  }

  ret <- purrr::map_dfr(url_list$value, scrape_fn)

  return(ret)

}

