#' Read all the text from RBA minutes
#' @name get_fsr
#' @import rvest
#' @import stringr
#' @import dplyr
#' @import xml2
#' @import tibble
#' @import purrr
#' @import lubridate
#' @import pdftools
#' @import tm
#' @import tidytext
#' @export
get_fsr <- function() {

  ### Creating URLs ---------------------------------------

  fsr_mainpage <- "https://www.rba.gov.au/publications/fsr/"

  # Reading the html
  html <- xml2::read_html(fsr_mainpage) %>%
    rvest::html_nodes("a") %>%
    rvest::html_attr("href") %>%
    dplyr::as_tibble()

  # FSR page stubs
  fsr_page_stubs <- html %>%
    dplyr::filter(str_detect(value, "/publications/fsr/"),
           !str_detect(value, "box|article"),
           nchar(value) > 23) %>%
    unique()

  # Creating the links
  fsr_page_links <- stringr::str_c("https://www.rba.gov.au", fsr_page_stubs$value)

  fsr_link_fn <- function(url) {

    # Reading the html
    html <- xml2::read_html(url) %>%
      rvest::html_nodes("a") %>%
      rvest::html_attr("href") %>%
      as_tibble()

    html <- html %>%
      dplyr::filter(str_detect(value,"pdf"))   # PDFs are our only friend

    page_links <- stringr::str_c("https://rba.gov.au",html$value) %>%
      unique()

    # The FSR is always going to be the first link
    fsr_link <- tibble(url = page_links[1])

    return(fsr_link)
  }

  # Applying the function to get the links
  url_list <- lapply(fsr_page_links, fsr_link_fn) %>% unlist() %>%  as_tibble()

# Scraping the FSR text

  scrape_fn <- function(url) {

    pdf <- pdftools::pdf_text(url)

    # Clean text function
    pdf_text <- pdf %>%
      tm::removePunctuation() %>%
      tolower() %>%
      tm::stripWhitespace()

    pdf_df <- pdf_text %>% dplyr::as_tibble() %>%
      tidytext::unnest_tokens(word, value)

    # Getting the date of the FSR
    date <- url %>%
      stringr::str_remove("https://rba.gov.au/publications/fsr/") %>%
      stringr::str_sub(start = 1, end = 8) %>%
      paste0("/1") %>%
      lubridate::as_date(format = "%Y/%b/%d")


    ret <- pdf_df %>% dplyr::mutate(date = date)
    return(ret)

  }

  ret <- purrr::map_dfr(url_list$value, scrape_fn)
  return(ret)


}

