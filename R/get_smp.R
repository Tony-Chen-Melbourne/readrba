#' Read all the text from RBA minutes
#' @name get_smp
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
get_smp <- function() {

  ### Creating URLs ---------------------------------------

  smp_mainpage <- "https://www.rba.gov.au/publications/smp/"

  # Reading the html
  html <- xml2::read_html(smp_mainpage) %>%
    rvest::html_nodes("a") %>%
    rvest::html_attr("href") %>%
    dplyr::as_tibble()

  # SMP page stubs
  smp_page_stubs <- html %>%
    dplyr::filter(str_detect(value, "/smp/\\d\\d\\d\\d/\\w"),
           !stringr::str_detect(value, "box")) %>%
    unique()

  # Creating the links
  smp_page_links <- paste0("https://www.rba.gov.au", smp_page_stubs$value)

  smp_link_fn <- function(url) {

    # Reading the html
    html <- xml2::read_html(url) %>%
      rvest::html_nodes("a") %>%
      rvest::html_attr("href") %>%
      dplyr::as_tibble() %>%
      dplyr::filter(str_detect(value,"pdf"))   # PDFs are our only friend

    page_links <- paste0("https://rba.gov.au",html$value) %>%
      unique()

    # The SMP is always going to be the first link

    smp_link <- dplyr::tibble(link = page_links[1])

    return(smp_link)
  }

  # Applying the function to get the links
  url_list <- map_dfr(smp_page_links, smp_link_fn)

  ### Scraping the text -----------------------------

  scrape_fn <- function(url) {

    pdf <- pdftools::pdf_text(url)

    # Clean text function
    pdf_text <- pdf %>%
      tm::removePunctuation() %>%
      tolower() %>%
      tm::stripWhitespace()

    pdf_df <- pdf_text %>% dplyr::as_tibble() %>%
      tidytext::unnest_tokens(word, value)

    # Getting the date of the SMP
    date <- url %>% stringr::str_sub(start = -11L, end = -5L) %>% paste0("-01")
    date <- date %>% lubridate::as_date(format = "%Y-%m-%d")

    ret <- pdf_df %>%
      mutate(date = date)

    return(ret)

  }

  ret <- map_dfr(url_list$link, scrape_fn)

  return(ret)

}

