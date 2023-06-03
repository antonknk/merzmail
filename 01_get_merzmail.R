# SCRIPT: 01_get_mm.R -------------------------------------------------------
#
# Author:   Anton Könneke 
#
# Save #MerzMail
#
# CREATED: 2023-06-03
#
# SETUP ------------------------------------------------------------------------
pacman::p_load(rvest, tidyverse)

get_mm <- function(baseurl = "https://www.friedrich-merz.de/merzmail/merzmail-", 
                   index_from, 
                   index_to,
                   wait = 3) {
  entries <- seq(index_from, index_to)
    urls <- sapply(entries, function(entries) {
      paste0(baseurl, entries, "/")
    })
    
    x <- lapply(urls, function(y) {
      
      y %>%
        read_html() %>%
        html_element(xpath = '/html/body/div[2]/div/section/div/div/div/div/div/div[4]/div') %>%
        html_text()
      
      Sys.sleep(wait)
    })
    
    return(x)
}

mm_1_to_152 <- get_mm(index_from = 1, index_to = 152)

