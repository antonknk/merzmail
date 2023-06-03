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
      
      html <- read_html(y)
      
      result <- list()
      
      result$fulltext <- html %>%
        html_element(xpath = '/html/body/div[2]/div/section/div/div/div/div/div/div[4]/div') %>%
        html_text()
      
      result$date <- html %>% 
        html_element(xpath = "/html/body/div[2]/div/section/div/div/div/div/div/div[3]/div/ul/li/span") %>% 
        html_text2()
      
      result$title <- html %>% 
        html_element(xpath = "/html/body/div[2]/div/section/div/div/div/div/div/div[1]/div/span") %>% 
        html_text()
      
      Sys.sleep(wait) 
      
      return(result)
    })
    
    return(x)
}

mm_1_to_152 <- get_mm(index_from = 1, index_to = 2, wait = 1)

"https://www.friedrich-merz.de/merzmail/merzmail-1" %>%
  read_html() %>%
  html_element(xpath = "/html/body/div[2]/div/section/div/div/div/div/div/div[3]/div/ul/li/span") %>% 
  html_text2()

