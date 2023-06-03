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
      
      print(y)
      
      result <- list()
      result$url <- y
      
      # check if site is on
      result$html_status <- httr::HEAD(y)$status_code
      
      if (result$html_status == 200) {
        html <- read_html(y)
        
        result$fulltext <- html %>%
          html_element(xpath = '/html/body/div[2]/div/section/div/div/div/div/div/div[4]/div') %>%
          html_text()
        
        result$date <- html %>% 
          html_element(xpath = "/html/body/div[2]/div/section/div/div/div/div/div/div[3]/div/ul/li/span") %>% 
          html_text2()
        
        result$title <- html %>% 
          html_element(xpath = "/html/body/div[2]/div/section/div/div/div/div/div/div[1]/div/span") %>% 
          html_text()
      
      } else {
        result$fulltext <- NA
        result$date <- NA
        result$title <- NA
      }
      
      Sys.sleep(wait) 
      
      return(result)
      
    })
    
    return(x)
}


mm_1_to_50 <- get_mm(index_from = 1, index_to = 50, wait = 1)
mm_50_to_100 <- get_mm(index_from = 50, index_to = 100, wait = 0.5)
mm_101_to152 <- get_mm(index_from = 101, index_to = 152, wait = 0.5)

lapply(mm_101_to152, function(x){
  x %>% 
    tibble() %>% 
    unnest_wider(col = 1, names_sep = "_")
}) %>% 
  bind_rows()

mm_1_to_50 %>% 
  tibble() %>% 
  unnest_wider(col = 1)

mm_50_to_100 %>% 
  tibble() %>% 
  unnest_wider(col = 1)

mm_101_to152 %>% 
  tibble() %>% 
  unnest_wider(col = 1)

