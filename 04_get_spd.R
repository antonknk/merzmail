# SCRIPT: 04_get_spd.R -------------------------------------------------------
#
# Author:   Anton Könneke 
#
# Scrape SPD Bundestag Faction Press Releases
#
# CREATED: 2023-06-15
#
# SETUP ------------------------------------------------------------------------
pacman::p_load(rvest, tidyverse, tictac)

# For the SPD Bundestag-Faction, scraping the press releases is a little easier
# than for the other pages, as they use simple index numbers in the urls to the
# pages with the press releases. Page 162 holds the first press releases from 
# 2020 so I will scrape pages 1 to 162. Note that these are dynamic indexes in 
# that in the future, page 162 will hold more recent press releases as today.

# GET URLS ---------------------------------------------------------------------
get_urls <- function(pages){
  lapply(pages, function(page){
    # create url
    url <- paste0("https://www.spdfraktion.de/artikel?page=", page)
    
    # get html
    html <- read_html(url)
    
    # extract urls
    page_urls <- html %>% 
      html_elements("article") %>% 
      html_elements("h3") %>% 
      html_elements("a") %>% 
      html_attr("href")
    
    result <- tibble(page_number = page,
           urls = page_urls)
    
    return(result)
    Sys.sleep(2)
  }) %>% 
    bind_rows()
}

spd_urls <- get_urls(pages = c(1:162))
# forgot most current page
spd_urls2 <- get_urls(pages = "")
spd_urls2$page_number = 0
spd_urls2 <- spd_urls2 %>% 
  mutate(urls = paste0("https://www.spdfraktion.de", urls))
spd_urls_all <- bind_rows(spd_urls2, spd_urls)
# SCAPRE ARTICLES --------------------------------------------------------------
get_articles <- function(urls) {
  lapply(urls, function(url){
    html <- read_html(url)
    article <- list()
    
    article$author <- html %>% 
      html_element("article") %>% 
      html_element("h2") %>% 
      html_text()
    
    article$title <- html %>% 
      html_element("article") %>% 
      html_element("h1") %>% 
      html_text()
    
    article$subtitle <- html %>% 
      html_element("article") %>% 
      html_element(".text-intro") %>% 
      html_text()
    
    article$date <- html %>% 
      html_element("article") %>% 
      html_element("#main-image-aside") %>% 
      html_element("dl") %>% 
      html_element("dd") %>% 
      html_text()
    
    article$meta <- html %>%
      html_element("article") %>%
      html_element("#main-image-aside") %>%
      html_element("dl") %>%
      html_elements("dd") %>%
      html_text() %>% 
      list()
    
    article$text <- html %>% 
      html_element("article") %>% 
      html_element(".maintext") %>% 
      html_text2()
    
    article$url <- url
    
    return(article)
    Sys.sleep(1.3)
  }) %>% 
 bind_rows() 
}

# actually scrape it 
tic()
spd_articles <- get_articles(urls = spd_urls_all$urls)
toc()