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

spd_urls <- get_urls(pages = c("", c(1:162)))
spd_urls <- spd_urls %>% 
  mutate(urls = paste0("https://www.spdfraktion.de", urls))

# SCAPRE ARTICLES --------------------------------------------------------------
get_articles <- function(urls) {
  length <- length(urls)
  counter <- 0
  lapply(urls, function(url){
    print(url)
    counter <<- counter + 1
    print(paste0("Scrape Article ", counter, " of ", length, sep = " "))
    
    html <- read_html(url)
    article <- list()
    
    Sys.sleep(1)
    
    article$headline <- html %>% 
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
    
  }) %>% 
 bind_rows() 
}

spd_articles <- get_articles(urls = spd_urls$urls)

write_csv(spd_articles, "texts/spd_articles.csv")

# PRESS STATEMENTS -------------------------------------------------------------
get_press_urls <- function(pages){
  lapply(pages, function(page){
    url <- paste0("https://www.spdfraktion.de/presse/pressemitteilungen?page=", page)
    
    Sys.sleep(2)
    html <-  read_html(url)
    
    # extract urls
    page_urls <- html %>% 
      html_elements(".view-content") %>% 
      html_elements("h3") %>% 
      html_elements("a") %>% 
      html_attr("href")
    
    result <- tibble(page_number = page,
                     urls = page_urls)
    
    return(result)
  }) %>% 
    bind_rows()
}

press_urls <- get_press_urls(pages = c("", c(1:41)))

press_urls <- press_urls %>% 
  mutate(urls = paste0("https://www.spdfraktion.de", urls))

# function to get press releases
# SCAPRE ARTICLES --------------------------------------------------------------
get_articles <- function(urls) {
  length <- length(urls)
  counter <- 0
  lapply(urls, function(url){
    print(url)
    counter <<- counter + 1
    print(paste0("Scrape Article ", counter, " of ", length, sep = " "))
    
    html <- read_html(url)
    article <- list()
    
    Sys.sleep(.3)
    
    article$title <- html %>% 
      html_element("article") %>% 
      html_element("h1") %>% 
      html_text()
    
    # fulltext
    article$fulltext <- html %>% 
      html_element("article") %>% 
      html_element(".maintext") %>% 
      html_text2()
    
    # promt
    article$prompt <- html %>% 
      html_element("article") %>% 
      html_element(".maintext") %>% 
      html_element("p:nth-child(1)") %>% 
      html_text()
    
    # authors
    article$authors <- html %>% 
      html_element("article") %>% 
      html_element(".maintext") %>% 
      html_elements("em") %>% 
      html_text() %>% 
      list()
    
    article$meta <- html %>%
      html_element("article") %>%
      html_element("#main-image-aside") %>%
      html_element("dl") %>%
      html_elements("dd") %>%
      html_text() %>% 
      list()
    
    article$url <- url
    
    return(article)
    
  }) %>% 
    bind_rows() 
}

spd_press_releases <- get_articles(press_urls$urls)

spd_press_releases <- spd_press_releases %>% 
  unnest_wider(meta, names_sep = "_") %>% 
  unnest_wider(authors, names_sep = "_") 

write_csv(spd_press_releases, "texts/spd_press_releases.csv")

# add older PMs 
baseurl <- "https://www.spdfraktion.de/presse/pressemitteilungen?s=&s_date%5Bdate%5D=&e_date%5Bdate%5D=&field_legislaturen=2&sort_by=created&sort_order=&items_per_page=100&page="
spd_old_press_urls <- lapply(c(0:16), function(page){
  url <- paste0(baseurl, page)
  html <- read_html(url)
  
  # extract urls
  page_urls <- html %>% 
    html_elements("body") %>% 
    html_elements("article") %>% 
    html_elements("h3") %>% 
    html_elements("a") %>% 
    html_attr("href")
  
  Sys.sleep(2)
  
  result <- tibble(page_number = page,
                   urls = page_urls)
  return(result)
}) %>% 
  bind_rows(.id = "page")

# scrape the old press releases
spd_old_press_urls <- spd_old_press_urls %>% 
  mutate(urls = paste0("https://www.spdfraktion.de", urls))

spd_press_releases_old <- get_articles(spd_old_press_urls$urls)
