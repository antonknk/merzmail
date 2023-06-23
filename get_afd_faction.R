# SCRIPT: 04_get_afd_faction.R -------------------------------------------------------
#
# Author:   Anton Könneke
#
# Scrape SPD Bundestag Faction Press Releases
#
# CREATED: 2023-06-23
#
# SETUP ------------------------------------------------------------------------
pacman::p_load(rvest, tidyverse)

# GET URLS ---------------------------------------------------------------------
afdbt_get_urls <- function(pages) {
  counter <- 0
  length <- length(pages)
  lapply(pages, function(page) {
    counter <<- counter + 1
    
    # create url
    url <- paste0("https://afdbundestag.de/presse/page/", page, "/")
    
    # print progress
    print(paste0("Scrape Article ", counter, " of ", length, sep = " "))
    
    # get html
    html <- rvest::read_html(url)
    
    # extract urls
    page_urls <- html %>%
      rvest::html_elements("article") %>%
      rvest::html_elements("h2") %>%
      rvest::html_element("a") %>%
      rvest::html_attr("href")
    
    result <- dplyr::tibble(page_number = page,
                            urls = page_urls)
    
    # cache
    if (!file.exists("~/Documents/Projekte/Aktuell/merzmail/texts/cache/afd_url_cache.csv")) {
      write.table(
        result,
        file = "~/Documents/Projekte/Aktuell/merzmail/texts/cache/afd_url_cache.csv",
        row.names = F,
        sep = ",",
        append = F
      )
    } else {
      write.table(
        result,
        file = "~/Documents/Projekte/Aktuell/merzmail/texts/cache/afd_url_cache.csv",
        row.names = F,
        col.names = F,
        sep = ",",
        append = T
      )
    }
    
    # after scraping four pages, wait
    if (page %% 4 == 0) {
      Sys.sleep(2)
    }
    
    return(result)
    
  }) %>%
    dplyr::bind_rows()
}

# scrape articles
get_afdbt_articles <- function(urls) {
  length <- length(urls)
  counter <- 0
  lapply(urls, function(url) {
    print(url)
    counter <<- counter + 1
    print(paste0("Scrape Article ", counter, " of ", length, sep = " "))
    
    html <- read_html(url)
    article <- list()
    
    # check wether this is an empty article that only links a parliamentary request
    is_anfrage <- (
      html %>%
        html_element(".post-content") %>%
        html_element("a") %>%
        html_text() %>%
        str_detect(pattern = "Zur Anfrage und Antwort der Bundesregierung|Zum Antrag") &
        html %>%
        html_element(".post-content") %>%
        html_element("a") %>%
        html_attr("href") %>%
        str_detect(pattern = "\\.pdf$")
    )
    
    
    if (is_anfrage) {
      message(paste("Article no", counter, "has no body. Not scraped. This was the url:", url, sep = " "))
    } else {
      # title
      article$title <- html %>%
        html_element("h1") %>%
        html_text()
      
      # date
      article$date <- html %>%
        html_element(".post-content") %>%
        html_element("div.fusion-fullwidth.fullwidth-box.fusion-builder-row-3.fusion-flex-container.has-pattern-background.has-mask-background.nonhundred-percent-fullwidth.non-hundred-percent-height-scrolling > div > div > div > div.fusion-content-tb.fusion-content-tb-1 > p:nth-child(1) > b") %>% 
        html_text2()
      
      # prompt
      article$prompt <- html %>%
        html_element(".post-content") %>%
        html_element("div.fusion-fullwidth.fullwidth-box.fusion-builder-row-3.fusion-flex-container.has-pattern-background.has-mask-background.nonhundred-percent-fullwidth.non-hundred-percent-height-scrolling > div > div > div > div.fusion-content-tb.fusion-content-tb-1 > p:nth-child(1) > strong") %>% 
        html_text2()
      
      # date backup
      article$date2 <- html %>%
        html_element(".post-content") %>%
        html_element("div.fusion-fullwidth.fullwidth-box.fusion-builder-row-3.fusion-flex-container.has-pattern-background.has-mask-background.nonhundred-percent-fullwidth.non-hundred-percent-height-scrolling > div > div > div > div.fusion-content-tb.fusion-content-tb-1 > p:nth-child(1) > strong") %>% 
        html_text2() %>%
        str_extract("^.*\\s\\d{4}")
      
      # fulltext
      article$fulltext <- html %>%
        html_element(".post-content") %>%
        html_element("div.fusion-fullwidth.fullwidth-box.fusion-builder-row-3.fusion-flex-container.has-pattern-background.has-mask-background.nonhundred-percent-fullwidth.non-hundred-percent-height-scrolling > div > div > div > div.fusion-content-tb.fusion-content-tb-1") %>%
        html_elements("p:not(:nth-child(1))") %>%
        html_text2() %>%
        str_flatten()
      
      article$url <- url
      
      # cache
      if (!file.exists("texts/cache/afd_article_cache.csv")) {
        write.table(
          article,
          file = "texts/cache/afd_article_cache.csv",
          row.names = F,
          sep = ",",
          append = F
        )
      } else {
        write.table(
          article,
          file = "texts/cache/afd_article_cache.csv",
          row.names = F,
          col.names = F,
          sep = ",",
          append = T
        )
      }
    }
    
    
    # after scraping four pages, wait
    if (counter %% 6 == 0) {
      Sys.sleep(2.2)
    }
    
    return(article)
    
  }) %>%
    bind_rows()
}


