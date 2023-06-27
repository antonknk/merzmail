# get linke bundestagsfraktions press releases
pacman::p_load(rvest, tidyverse, RSelenium)

get_left_urls <- function(pages){
  length <- length(pages)
  counter <- 0
  
  lapply(pages, function(page){
    counter <<- counter + 1
    print(paste0("Scrape Page ", counter, " of ", length, sep = " "))
    url <- paste0("https://www.linksfraktion.de/suche/suchergebnisse/?tx_solr[filter][0]=artikeltyp%3APressemitteilung&tx_solr[page]=", page)
    html <- read_html(url)
    
    # get the data
    pm_urls <- html %>% 
      html_elements("div.result-list-item > .result-title > a") %>% 
      html_attr("href")
    
    pm_urls <- tibble(page_number = page,
           url = pm_urls)
    
    if ((as.numeric(page) %% 9) == 0){
      Sys.sleep(3)
    }
    
    return(pm_urls)
    
  }) %>% 
    bind_rows(.id = "page_number")
}

left_urls <- get_left_urls(pages = c("0", c(1:200)))

left_urls <- left_urls %>% 
  mutate(url = paste0("https://www.linksfraktion.de", url))
write_csv(left_urls, "left_urls.csv")

get_left_articles <- function(urls) {
  length <- length(urls)
  counter <- 0
  
  lapply(urls, function(url) {
    print(url)
    html <- read_html(url)
    counter <<- counter + 1
    print(paste0("Scrape Article ", counter, " of ", length, sep = " "))

    article <- list()
    
    #title
    article$title <- html %>%
      html_element(".white > h1") %>%
      html_text2()
    
    # date
    article$date <- html %>%
      html_element(".news-list-date > time") %>%
      html_text2()
    
    # author
    article$author <- html %>%
      html_element(".add-info > span:nth-child(1) > a") %>%
      html_text2()
    
    # type
    article$type <- html %>%
      html_element(".add-info > span:not(a)") %>%
      html_text2() %>%
      str_extract(regex("(?<=article type category )\\w*\\b"))
    
    # fulltext
    article$fulltext <- html %>%
      html_element(".article") %>%
      html_elements("p") %>%
      html_text2() %>%
      str_flatten() %>%
      str_squish()
    
    article$url <- url
    
    
    # cache
    if (!file.exists("texts/cache/leftbt_article_cache.csv")) {
      write.table(
        article,
        file = "texts/cache/leftbt_article_cache.csv",
        row.names = F,
        sep = ",",
        append = F
      )
    } else {
      write.table(
        article,
        file = "texts/cache/leftbt_article_cache.csv",
        row.names = F,
        col.names = F,
        sep = ",",
        append = T
      )
    }
    
    
    # after scraping four pages, wait
    if (counter %% 15 == 0) {
      Sys.sleep(2)
    }
    
    return(article)
  }) %>%
    bind_rows()
}

t <- get_left_articles(urls = sample(left_urls$url, 4, replace = F))
