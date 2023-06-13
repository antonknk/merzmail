# SCRIPT: 02_get_cdu.R -------------------------------------------------------
#
# Author:   Anton Könneke 
#
# Scrape CDU press releases
#
# CREATED: 2023-06-13
#
# SETUP ------------------------------------------------------------------------
pacman::p_load(rvest, tidyverse, RSelenium)

# Start a Selenium firefox browser
driver <- rsDriver(browser = "firefox",
                   port = 4555L,
                   verbose = FALSE,
                   chromever = NULL)

# extract the client for readability of the code to follow
remote_driver <- driver[["client"]]

# Set URL
url <- "https://www.cducsu.de/presse/pressemitteilungen#presse"

# Navigate to the webpage
remote_driver$navigate(url)

# accept cookies
cookie_button <- remote_driver$findElement(using = "css selector", ".eu-cookie-compliance-accept-all-button")
cookie_button$clickElement()

# Load More
load_more <- function(rd) {
  # scroll to end of page
  rd$executeScript("window.scrollTo(0, document.body.scrollHeight);", args = list())
  # Find the "Load more" button by its CSS selector and ...
  load_more_button <- rd$findElement(using = "css selector", ".cducsu-views-collection__tab--active > div:nth-child(2) > a:nth-child(2)")
  # ... click it
  load_more_button$clickElement()
  # give the website a moment to respond
  Sys.sleep(2)
}

load_page_completely <- function(rd) {
  # load more content even if it throws an error
  tryCatch({
    # call load_more()
    load_more(rd)
    # if no error is thrown, call the load_page_completely() function again
    Recall(rd)
  }, error = function(e) {
    # if an error is thrown return nothing / NULL
  })
}

load_page_completely(remote_driver)

# html speichern
page_source <- remote_driver$getPageSource()
webpage <- read_html(page_source[[1]])

cdu_press <- tibble(
  url = webpage %>% 
  html_elements(".game") %>% 
  html_attr("href"),
  date = webpage %>% 
    html_elements(".game") %>% 
    html_elements(".meta__date") %>% 
    html_text(),
  author =   webpage %>% 
    html_elements(".game") %>% 
    html_elements("div:nth-child(2) > div:nth-child(1)") %>% 
    html_text2() %>% 
    str_remove_all("\\s\\/\\/.*")
)

cdu_press %>% 
  mutate(date = lubridate::dmy(date)) %>% 
  ggplot(aes(date))+
  geom_density()

# scrape it
get_cdu_articles <- function(urls) {
  articles <- lapply(urls, function(url){
    html <- read_html(url)
    article <- list()
    
    article$url <- url
    
    article$headline <- html %>% 
      html_elements("h1") %>% 
      html_text()
    
    article$subtitle <- html %>% 
      html_elements("body > div.dialog-off-canvas-main-canvas > div > div.sheet__content > div > article > header > div > div.head__right > div:nth-child(2) > div > p:nth-child(2)") %>% 
      html_text()
    
    article$primer <- html %>% 
      html_elements("body > div.dialog-off-canvas-main-canvas > div > div.sheet__content > div > article > header > div > div.head__right > div:nth-child(2) > div > p:nth-child(3)") %>% 
      html_text()

    article$text <- html %>%
      html_elements("body > div.dialog-off-canvas-main-canvas > div > div.sheet__content > div > article > div.mantle.mantle--text-limited.mantle--wrap.mantle--mobilespacing > div > div") %>% 
      html_text()
    
    Sys.sleep(2+runif(1))
    return(article)
    

  })
  return(articles)
  
}

# fix urls
cdu_press <- cdu_press %>% 
  mutate(url = paste0("https://www.cducsu.de", url))

cdu_full <- get_cdu_articles(cdu_press$url)
