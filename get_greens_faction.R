# SCRIPT: get_greens_faction.R -------------------------------------------------------
#
# Author:   Anton Könneke
#
# Read press releases by the green party bundestag faction
#
# CREATED: 2023-06-23
#
# SETUP ------------------------------------------------------------------------
pacman::p_load(tidyRSS, tidyverse, rvest, RSelenium)
greens_rss <-
  tidyRSS::tidyfeed("https://www.gruene-bundestag.de/presse/rss.xml")

# SCRAPE INTERACTIVE GREENS PAGE --------------------------------------------------

# Instructions taken from Tim Tiefenbach Tutorial
# https://tim-tiefenbach.de/post/2023-web-scraping/

# Start a Selenium firefox browser
driver <- rsDriver(
  browser = "firefox",
  port = 4555L,
  verbose = FALSE,
  chromever = NULL
)

# extract the client for readability of the code to follow
remote_driver <- driver[["client"]]

# Set URL
url <- "https://www.gruene-bundestag.de/presse/pressestatements"

# Navigate to the webpage
remote_driver$navigate(url)

# accept cookies
cookie_button <-
  remote_driver$findElement(using = "css selector", "button.button")
cookie_button$clickElement()

load_more <- function(rd) {
  # scroll to end of page
  rd$executeScript("window.scrollTo(0, document.body.scrollHeight);", args = list())
  # Find the "Load more" button by its CSS selector and ...
  load_more_button <-
    rd$findElement(using = "css selector", "button.button")
  # ... click it
  load_more_button$clickElement()
  # give the website a moment to respond
  Sys.sleep(3)
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
page_source <- remote_driver$getPageSource()
webpage <- read_html(page_source[[1]])

green_urls <- webpage %>%
  html_elements("article") %>%
  html_elements(".basicTeaser__wrapper") %>%
  # html_elements("a") %>%
  html_attr("href")

green_urls[1]

green_urls <- paste0("https://www.gruene-bundestag.de", green_urls)

page <- read_html(green_urls[8])



# scrape articles
get_greensbt_articles <- function(urls) {
  length <- length(urls)
  counter <- 0
  
  lapply(urls, function(url) {
    print(url)
    counter <<- counter + 1
    print(paste0("Scrape Article ", counter, " of ", length, sep = " "))
    
    html <- read_html(url)
    article <- list()
    
    article$url <- url
    
    # title
    article$title <- html %>%
      html_element("article") %>%
      html_element("h1") %>%
      html_text()
    
    #date
    article$date <- html %>%
      html_element("article") %>%
      html_element(".articleHeader__superHeadline") %>%
      html_text() %>%
      str_remove("Statement vom ")
    
    # themen
    article$topics <- html %>%
      html_element("article") %>%
      html_element(".articleHeader__keywords") %>%
      html_element("ul") %>%
      html_elements(".keyword") %>%
      html_attr("href") %>%
      str_remove("/themen/") %>%  
      list()
    
    # prompt
    article$prompt <- html %>%
      html_element("article > div > div > section > section > div > div > div > p:nth-child(1)")  %>%
      html_text2()
    
    # fulltext
    article$fulltext <- html %>%
      # html_elements("article > div > div > section > section > div > div > div > p:not(:nth-child(1))")  %>%
      html_elements("article > div > div > section > section > div > div > div > p") %>% 
      html_text2() %>%
      str_flatten() %>%
      str_squish()
    
    # authors
    article$authors <- html %>%
      html_elements(
        "article > div > div > section > section.co.coTeaserlist.co--small.bg--white > div > div > div"
      ) %>%
      html_elements(".teaserList__item > article > div.personTeaser__content")  %>%
      html_text2() %>% 
      list()
    
    # cache
    # if (!file.exists("texts/cache/greens_article_cache.csv")) {
    #   write.table(
    #     article,
    #     file = "texts/cache/greens_article_cache.csv",
    #     row.names = F,
    #     sep = ",",
    #     append = F
    #   )
    # } else {
    #   write.table(
    #     article,
    #     file = "texts/cache/greens_article_cache.csv",
    #     row.names = F,
    #     col.names = F,
    #     sep = ",",
    #     append = T
    #   )
    # }
    
    
    # after scraping four pages, wait
    if (counter %% 3 == 0) {
      Sys.sleep(2.2)
    }
    
    return(article)
    
  }) %>%
    bind_rows()
}


