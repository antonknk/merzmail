# SCRIPT: 02_get_afd.R -------------------------------------------------------
#
# Author:   Anton Könneke 
#
# DESCRIPTION
#
# CREATED: 2023-06-05
#
# SETUP ------------------------------------------------------------------------
pacman::p_load(rvest, tidyverse, RSelenium)

# SCRAPE INTERACTIVE AFD PAGE --------------------------------------------------

# Instructions taken from Tim Tiefenbach Tutorial
# https://tim-tiefenbach.de/post/2023-web-scraping/

# Start a Selenium firefox browser
driver <- rsDriver(browser = "firefox",
                   port = 4555L,
                   verbose = FALSE,
                   chromever = NULL)

# extract the client for readability of the code to follow
remote_driver <- driver[["client"]]

# Set URL
url <- "https://www.afd.de/presse/"

# Navigate to the webpage
remote_driver$navigate(url)

# accept cookies
cookie_button <- remote_driver$findElement(using = "css selector", "p._brlbs-refuse-btn > a:nth-child(1)")
cookie_button$clickElement()

load_more <- function(rd) {
  # scroll to end of page
  rd$executeScript("window.scrollTo(0, document.body.scrollHeight);", args = list())
  # Find the "Load more" button by its CSS selector and ...
  load_more_button <- rd$findElement(using = "css selector", ".fusion-load-more-button")
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

# save html
page_source <- remote_driver$getPageSource()
webpage <- read_html(page_source[[1]])

afd_posts <- webpage %>% 
  html_elements("article") %>% 
  html_elements("h2 > a") %>% 
  html_attr("href") 

# SCRAPE ARTICLES  -------------------------------------------------------------
afd_posts[[1]]
