# get fdp bundestagsfraktions press releases
pacman::p_load(rvest, tidyverse, RSelenium)

url <- "https://www.fdpbt.de/presse"

# Start a Selenium firefox browser
driver <- rsDriver(
  browser = "firefox",
  port = 45589L,
  verbose = FALSE,
  chromever = NULL
)

# extract the client for readability of the code to follow
remote_driver <- driver[["client"]]

# Navigate to the webpage
remote_driver$navigate(url)

# accept cookies
cookie_button <-
  remote_driver$findElement(using = "css selector", "#edit-submit")
cookie_button$clickElement()

load_more <- function(rd) {
  # scroll to end of page
  rd$executeScript("window.scrollTo(0, document.body.scrollHeight);", args = list())
  # Find the "Load more" button by its CSS selector and ...
  load_more_button <-
    rd$findElement(using = "name", "op")
  # ... click it
  load_more_button$clickElement()
  # give the website a moment to respond
  Sys.sleep(1.5)
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

fdp_urls <- webpage %>% 
  html_element("#form-wrapper > div > ul") %>% 
  html_elements("li") %>% 
  html_elements("a") %>% 
  html_attr("href") %>% 
  tibble(url = .) %>% 
  filter(str_detect(url, "^\\/")) %>% 
  mutate(url = paste0("https://www.fdpbt.de", url))




# SCRAPEING
get_fdpbt_articles <- function(urls) {
  length <- length(urls)
  counter <- 0
  
  lapply(urls, function(url) {
    print(url)
    html <- httr::GET(url,
                      httr::set_cookies(
                        .cookies =
                          c(
                            `_pk_id.1.513c`	= "7625da8f2f7960ab.1687734520.",
                            `_pk_ses.1.513c` = "1",
                            `cookie-agreed`	= "2",
                            `cookie-agreed-categories` = "%5B%22bundestag_media_library%22%2C%22cloudinary%22%2C%22facebook_embed%22%2C%22instagram_embed%22%2C%22matomo%22%2C%22slides%22%2C%22twitter_embed%22%2C%22youtube_embed%22%5D",
                            `cookie-agreed-status` = "all",
                            `cookie-agreed-timestamp` =	"1687734519",
                            `cookie-agreed-version` =	"unknown"
                          )
                      )) %>%
      read_html()
    
    counter <<- counter + 1
    print(paste0("Scrape Article ", counter, " of ", length, sep = " "))
    
    # is_pm_statement <-
    #   tryCatch({
    #     html %>%
    #       html_elements(
    #         "#block-uv-fdpbt-theme-content > article > div.mdb-wrapper.full > div > div.person-data2 > div"
    #       ) %>%
    #       html_text2() %>% 
    #       tolower() %>% 
    #       str_detect("presse|statement")
    #     },
    #     error = function(e) {
    #       F
    #     }
    #   )
    
    is_no_pm_statement <- html %>%
      html_elements(
        "#block-uv-fdpbt-theme-content > article > div.mdb-wrapper.full > div > div.person-data2 > div"
      ) %>%
      html_text2() %>% 
      tolower() %>% 
      str_detect("presse|statement") %>% 
      is_empty()
       
    
    if (!is_no_pm_statement) {
      article <- list()
      
      article$url <- url
      
      # title
      article$title <- html %>%
        html_element("body > div > div > main > div > div > #block-uv-fdpbt-theme-content > article > h1") %>%
        html_text2()
      
      #date
      article$date <- html %>%
        html_element("body > div > div > main > div > div > #block-uv-fdpbt-theme-content > article > time") %>%
        html_text2()
      
      
      # fulltext
      article$fulltext <- html %>%
        html_element("body > div > div > main > div > div > #block-uv-fdpbt-theme-content") %>%
        html_elements("article:not(div) > p") %>%
        html_text2() %>%
        str_flatten() %>%
        str_squish()
      
      # authors
      article$authors <- html %>%
        html_elements(
          "#block-uv-fdpbt-theme-content > article > div.mdb-wrapper.full > div > div.person-data2 > div"
        ) %>%
        html_text2() %>%
        str_remove(" Pressemitteilung")
      
      article$type <-
        ifelse(
          html %>%
            html_elements(
              "#block-uv-fdpbt-theme-content > article > div.mdb-wrapper.full > div > div.person-data2 > div"
            ) %>%
            html_text2() %>%
            str_detect("Pressemitteilung"),
          "Pressemitteilung",
          ifelse(
            html %>%
              html_elements(
                "#block-uv-fdpbt-theme-content > article > div.mdb-wrapper.full > div > div.person-data2 > div"
              ) %>%
              html_text2() %>%
              str_detect("Statement"),
            "Statement",
            NA
          )
        )
      
      
      
      
      
      # cache
      if (!file.exists("texts/cache/fdpbt_article_cache.csv")) {
        write.table(
          article,
          file = "texts/cache/fdpbt_article_cache.csv",
          row.names = F,
          sep = ",",
          append = F
        )
      } else {
        write.table(
          article,
          file = "texts/cache/fdpbt_article_cache.csv",
          row.names = F,
          col.names = F,
          sep = ",",
          append = T
        )
      }
      
      
      # after scraping four pages, wait
      if (counter %% 5 == 0) {
        Sys.sleep(1.2)
      }
      
      return(article)
    } else {
      print("Url is neither pm nor press statement and was skipped")
      return(NULL) 
    }
  }) %>%
    bind_rows()
}





