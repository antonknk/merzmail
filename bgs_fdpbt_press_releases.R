# background job: scrape fdpbt press releases
pacman::p_load(rvest, tidyverse, httr)
fdp_press_releases <- get_fdpbt_articles(fdp_urls$url)

fdp_press_releases <- fdp_press_releases %>% 
  distinct(fulltext)
write_csv(fdp_press_releases, "texts/fdpbt_press.csv")
