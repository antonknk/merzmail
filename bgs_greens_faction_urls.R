# background job: scrape green press releases
pacman::p_load(rvest, tidyverse)
green_press_releases <- get_greensbt_articles(green_urls)

saveRDS(green_press_releases, "texts/greensbt_press.RDS")
