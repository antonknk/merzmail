# background script: get afd faction urls

source("get_afd_faction.R")
# afdbt_urls_bg <- afdbt_get_urls(pages = c(0:750))
afdbt_press <- get_afdbt_articles(urls = afdbt_urls_bg$urls[738:6008])
