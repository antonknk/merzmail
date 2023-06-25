# background script: get afd faction urls

source("get_afd_faction.R")
# afdbt_urls_bg <- afdbt_get_urls(pages = c(0:750))
afdbt_press <- get_afdbt_articles(urls = afdbt_urls_bg$urls[4763:6008])
afd_articles <- read_csv("texts/cache/afd_article_cache.csv")

afdarticles <- afd_articles %>% 
  unique()

# remove empty articles that only linked legislative docs
afd_articles <-  afd_articles %>% 
  filter(!is.na(fulltext))

# save to disk 
write_csv(afd_articles, "texts/afdbt_articles.csv")
