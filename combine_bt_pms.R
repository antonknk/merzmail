# combine press releases
p_load(tidyverse, quanteda, quanteda.textstats, readtext, quanteda.textmodels, ggforce, grid)

# IMPORT -----------------------------------------------------------------------
cdu <- read_csv("texts/cdu_articles.csv", show_col_types = FALSE)
cdu_old <- read_csv("texts/cdu_articles_addition.csv", show_col_types = FALSE)
afd <- read_csv("texts/afdbt_articles.csv", show_col_types = FALSE)
spd <- read_csv("texts/spd_press_releases.csv", show_col_types = FALSE)
spd_old <- read_csv("texts/spd_press_releases_old.csv", show_col_types = FALSE)
greens <- readRDS("texts/greensbt_press.RDS")
fdp <- read_csv("texts/fdpbt_press.csv")
left <- read_csv("texts/leftbt_press.csv")

spd <- bind_rows(spd, spd_old)
cdu <- bind_rows(cdu, cdu_old)

# UNIFY FORMAT -----------------------------------------------------------------
# URL TITLE, DATE, AUTHOR, PRIMER, FULLTEXT, ALLTEXT, OPTIONAL COLS
na.rm_text <- function(text){
  ifelse(is.na(text), "", text)
}

cdu <- cdu %>%
  mutate(
    url = url,
    date = as.Date(date, "%e.%m.%Y"),
    title = headline,
    primer = primer,
    fulltext = text
  ) %>%
  rowwise() %>%
  mutate(alltext = str_flatten(str_c(c(
    na.rm_text(title),
    na.rm_text(subtitle),
    na.rm_text(primer),
    na.rm_text(fulltext)
  ),
  collapse = " \n"))) %>% 
  ungroup()

spd <- spd %>% 
  mutate(url = url,
         date = as.Date(meta_1, format= "%d.%m.%Y"),
         title = title,
         primer = prompt,
         fulltext = fulltext) %>% 
  rowwise() %>% 
  mutate(alltext = str_flatten(
           str_c(
             c(na.rm_text(title),
               # prompt ist schon im fulltext
               na.rm_text(fulltext)),
             collapse = " \n")),
           author = meta_5,
           topics = meta_6)  %>%
  ungroup() %>% 
  select(!matches("authors_|meta"))

greens <- greens %>% 
  mutate(date = as.Date(date, "%d.%m.%Y"),
         primer = prompt) %>% 
  rowwise() %>% 
  mutate(alltext = str_flatten(
    str_c(
      c(na.rm_text(title),
        # prompt ist schon im fulltext 
        na.rm_text(fulltext)),
      collapse = " \n"
    ))) %>% 
  ungroup() %>% 
  mutate(authors = map_chr(authors, .f = ~str_c(.x, collapse = ", ")),
         topics = map_chr(topics, .f = ~str_c(.x, collapse = ", ")),)

fdp <- fdp %>% 
  rowwise() %>% 
  mutate(alltext = str_flatten(
    str_c(na.rm_text(title),
          na.rm_text(fulltext),
          collapse = " \n"
  ))) %>% 
  ungroup() %>% 
  mutate(date = as.Date(date, "%d.%m.%Y")) 

Sys.setlocale(locale = "de_DE") # for month recognition
afd <- afd %>% 
  select(date, date2, title, prompt, fulltext, url) %>% 
  mutate(date3 = str_extract(fulltext, regex("(?<=Berlin,\\s)\\d+\\.\\s\\w*\\s\\d{4}")),
         date4 = str_extract(prompt, regex("(?<=Berlin,\\s)\\d+(\\.\\s|\\s)\\w*\\s\\d{4}")),
         date4 = ifelse(str_detect(date4, "\\."), 
                        as.Date(str_extract(date4, "\\d+\\.\\s\\w*\\s\\d{4}"), format = "%e. %B %Y"),
                        as.Date(str_extract(date4, "\\d+\\s\\w*\\s\\d{4}"), format = "%e %B %Y")),
         date4 = as.Date(date4),
         across(c(date, date2, date3), ~as.Date(str_extract(.x, "\\d+\\.\\s\\w*\\s\\d{4}"), format = "%e. %B %Y")),
         date = coalesce(date, date2, date3, date4)) %>% 
  select(!c(date2, date3, date4)) %>% 
  mutate(author = str_extract(title, regex("^.*(?=\\:)")),
         author = ifelse(str_detect(author, "\\:"), 
                         str_extract(author, regex("^.*(?=\\:)")),
                         author),
         primer = prompt) %>% 
  rowwise() %>% 
  mutate(alltext = str_flatten(
    str_c(na.rm_text(title),
          na.rm_text(primer),
          na.rm_text(fulltext),
          collapse = " \n"
    ))) %>% 
  ungroup() 

left <- left %>% 
  mutate(date = as.Date(date, "%d. %b %Y")) %>% 
  rowwise() %>% 
  mutate(alltext = str_flatten(
    str_c(na.rm_text(title),
          na.rm_text(fulltext),
          collapse = " \n"
    ))) %>% 
  ungroup() 

left$party = "left"
spd$party = "spd"
greens$party = "greens"
cdu$party = "cdu/csu"
afd$party = "afd"
fdp$party = "fdp"

press_releases <- bind_rows(left, spd, cdu, greens, afd, fdp) %>% 
  select(party, date, type, url, title, primer, fulltext, alltext, author) 

press_releases %>% 
  with(., table(is.na(date), party))

saveRDS(press_releases, "texts/press_releases.rds")
