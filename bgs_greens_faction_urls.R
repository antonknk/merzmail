# background job: scrape green press releases
pacman::p_load(rvest, tidyverse)
green_press_releases <- get_greensbt_articles(green_urls)

get_greensbt_articles("https://www.gruene-bundestag.de/presse/pressestatements/lamya-kaddor-und-filiz-polat-zum-neuen-entwurf-des-bundesinnenministeriums-zur-reform-des-staatsangehoerigkeitsrechts")
