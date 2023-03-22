# Load
library(tidyverse)
library(lubridate)
library(janitor)
library(rvest)
library(httr)
library(polite)
library(data.table)


get_alone_data <- function(url){

url <- "https://en.wikipedia.org/wiki/Alone_(TV_series)"
url_s1 <- "https://en.wikipedia.org/wiki/Alone_(season_1)"

url_bow <- polite::bow(url)
url_bow_s1 <- polite::bow(url_s1)

wiki_table <-
  polite::scrape(url_bow) %>%
  rvest::html_nodes("table.wikitable") %>%
  rvest::html_table(fill = TRUE)

wiki_table_s1 <-
  polite::scrape(url_bow_s1) %>%
  rvest::html_nodes("table.wikitable") %>%
  rvest::html_table(fill = TRUE)

suppressWarnings(air_data <- as.data.frame(wiki_table[1]) %>%
                   row_to_names(row_number = 1) %>%
                   clean_names() %>%
                   select(-3))

seasons <-
  c(wiki_table_s1[2], wiki_table[-1])

seasons <-
seasons%>%
  map2(., c(1:length(seasons)), ~cbind(.x, season = .y)) %>%
  map(., ~ clean_names(.)) %>%
  map(., ~ select(.,-ref))

}


seasons <- get_alone_data()
