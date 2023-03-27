# Load
library(tidyverse)
library(lubridate)
library(janitor)
library(rvest)
library(httr)
library(polite)
library(data.table)

get_alone_data <- function(data = "all"){

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

suppressWarnings(
  air_data <- as.data.frame(wiki_table[1]) %>%
    row_to_names(row_number = 1) %>%
    clean_names() %>%
    select(-3))

rownames(air_data) <- c(1:nrow(air_data))

seasons_list <-
  c(wiki_table_s1[2], wiki_table[-1])


locations <-
  tribble(~season, ~location, ~destination_country,
          "1", "Vancouver Island", "Canada",
          "2", "Vancouver Island", "Canada",
          "3", "Patagonia", "Argentina",
          "4", "Vancouver Island", "Canada",
          "5", "Khonin Nuga", "Mongolia",
          "6", "Northwest Territories", "Canada",
          "7", "Northwest Territories", "Canada",
          "8", "British Columbia", "Canada",
          "9", "Labrador", "Canada",)

seasons <-
  seasons_list %>%
  bind_rows(.id = "Season") %>%
  clean_names()%>%
  select(-ref,-linked_up,-original_season) %>%
  mutate(name = str_remove_all(name, "\\[.]")) %>%
  mutate(winner = if_else(str_detect(status, "Winner"), TRUE, FALSE)) %>%
  mutate(days_lasted = if_else(str_detect(status, "day"),
                        # Detect and extract days lasted, if not convert
                        as.numeric(str_remove_all(status,"[^\\d]")),
                        as.numeric(str_remove_all(status,"[^\\d]"))/24)) %>%
  mutate(days_lasted = round(days_lasted,1)) %>%
  mutate(evacuated = if_else(str_detect(status, "evacuated"), TRUE, FALSE)) %>%
  left_join(locations, by = "season")

row.names(seasons) <- NULL


if(data == "all"){

  return(seasons)

}

if(data == "location"){

  return(locations)

}


if(data == "broadcast"){

  return(air_data)

}


}


alone_data <- get_alone_data()
saveRDS(alone_data,"data/alone_data.RDS")
write.csv(alone_data, "data/alone_data.csv")

