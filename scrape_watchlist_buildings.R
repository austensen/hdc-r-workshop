# Scrape list of buildings on the Public Advocate's NYC Landlord Watchlist

# Helpful guide to getting started with web-scraping in R with rvest
# https://blog.rstudio.com/2014/11/24/rvest-easy-web-scraping-with-r/

library(tidyverse)
library(rvest)

dir.create("data", recursive = TRUE)

watchlist_url <- "http://landlordwatchlist.com"

watchlist_page <- read_html(watchlist_url)

# For a given landlord, scrape info on all buildings, returning a dataframe for
# each building with columns for address, units, and HPD violations
get_buildings_info <- function(landlord_name) {
  landlord_page <- str_glue("{watchlist_url}/landlord-{landlord_name}") %>% 
    URLencode() %>% 
    read_html()
  
  address <- landlord_page %>% 
    html_nodes("h2") %>% 
    html_text() %>% 
    str_trim()
  
  units <- landlord_page %>% 
    html_nodes("h4:nth-child(4) d") %>% 
    html_text() %>% 
    as.integer()
  
  violations <- landlord_page %>% 
    html_nodes("h4:nth-child(6) d") %>% 
    html_text() %>% 
    as.integer()
  
  tibble(address, units, violations)
}

# RegEx to match entire address and capture the following adddress components
parse_address_pat <- "^(\\d+[A-Z]?(?:-\\d+[A-Z]?)?)\\s+([^,]+),\\s+([A-Z]+(?:\\s[A-Z]+)?)\\s+(\\d+)$"

watchlist_buildings <- tibble(
  landlord = watchlist_page %>% 
    html_nodes("h2") %>% 
    html_text() %>% 
    str_remove("^#\\d+\\s*"),
  bldg_info = map(landlord, get_buildings_info)
) %>% 
  unnest(bldg_info) %>% 
  mutate(parsed_address = str_replace(address, parse_address_pat, "\\1,\\2,\\3,\\4")) %>% 
  separate(parsed_address, c("number", "street", "borough", "zip"), sep = ",") %>% 
  write_csv(str_glue("data/landlord-watchlist-buildings_{Sys.Date()}.csv"))
