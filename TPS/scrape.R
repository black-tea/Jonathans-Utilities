require(rvest)
library(magrittr)
library(stringr)

pg <- read_html('C:/Users/Tim/Documents/GitHub/Jonathans-Utilities/TPS/RA62/mymap0.html')

pg %>% 
  html_nodes('script') %>%
  # Extract second 'script' node
  extract2(2) %>% 
  html_text %>%
  # Regex match formula
  str_match_all('title: "(.*)"') %>%
  # Pull out content within ()
  extract2(1) -> lat_lng

# Grab second column, split
lat_lng <- strsplit(lat_lng[,2], ", ")

print(df)
