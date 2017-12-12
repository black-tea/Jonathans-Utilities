require(rvest)
library(magrittr)
library(stringr)

pg <- read_html('C:/Users/Tim/Documents/GitHub/Jonathans-Utilities/TPS/RA62/mymap0.html')

pg %>% 
  html_nodes('script') %>%
  extract2(2) %>% 
  html_text %>% 
  str_match_all('title: "(.*)"') %>%
  extract2(1) -> lat_lng 

lat_lng
