library(readr)
library(mapboxapi)
library(tidyverse)

# NOTE: DON'T RUN REPEATEDLY, FILES HAVE ALREADY BEEN SAVED.
# Geocode for weather stations
weather_geocodes <- read_csv("data/weather/sunny_percentage.csv") %>% 
  select(...1) %>% 
  mutate(submit = str_sub(...1,6,-1)) %>% 
  mutate_at(vars("submit"), ~gsub(",",", ",.)) %>% 
  mutate_at(vars("submit"), ~paste0(.,", United States"))

### Expensive code
out <- sapply(weather_geocodes$submit, mb_geocode)
### 


t(out) %>%
  as.data.frame(.) %>% 
  mutate(submit = row.names(.)) %>% 
  left_join(weather_geocodes,.) %>% 
  select(-...1)-> weather_geocodes

colnames(weather_geocodes) <- c("Key", long, lat)

weather_geocodes %>% 
  write.csv(., "weather_geocodes.csv")


