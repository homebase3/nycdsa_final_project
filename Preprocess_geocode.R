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

# Geocode for cost_of_living cities
cost_of_living_index <- read_csv("data/cost of living/advisorsmith_cost_of_living_index.csv")
cost_of_living_index %<>% 
  mutate(submit = paste0(City, ", ", State, ", United States"))

### Expensive code
out <- sapply(cost_of_living_index$submit, mb_geocode)
###


t(out) %>%
  as.data.frame(.) %>% 
  mutate(submit = row.names(.)) %>% 
  left_join(cost_of_living_index,.) %>% 
  select(submit, V1, V2)-> cost_of_living_index_geocodes

cost_of_living_index_geocodes %>% 
  write.csv(., "cost_of_living_index_geocodes.csv")

# Add in LGBT MSA translation
lgbt_data <- read_excel("data/population/lgbt_data.xlsx", 
                        sheet = "Sheet2")
MSAs <- FIPS_to_MSA_2020['CBSA Title'] %>% 
  mutate(State = str_sub(`CBSA Title`, -2, -1)) %>% 
  unique(.) %>% 
  drop_na(.)

lgbt_data$percent_key <- as.data.frame(str_locate(lgbt_data$Data,"%"))$start
lgbt_data %>% 
  mutate(`% LGBT` = as.numeric(str_sub(Data,percent_key - 3, percent_key -1))/100) %>% 
  mutate(MSA = str_sub(Data,1, percent_key -4)) %>% 
  mutate(State = str_sub(MSA, -3, -2)) %>% 
  write.csv('lgbt_data_with_MSA.csv')
