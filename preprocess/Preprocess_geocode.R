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

# Program geocodes
file_list <- list.files("data/AAMC")

for (num in seq_along(file_list)) {
  read_excel(paste0("data/AAMC/",file_list[num])) %>% 
    colnames(.)[1] %>% 
    colnames(.) %>% 
    gsub(".*: ","",.) -> specialty_name
  
  read_excel(paste0("data/AAMC/",file_list[num]),skip = 1)  %>% 
    colnames(.) %>% 
    .[-1] -> ID
  
  read_excel(paste0("data/AAMC/",file_list[num]), skip = 1) %>% 
    t(.) %>% 
    as.data.frame(.) %>% 
    row_to_names(row_number = 1) %>% 
    mutate(Specialty = specialty_name, .before = 1) %>% 
    mutate(ID = ID, .before = 1)-> out
  
  if(num == 1) {
    dat <- out
  } else {
    dat <- bind_rows(dat,out)
  }
}

# Remove duplicates and check to ensure have the correct number of programs in each speciality
dat_geocodes <- unique(dat)

dat_geocodes %<>% 
  select(1:6) %>% 
  mutate(submit = paste(str_sub(`Residency program name`,1,-9),City,State, Zip,"USA", sep = ", ")) %>% 
  .[!duplicated(.$submit),] %>% 
  mutate(len_submit = sapply(strsplit(submit, " "), length)) %>% 
  mutate(submit_adj = if_else(len_submit > 20, paste(str_sub(`Residency program name`,1,-9), Zip,"USA", sep = ", "),submit )) 

row.names(dat_geocodes) <- NULL


### Expensive code
len_run <- 100
runs <- ceiling(nrow(dat_geocodes)/len_run)
out <- list()
for (i in 1:runs) {
  out[[i]] <- sapply(dat_geocodes$submit_adj[(len_run*(i-1) + 1):min(nrow(dat_geocodes),len_run*i)], mb_geocode)
}
### 

for (i in 1:runs) {
  if (i == 1) {
    out_full <- as.data.frame(out[[i]])
  } else {
    out_full <- bind_cols(out_full, as.data.frame(out[[i]]))
  }
}


t(out_full) %>%
  as.data.frame(.) %>% 
  mutate(submit_adj = row.names(.)) %>% 
  rename(longitude = V1) %>% 
  rename(latitude = V2) %>% 
  select(submit_adj, longitude, latitude) %>% 
  left_join(dat_geocodes,.) %>% 
  select(ID, longitude, latitude) %>% 
  write.csv(., "geo/program_geocodes.csv")


#NIH geocodes
NIH <- read_excel("data/brimr/Worldwide_2019.xls", skip = 1)

NIH %>% 
  mutate(submit = if_else(is.na(`ZIP CODE`),
                          str_c(`ORGANIZATION NAME`,CITY,`STATE OR COUNTRY NAME`, sep = ", "),
                          str_c(`ORGANIZATION NAME`,CITY,`STATE OR COUNTRY NAME`,`ZIP CODE`, sep = ", "))) %>% 
  .[!duplicated(.),] %>% 
  .$submit %>% 
  unique(.)


  
