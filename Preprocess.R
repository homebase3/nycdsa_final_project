library(tidyverse)
library(magrittr)
library(readxl)
library(data.table)
library(janitor)
library(readr)
library(fuzzyjoin)
library(zipcodeR)
library(stringr)
library(parallel)
library(geosphere)
library(tm)
library(ggmap)
library(numform)

# read in aamc data files
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
dat <- unique(dat)
# dat %>%
#   group_by(Specialty) %>%
#   summarize(count = n()) %>%
#   View(.)

# Remove unnecessary columns
dat %<>%
  select(-contains("Comparison to Matched Applicants:")) %>% 
  arrange(Specialty, `Residency program name`)

# Process and join link files
file_list <- list.files("data/AAMC_links")
for (num in seq_along(file_list)) {
  read_csv(paste0("data/AAMC_links/",file_list[num])) -> out
  
  if(num == 1) {
    links <- out
  } else {
    links <- bind_rows(links,out)
  }
}


dat %<>% 
  left_join(links, by=c("Specialty","Residency program name" = "Program")) %>% 
  select(-`...1`) %>% 
  mutate(link = paste0('https://www.residencyexplorer.org',`Link suffix`))

# Read in NIH file
stopwords = c("a","of","the","school","hospital","program","medical", "residency", "medicine","system","health", "center","college", "for","university")

NIH <- read_excel("data/brimr/Worldwide_2019.xls", skip = 1)
NIH_cleaning_func <- function(df) {
  df %>% 
  drop_na(`ORGANIZATION NAME`) %>% 
  mutate_at(vars(`ORGANIZATION NAME`), tolower) %>% 
  mutate_at(vars(`ORGANIZATION NAME`), ~gsub("[[:punct:][:blank:]]+", " ",.)) %>%
  mutate(org_cleaned = lapply(`ORGANIZATION NAME`, function(word) removeWords(word,stopwords))) %>% 
  return(.)
}


totals <- NIH %>% 
  group_by(`ORGANIZATION NAME`,`ORGANIZATION ID (IPF)`, `ZIP CODE`) %>% 
  summarize(tot = sum(FUNDING, na.rm = T)) %>% 
  NIH_cleaning_func()

nih_specialty_lookup <- read_csv("data/brimr/nih_specialty_lookup.csv")

spec_totals <- NIH %>% 
  left_join(nih_specialty_lookup,by = "NIH DEPT COMBINING NAME") %>% 
  group_by(`ORGANIZATION ID (IPF)`, `Specialty`) %>% 
  summarize(`2019 NIH specialty funding` = sum(FUNDING))


## Read in Zip codes and make sure both orders of pairs are included
ZIPdb <- read_excel("data/geo/zip_code_database.xls")
ZIPdb_longlat <- ZIPdb %>% 
  select(zip,longitude,latitude) %>% 
  mutate_at(vars(zip), f_pad_zero)

## Join in best match NIH data based on combination of geo and string_dist match for all specialties
dat %<>%
  mutate(`Residency program name_lower` = tolower(`Residency program name`)) %>%
  mutate_at(vars("Residency program name_lower"), ~gsub("[[:punct:][:blank:]]+", " ",.)) %>%
  mutate(org_cleaned = lapply(`Residency program name_lower`, function(word) removeWords(word,stopwords))) %>% 
  stringdist_left_join(totals, by = c("org_cleaned"), distance_col = "match_distance",max_dist = 10) %>%
  mutate(zip1 = str_sub(Zip,1,5)) %>%
  mutate(zip2 = str_sub(`ZIP CODE`,1,5)) %>%
  filter(str_length(zip1) == 5) %>%
  filter(str_length(zip2) == 5) %>%
  # mutate_at(vars(zip1,zip2), as.numeric) %>%
  left_join(.,ZIPdb_longlat, by = c("zip1" = "zip")) %>%
  left_join(.,ZIPdb_longlat, by = c("zip2" = "zip")) %>%
  mutate(geo_distance = distHaversine(bind_cols(longitude.x,latitude.x),bind_cols(longitude.y,latitude.y))*0.0006213712) %>%
  filter(geo_distance <= 5) %>%
  .[order(.$Specialty,.$`Residency program name`,.$match_distance),] %>%
  .[!duplicated(.$ID),] %>% 
  mutate(str_length = pmax(str_length(org_cleaned.x), str_length(org_cleaned.y))) %>% 
  mutate(dist_ratio = match_distance / str_length) %>%
  filter(dist_ratio <= 0.5) %>% 
  select(ID, `ORGANIZATION ID (IPF)`, tot) %>% 
  rename(`2019 NIH total funding` = tot) %>% 
  left_join(dat,.,by = "ID") %>% 
  left_join(.,spec_totals, by = c("Specialty", "ORGANIZATION ID (IPF)")) #specialty-funding

# Join in Board scores
## Family medicine
family_medicine_pass_rates <- read_excel("data/programs/family_medicine_pass_rates.xlsx")
family_medicine_pass_rates$`Pass %`[family_medicine_pass_rates$`Pass %` == "N/A"] <- NA
family_medicine_pass_rates %>% 
  select(-`Total residents per year`)

dat %>% 
  left_join(family_medicine_pass_rates) %>% 
  rename(`Family medicine board pass rate` = `Pass %`) %>% 
  drop_na(`Family medicine board pass rate`) %>% 
  select(`Residency program name`, `Family medicine board pass rate`) %>% 
  left_join(dat,.) -> dat

## Internal medicine
internal_medicine_pass_rates <- read_csv("data/programs/internal_medicine_pass.csv")
internal_medicine_pass_rates %>% 
  mutate_at(vars(`Percent Passing`), ~parse_number(.)/100) %>% 
  rename(`Internal medicine board pass rate` = `Percent Passing`) %>% 
  mutate(`Residency program name` = str_sub(`Program Name City and State`, 1, str_locate(`Program Name City and State`, "Program")[,2])) %>% 
  select(`Residency program name`, `Internal medicine board pass rate`) %>% 
  left_join(dat,.,by = "Residency program name") -> dat

## Pediatrics (fuzzy match with city, state + string_dist)
pediatrics_pass_rates <- read_csv("data/programs/pediatrics_pass_rates.csv")
dat %<>%
  mutate(`Residency program name_lower`= tolower(`Residency program name`)) %>%
  mutate_at(vars("Residency program name_lower"), ~gsub("[[:punct:][:blank:]]+", " ",.)) %>%
  mutate(org_cleaned = lapply(`Residency program name_lower`, function(word) removeWords(word,stopwords))) #add org-cleaned back

pediatrics_pass_rates %>% 
  mutate_at(vars(`Percent Passing`), ~parse_number(.)/100) %>% 
  rename(`Pediatrics board pass rate` = `Percent Passing`) %>% 
  mutate(`Residency program name` = str_sub(`Program Name`, 1, str_locate(`Program Name`, "\n")[,1]-1)) %>% 
  mutate(`City and state` = str_sub(`Program Name`, str_locate(`Program Name`, "\n")[,2]+1,-1)) %>% 
  mutate(City = str_sub(`City and state`, 1, str_locate(`City and state`, ", ")[,1]-1)) %>% 
  mutate(State = str_sub(`City and state`, str_locate(`City and state`, ", ")[,2]+1),-1) %>% 
  mutate_at(vars(`Residency program name`), ~paste0(.," Program")) %>% 
  select(`Residency program name`, City, State, `Pediatrics board pass rate`) %>% 
  mutate(`Residency program name_lower` = tolower(`Residency program name`)) %>%
  mutate_at(vars("Residency program name_lower"), ~gsub("[[:punct:][:blank:]]+", " ",.)) %>%
  mutate(org_cleaned = lapply(`Residency program name_lower`, function(word) removeWords(word,stopwords))) %>% 
  stringdist_left_join(dat,., by = "org_cleaned", distance_col = "match_distance",max_dist = 1) %>%
  filter(City.x == City.y,State.x == State.y) %>% 
  mutate(str_length = pmax(str_length(org_cleaned.x), str_length(org_cleaned.y))) %>% 
  mutate(dist_ratio = match_distance / str_length) %>%
  filter(dist_ratio <= 0.5) %>% 
  .[order(.$Specialty,.$ID,.$match_distance),] %>%
  .[!duplicated(.$ID),] %>%
  select(`ID`, `Pediatrics board pass rate`) %>% 
  left_join(dat,., by = "ID") %>% 
  select(-`Residency program name_lower`,-org_cleaned) -> dat

## General surgery
surgery_pass_rates <- read_csv("data/programs/surgery_pass_rates.csv")
dat %<>%
  mutate(`Residency program name_lower`= tolower(`Residency program name`)) %>%
  mutate_at(vars("Residency program name_lower"), ~gsub("[[:punct:][:blank:]]+", " ",.)) %>%
  mutate(org_cleaned = lapply(`Residency program name_lower`, function(word) removeWords(word,stopwords))) #add org-cleaned back

surgery_pass_rates %>% 
  mutate_at(vars(`CE Success`), ~parse_number(.)/100) %>% 
  rename(`Surgery board pass rate` = `CE Success`) %>% 
  mutate(`Residency program name` = str_sub(Program, 1, str_locate(Program, "\n")[,1]-1)) %>% 
  mutate(`State` = str_sub(Program, str_locate(Program, "\n")[,2]+1,-1)) %>%  #city not available)
  mutate_at(vars(`Residency program name`), ~paste0(.," Program")) %>% 
  select(`Residency program name`, State, `Surgery board pass rate`) %>% 
  mutate(`Residency program name_lower` = tolower(`Residency program name`)) %>%
  mutate_at(vars("Residency program name_lower"), ~gsub("[[:punct:][:blank:]]+", " ",.)) %>%
  mutate(org_cleaned = lapply(`Residency program name_lower`, function(word) removeWords(word,stopwords))) %>% 
  stringdist_left_join(dat,., by = "org_cleaned", distance_col = "match_distance",max_dist = 5) %>%
  filter(State.x == State.y) %>% 
  mutate(str_length = pmax(str_length(org_cleaned.x), str_length(org_cleaned.y))) %>% 
  mutate(dist_ratio = match_distance / str_length) %>%
  filter(dist_ratio <= 0.5) %>% 
  .[order(.$Specialty,.$ID,.$match_distance),] %>%
  .[!duplicated(.$ID),] %>%
  select(`ID`, `Surgery board pass rate`) %>% 
  left_join(dat,., by = "ID") %>% 
  select(-`Residency program name_lower`,-org_cleaned) -> dat

## Note -> do aggregation later after random forest

# Read in weather data
## Build weather station lookup

weather_geocodes <- read_csv("data/weather/weather_geocodes.csv")
`%notin%` <- Negate(`%in%`) #create notin operator
weather_geocodes %<>% 
  filter(...1 %notin% c(153:159)) #removing non-US state that were incorrectly geocoded

weather_geocodes[weather_geocodes$submit == "NEWYORKCentralPar, United States", 3:4] <- list(-73.935242, 40.730610) #manually correct NY geocode

weather_key_helper <- read_csv("data/weather/sunny_percentage.csv")

weather_lat_long <- bind_cols(weather_geocodes$V1,weather_geocodes$V2)

best_geocode_match <- function(lat,lng,underlying, key_helper) {
  distHaversine(bind_cols(lng,lat),underlying) %>% 
    which.min(.) %>% 
    key_helper[[1]][.] %>% 
    return(.)
}

best_weather_geocode_match <- function(lat,lng) {
  best_geocode_match(lat,lng,weather_lat_long, weather_key_helper) %>% 
    return(.)
}

dat %<>%
  mutate(zip5 = str_sub(Zip,1,5)) %>%
  left_join(.,ZIPdb_longlat, by = c("zip5" = "zip")) %>% 
  mutate(weather_key = mapply(best_weather_geocode_match, latitude, longitude)) 
  
dat %<>% 
  mutate(weather_key_5 = str_sub(weather_key,1,5)) #separated to reduce key errors

## join in sun percentage
sunny_percentage <- read_csv("data/weather/sunny_percentage.csv")
sunny_percentage %>% 
  mutate(`Mean % sunny` = ANN/100) %>% 
  mutate(weather_key_5 = str_sub(...1,1,5)) %>% 
  select(weather_key_5, `Mean % sunny`) %>% 
  left_join(dat,.) -> dat

## join in precipitation data
precipitation_days <- read_csv("data/weather/precipitation_days.csv")
precipitation_days %>% 
  mutate(`% days with precipitation` = ANN/365.25) %>% 
  mutate(weather_key_5 = str_sub(...1,1,5)) %>% 
  select(weather_key_5, `% days with precipitation`) %>% 
  left_join(dat,.) -> dat

## join in snow
snowfall_average <- read_csv("data/weather/snowfall_average.csv")
snowfall_average %>% 
  mutate(`Annual snowfall` = ANN) %>% 
  mutate(weather_key_5 = str_sub(...1,1,5)) %>% 
  select(weather_key_5, `Annual snowfall`) %>% 
  left_join(dat,.) -> dat

## join in cold data
mean_days_32 <- read_csv("data/weather/mean_days_<32.csv")
mean_days_32 %>% 
  mutate(`% days below freezing` = ANN/365.25) %>% 
  mutate(weather_key_5 = str_sub(...1,1,5)) %>% 
  select(weather_key_5, `% days below freezing`) %>% 
  left_join(dat,.) -> dat

## join in heat data
mean_days_90 <- read_csv("data/weather/mean_days_>90.csv")
mean_days_90 %>% 
  mutate(`% days above 90F` = ANN/365.25) %>% 
  mutate(weather_key_5 = str_sub(...1,1,5)) %>% 
  select(weather_key_5, `% days above 90F`) %>% 
  left_join(dat,.) -> dat

## remove joining variables
dat %<>%
  select(-weather_key,-weather_key_5)

# Join in cost of living index
## Build cost of living lookup
cost_of_living_index_geocodes <- read_csv("data/cost of living/cost_of_living_index_geocodes.csv")
cost_of_living_index_geocodes %<>% select(-1) 

cost_of_living_lat_long <- bind_cols(cost_of_living_index_geocodes$V1,cost_of_living_index_geocodes$V2)

best_cost_of_living_geocode_match <- function(lat,lng) {
  best_geocode_match(lat,lng,cost_of_living_lat_long, cost_of_living_index_geocodes) %>% 
    return(.)
}

dat %<>%
  mutate(cost_of_living_key = mapply(best_cost_of_living_geocode_match, latitude, longitude)) %>% 
  mutate_at(vars("cost_of_living_key"), as.character)

cost_of_living_index <- read_csv("data/cost of living/advisorsmith_cost_of_living_index.csv")
cost_of_living_index %>% 
  mutate(cost_of_living_key = paste0(City, ", ", State, ", United States")) %>% 
  select(-City, -State) %>% 
  left_join(dat,.) %>% 
  select(-cost_of_living_key) -> dat

# Join in geographical identifiers
## Join in FIPS
ZIP_COUNTY_FIPS <- read_csv("data/geo/ZIP-COUNTY-FIPS_2018-03.csv")
ZIP_COUNTY_FIPS %>% 
  select(ZIP, STCOUNTYFP) %>% 
  left_join(dat,.,by = c("zip5" = "ZIP")) -> dat

## Join in MSA
FIPS_to_MSA_2020 <- read_excel("data/geo/FIPS_to_MSA_2020.xls", 
                               skip = 2)
FIPS_to_MSA_2020 %>% 
  mutate(`Micro/Metro Area Name` = paste0(`CBSA Title`," ", `Metropolitan/Micropolitan Statistical Area`)) %>% 
  mutate(STCOUNTYFP = paste0(`FIPS State Code`,`FIPS County Code`)) %>% 
  select(STCOUNTYFP,`Micro/Metro Area Name`) %>% 
  left_join(dat,.) -> dat

## Join in county health parameters
County_Health_Rankings_Data <- read_excel("data/county health/2021 County Health Rankings Data - v1.xlsx", 
                                                   sheet = "Ranked Measure Data", skip = 1)
County_Health_lookup_key <-  read_excel("data/county health/2021 County Health Rankings Data - v1.xlsx", 
                                        sheet = "Lookup key")

## Select and make all numeric
County_Health_Rankings_Data %>% 
  select(FIPS,any_of(colnames(County_Health_lookup_key))) %>% 
  mutate_at(vars(contains("ratio")),~as.numeric(str_sub(.,1,-3))) %>% 
  mutate_at(vars(`Presence of Water Violation`),~recode(., Yes = 1, No = 0)) -> county_rankings

for (col in 2:ncol(county_rankings)) {
  ecdf_it <- ecdf(county_rankings[[col]])
  if(County_Health_lookup_key[[col-1]][5] == "Neg") {
    county_rankings[[paste0(colnames(county_rankings)[col]," (percentile")]] <- 1 - ecdf_it(county_rankings[[col]])
  } else {
    county_rankings[[paste0(colnames(county_rankings)[col]," (percentile)")]] <- ecdf_it(county_rankings[[col]])
  }
}

level1 <- County_Health_lookup_key[2,] %>% 
  as.character(.) %>% 
  unique(.)

for (val in level1) {
  relevant <- (County_Health_lookup_key[2,] == val)
  weights <- as.numeric(County_Health_lookup_key[1,])
  weighted_outcome_func <- function(row_num) {
    rankings <- as.numeric(county_rankings[row_num,37:71])
    sum(relevant*weights*rankings,na.rm = T)/sum(relevant*weights*(!is.na(rankings)), na.rm = T)
  }
  tot <- sapply(1:nrow(county_rankings), weighted_outcome_func)
  ecdf_it <- ecdf(tot)
  county_rankings[[paste0(val," (percentile)")]] <- ecdf_it(tot)
}
level2 <- County_Health_lookup_key[3,] %>% 
  as.character(.) %>% 
  unique(.)

for (val in level2) {
  relevant <- (County_Health_lookup_key[3,] == val)
  weights <- as.numeric(County_Health_lookup_key[1,])
  weighted_outcome_func <- function(row_num) {
    rankings <- as.numeric(county_rankings[row_num,37:71])
    sum(relevant*weights*rankings,na.rm = T)/sum(relevant*weights*(!is.na(rankings)), na.rm = T)
  }
  tot <- sapply(1:nrow(county_rankings), weighted_outcome_func)
  ecdf_it <- ecdf(tot)
  county_rankings[[paste0(val," (percentile)")]] <- ecdf_it(tot)
}

level3 <- County_Health_lookup_key[4,] %>% 
  as.character(.) %>% 
  unique(.)

for (val in level3) {
  relevant <- (County_Health_lookup_key[4,] == val)
  weights <- as.numeric(County_Health_lookup_key[1,])
  weighted_outcome_func <- function(row_num) {
    rankings <- as.numeric(county_rankings[row_num,37:71])
    sum(relevant*weights*rankings,na.rm = T)/sum(relevant*weights*(!is.na(rankings)), na.rm = T)
  }
  tot <- sapply(1:nrow(county_rankings), weighted_outcome_func)
  ecdf_it <- ecdf(tot)
  county_rankings[[paste0(val," (percentile)")]] <- ecdf_it(tot)
}

county_rankings %>% 
  select(any_of("FIPS"),contains(level1), contains(level2), contains(level3)) %>% 
  left_join(dat,.,by=c("STCOUNTYFP" = "FIPS")) -> dat

# read in demographic statistics

County_Health_Rankings_Data_demography <- read_excel("data/county health/2021 County Health Rankings Data - v1.xlsx", 
                                                   sheet = "Additional Measure Data", skip = 1)
County_Health_Rankings_Data_demography %>% 
  select(FIPS,Population,`% Less Than 18 Years of Age`,
         `% 65 and Over`,`% Black`,`% American Indian & Alaska Native`,
         `% Asian`,`% Native Hawaiian/Other Pacific Islander`,`% Hispanic`,`% Non-Hispanic White`,`% Female`,`% Rural`) %>% 
  left_join(dat,.,by=c("STCOUNTYFP" = "FIPS")) -> dat



  
