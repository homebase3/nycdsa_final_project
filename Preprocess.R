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
  NIH_cleaning_func(.)

nih_specialty_lookup <- read_csv("data/brimr/nih_specialty_lookup.csv")

spec_totals <- NIH %>% 
  left_join(nih_specialty_lookup,by = "NIH DEPT COMBINING NAME") %>% 
  group_by(`ORGANIZATION ID (IPF)`, `Specialty`) %>% 
  summarize(`2019 NIH specialty funding` = sum(FUNDING))


## Read in Zip codes and make sure both orders of pairs are included
ZIPdb <- read_excel("data/geo/zip_code_database.xls")
ZIPdb_longlat <- ZIPdb %>% 
  select(zip,longitude,latitude)

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
  mutate_at(vars(zip1,zip2), as.numeric) %>%
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
## Process distances from every residency to top 160 cities for comparitive weather
sunny_percentage <- read_csv("data/weather/sunny_percentage.csv")
sunny_percentage %>% 
  mutate(zip = as.numeric(str_sub(...1,1,5))) %>% 
  left_join(ZIPdb_longlat) %>% 
  View(.)
