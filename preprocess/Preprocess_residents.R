# import packages
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
library(HistogramTools)
library(plotly)

# read in specialty names

file_list <- list.files("data/AAMC")

specialty_name_vec = c()
for (num in seq_along(file_list)) {
  read_excel(paste0("data/AAMC/",file_list[num])) %>% 
    colnames(.)[1] %>% 
    colnames(.) %>% 
    gsub(".*: ","",.) -> specialty_name
  specialty_name_vec = c(specialty_name_vec,specialty_name)
}

spec_dat <- specialty_name_vec %>% 
  unique(.) %>% 
  as.data.frame(.)

colnames(spec_dat) <- "Specialty"

# read in program director preferences for applicant rating weights
PD_Survey_sub <- read_excel("data/residents/2020-PD-Survey_sub.xlsx")

PD_Survey_sub %>% 
  filter(`Factor` %in% c("USMLE Step 1 score",
                         "USMLE Step 2 CK score",
                         "Demonstrated involvement and interest in research",
                         "Other life experience",
                         "Volunteer/extracurricular experiences")) %>% 
  mutate(Weight = `Percent Citing Factor` * `Average Rating`) %>% 
  select(-`Percent Citing Factor`, -`Average Rating`) %>% 
  pivot_wider(names_from = Factor, values_from = Weight) %>% 
  rename(`Number of volunteer experiences` = `Volunteer/extracurricular experiences`) %>% 
  mutate(`Number of abstracts, presentations, and publications` = `Demonstrated involvement and interest in research` / 2) %>% #splitting research score over two categories
  mutate(`Number of research experiences` = `Demonstrated involvement and interest in research` / 2) %>%  #splitting research score over two categories
  mutate(`Number of work experiences` = `Other life experience` / 2) %>%  #life experience is not just work
  select(-`Other life experience`, -`Demonstrated involvement and interest in research`) %>% 
  mutate(tot = rowSums(.[, sapply(., is.numeric)])) %>% 
  mutate_at(vars(-Specialty), ~ ./tot) %>% 
  select(-tot) -> PD_Survey_weights



# read in step distribution
dists <- list()
dists_raw <- list()
dists[["USMLE STEP 1 Score"]] <- list()
dists[["USMLE STEP 2 CK Score"]] <- list()
dists_raw[["USMLE STEP 1 Score"]] <- list()
dists_raw[["USMLE STEP 2 CK Score"]] <- list()
sheet_names <- excel_sheets("data/residents/test_scores_2.xlsx")  

make_distribution_raw <- function(df) {
  vals = c()
  for (num in 1:nrow(df)) {
    vals = c(vals, rep(df[[2]][num],df[[1]][num]))
  }
  return(vals)
}
  
make_distribution <- function(df) {
  make_distribution_raw(df) %>% 
    hist(.) %>% 
    HistToEcdf(method = 'linear') %>% 
    return(.)
}

spec_step <- c()
for (sheet in sheet_names) {
  read_excel("data/residents/test_scores_2.xlsx", sheet = sheet) %>% 
    colnames(.)[1] %>% 
    colnames(.) -> specialty_name
  
  print(specialty_name)
  spec_step <- unique(c(spec_step, specialty_name))
  
  read_excel("data/residents/test_scores_2.xlsx", sheet =sheet, skip = 2) -> read
  read %>% 
    filter(...1 == "All") %>% 
    t(.) %>% 
    as.data.frame(.) %>% 
    row_to_names(row_number = 1) %>% 
    mutate_at(vars(All), ~replace(., . == "-",0)) %>% 
    mutate(val = 18:27*10) %>% 
    mutate_at(1, as.numeric) %>% 
    head(.,-1) -> step_2_df
  
  dists[["USMLE STEP 2 CK Score"]][[specialty_name]] <- make_distribution(step_2_df)
  dists_raw[["USMLE STEP 2 CK Score"]][[specialty_name]]  <- make_distribution_raw(step_2_df)
  
  read %>% 
    drop_na(...1) %>% 
    select(...1, All) %>% 
    head(.,-1) %>% 
    map_df(rev) %>% 
    mutate_at(vars(All), ~replace(., . == "-",0)) %>% 
    mutate(val = 18:26*10) %>% 
    select(-...1) -> step_1_df
  
  dists[["USMLE STEP 1 Score"]][[specialty_name]] <- make_distribution(step_1_df)
  dists_raw[["USMLE STEP 1 Score"]][[specialty_name]] <- make_distribution_raw(step_1_df)
}

# read in experience distributions
experience_scores <- read_excel("data/residents/test_scores.xlsx", 
                          skip = 2)
`%notin%` <- Negate(`%in%`)
dists[["Number of research experiences"]] <- list()
dists[["Number of abstracts, presentations, and publications"]] <- list()
dists[["Number of work experiences"]] <- list()
dists[["Number of volunteer experiences"]] <- list()


experience_scores %<>% 
  tail(.,-1) %>% 
  filter(`Description of Test or Experience` %notin% c("COMLEX Level 1 Score",
                                                       "COMLEX Level 2-CE Score",
                                                       "USMLE STEP 1 Score",
                                                       "USMLE STEP 2 CK Score")) %>% 
  mutate_at(4:10, as.numeric)

spec_others <- experience_scores[[1]] %>% unique(.)

for (row in 1:nrow(experience_scores)) {
  specialty <- experience_scores[[1]][row]
  type <- experience_scores[[2]][row]
  y <- c(0, 0.1,0.25,0.5,0.75,0.9,.99,1)
  x <- c(0,
       experience_scores[["10th Percentile"]][row],
       experience_scores[["25th Percentile"]][row],
       experience_scores[["50th Percentile"]][row],
       experience_scores[["75th Percentile"]][row],
       experience_scores[["90th Percentile"]][row],
       experience_scores[["90th Percentile"]][row] + experience_scores[["Standard Deviation"]][row] * (2.323 - 1.282), #assuming normal distribution
       1000)
  approx(x,y)
  dists[[type]][[specialty]] <- approxfun(x,y) 
}

# match specialty titles
spec_dat_step <- spec_step %>% 
  as.data.frame(.)

colnames(spec_dat_step) <- "Specialty_step"

spec_dat %<>% 
  stringdist_left_join(., spec_dat_step, by = c("Specialty" = "Specialty_step"), max_dist = 30, distance_col =  "match_distance") %>% 
  .[order(.$Specialty, .$match_distance),] %>%
  .[!duplicated(.$Specialty),] 

## manually fix matches
spec_dat[spec_dat$Specialty == "Otolaryngology - Head and Neck Surgery", 2] <- "Otolaryngology"
spec_dat[spec_dat$Specialty == "Internal Medicine - Preliminary Track", 2] <- "Internal Medicine: Categorical"
spec_dat[spec_dat$Specialty == "Transitional Year", 2] <- "Internal Medicine: Categorical"
spec_dat[spec_dat$Specialty == "Interventional Radiology - Integrated", 2] <- "Radiology-Diagnostic"
spec_dat[spec_dat$Specialty == "Neurodevelopmental Disabilities", 2] <- "Neurology"

spec_dat %<>% 
  mutate(step_inexact_match = match_distance > 10)

## manually fix labeling
spec_dat[spec_dat$Specialty_step %in% c("Otolaryngology","General Surgery: Categorical"), 4] <- FALSE
spec_dat %<>% 
  mutate(step_no_match = is.na(Specialty_step)) %>% 
  select(-match_distance)


spec_dat_others <- spec_others %>% 
  as.data.frame(.)

colnames(spec_dat_others) <- "Specialty_others"


spec_dat %<>% 
  stringdist_left_join(., spec_dat_others, by = c("Specialty" = "Specialty_others"), max_dist = 30, distance_col =  "match_distance") %>% 
  .[order(.$Specialty, .$match_distance),] %>%
  .[!duplicated(.$Specialty),] 

# manually fix matches
spec_dat[spec_dat$Specialty == "Otolaryngology - Head and Neck Surgery", 5] <- "Otolaryngology"
spec_dat[spec_dat$Specialty == "Neurodevelopmental Disabilities", 5] <- "Neurology"
spec_dat[spec_dat$Specialty == "Dermatology", 5] <- NA
spec_dat[spec_dat$Specialty == "Interventional Radiology - Integrated", 5] <- NA
spec_dat[spec_dat$Specialty == "Radiation Oncology", 5] <- NA
spec_dat[spec_dat$Specialty == "Radiology - Diagnostic", 5] <- NA

spec_dat %<>% 
  mutate(others_inexact_match = match_distance > 20 & !is.na(Specialty_others))

# manually fix labeling
spec_dat[spec_dat$Specialty_step %in% c("Otolaryngology"), 6] <- FALSE
spec_dat %<>% 
  mutate(others_no_match = is.na(Specialty_others)) %>% 
  select(-match_distance)

## add in weights labels
spec_dat_weights <- PD_Survey_weights$Specialty %>% 
  unique() %>% 
  as.data.frame()

#add in weights labels
colnames(spec_dat_weights) <- "Specialty_weights"

spec_dat %<>% 
  stringdist_left_join(., spec_dat_weights, by = c("Specialty" = "Specialty_weights"), max_dist = 30, distance_col =  "match_distance") %>% 
  .[order(.$Specialty, .$match_distance),] %>%
  .[!duplicated(.$Specialty),]

## manually fix values
spec_dat[spec_dat$Specialty == "Internal Medicine - Categorical Track", 8] <- "Internal Medicine"
spec_dat[spec_dat$Specialty == "Internal Medicine - Preliminary Track", 8] <- "Internal Medicine"
spec_dat[spec_dat$Specialty == "Neurodevelopmental Disabilities", 8] <- "Neurology"
spec_dat[spec_dat$Specialty == "Otolaryngology - Head and Neck Surgery", 8] <- "Otolaryngology"
spec_dat[spec_dat$Specialty == "Pathology-Anatomic and Clinical", 8] <- "Pathology"
spec_dat[spec_dat$Specialty == "Surgery-General - Categorical Track", 8] <- "Surgery"
spec_dat[spec_dat$Specialty == "Surgery-General - Preliminary Track", 8] <- "Surgery"
spec_dat[spec_dat$Specialty == "Thoracic Surgery - Integrated", 8] <- "Surgery"

  
        