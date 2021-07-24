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
library(doParallel)
library(miceRanger)

# source
source('preprocess/Preprocess_programs.R')
source('preprocess/Preprocess_residents.R')

# correct not truly missing values
dat$`Number of current residents  who were graduates of a joint MD-PhD program`[
  is.na(dat$`Number of current residents  who were graduates of a joint MD-PhD program`)
] <- 0

ID_columns <- 
  c("ID", 
    "Specialty", 
    "Residency program name",
    "STCOUNTYFP",
    "Metro Area Name",
    "ORGANIZATION ID (IPF)"
  )
extraneous_columns <- 
  c("Zip", 
    "Percentage of matched applicants who were members of Sigma Sigma Phi (at the time of application)",
    "City",
    "link",
    "Link suffix")

# numeric_character_columns <- c(
#   "Number of current residents  who were graduates of a joint MD-PhD program",
#   "Total # Residents on Duty",
#   "# of categorical positions offered by this program in the 2021 NRMP Main Match",
#   "# of categorical positions filled by this program in the 2021 NRMP Main Match",
#   "# of advanced positions offered by this program in the 2021 NRMP Main Match",
#   " # of advanced positions filled by this program in the 2021 NRMP Main Match",
#   
#   
# )

dat %>% 
  select(-all_of(ID_columns)) %>% 
  select(-all_of(extraneous_columns)) -> dat_impute

lapply(dat_impute, as.numeric) %>% 
  as.data.frame(.) %>% 
  colSums(na.rm = T) -> out

which(out > 0) %>%
  as.numeric(.) -> numeric_cols

dat_impute %<>% 
  mutate_at(numeric_cols, as.numeric) %>% 
  mutate_if(is.character, as.factor)

#Address colnames issues
old_colnames <- colnames(dat_impute)
new_colnames <- make.names(old_colnames)
colname_helper <- bind_cols(old_colnames, new_colnames)
colnames(dat_impute) <- new_colnames

## relocate columns in data so they are easier to join back later
dat %<>% 
  relocate(any_of(extraneous_columns),.before = 1) %>% 
  relocate(any_of(ID_columns),.before = 1)



#### Computationally expensive
set.seed(1)
miceObj <- miceRanger(dat_impute, m = 1, returnModels = F, verbose = T, maxiter = 1)
####



# Get imputed df and rank
imputed_df <- completeData(miceObj)$Dataset_1 %>% 
  bind_cols(dat[,ID_columns], dat[,extraneous_columns],.)

#fix columanes again
colnames(imputed_df) <- colnames(dat)
imputed_df_MCA <- imputed_df %>%
  select(-ID_columns,-extraneous_columns)
# Calculate rankings
## Calculate ranking weights
  
ranking_methodology <- read_excel("config/ranking_methodology.xlsx")

ranking_weights <- as.data.frame(matrix(ncol=1 + nrow(ranking_methodology), nrow = 1))
colnames(ranking_weights) <- c("Specialty",ranking_methodology$Metric)
ranking_weights_it <- ranking_weights

for (spec in spec_dat$Specialty) {
  vec <- c(spec, ranking_methodology$Weight)
  spec_weights <- spec_dat[spec_dat$Specialty == spec, 8]
  print(vec)
  #step scores
  weight_step <- sum(as.numeric(PD_Survey_weights[PD_Survey_weights$Specialty == spec_weights,2:3]))
  vec[2] <- ranking_methodology[1,"Weight"][[1]]/ranking_methodology[1,"Category weight"][[1]] * weight_step
  print(vec)
  #orther weights
  weight_others <- as.numeric(PD_Survey_weights[PD_Survey_weights$Specialty == spec_weights,4:7])
  vec[3:6] <- ranking_methodology[2:5,"Weight"][[1]]/ranking_methodology[2:5,"Category weight"][[1]] *weight_others
  ranking_weights_it[1,] <- vec
  ranking_weights <- bind_rows(ranking_weights,ranking_weights_it)
}

##R remove NA row
ranking_weights %<>% tail(.,-1) 
row.names(ranking_weights) <- NULL


rank_columns <- c("ID","Specialty", "Range of average USMLE Step 1 score of current residents",
                  "Average number of work experiences among matched applicants",
                  "Average number of volunteer experiences among matched applicants",
                  "Average number of research experiences among matched applicants",
                  "Average number of peer-reviewed publications among matched applicants",
                  "Percentage of residents who were US MD graduates",
                  "Percentage of matched applicants who were members of Alpha Omega Alpha (at the time of application)",
                  "Percentage of matched applicants who were members of Gold Humanism Honor Society (at the time of application)",
                  "# of positions offered by this program in the 2021 NRMP Main Match",
                  "# of positions filled by this program in the 2021 NRMP Main Match",
                  "# of applications submitted to this program in 2021",
                  "% of applicants interviewed by the program in 2020",
                  "Program setting","Hospital overall rating",
                  "MORT Better %","MORT Worse %",
                  "Safety Better %",
                  "Safety Worse %",
                  "READM Better %",
                  "READM Worse %",
                  "2019 NIH total funding",
                  "2019 NIH specialty funding",
                  "Program setting",
                  "Family medicine board pass rate",
                  "Internal medicine board pass rate",
                  "Pediatrics board pass rate",
                  "Surgery board pass rate")

## build ranking columns for each specialty
internal_medicine_specialties <- c("Dermatology","Internal Medicine - Categorical Track","Internal Medicine - Preliminary Track","Internal Medicine/Pediatrics",
                                   "Neurology","Pathology-Anatomic and Clinical","Radiology - Diagnostic","Transitional Year","Physical Medicine and Rehabilitation",
                                   "Radiation Oncology", "Dermatology" , "Emergency Medicine", "Psychiatry")
family_medicine_specialties <- c("Family Medicine")
pediatrics_specialties <- c("Child Neurology", "Neurodevelopmental Disabilities", "Pediatrics")



imputed_df[,rank_columns] %>% 
  mutate_at(vars("Range of average USMLE Step 1 score of current residents"), ~as.numeric(str_sub(.,-3,-1))) %>% 
  mutate(`% of positions offered in 2021 that were filled` = `# of positions filled by this program in the 2021 NRMP Main Match` / `# of positions offered by this program in the 2021 NRMP Main Match`,
         .after = `Percentage of matched applicants who were members of Gold Humanism Honor Society (at the time of application)`) %>% 
  mutate(`Ratio of 2020 applicants to 2021 positions` = `# of applications submitted to this program in 2021`/`# of positions offered by this program in the 2021 NRMP Main Match`,
         .after = `% of applicants interviewed by the program in 2020`) %>% 
  select(-`# of positions offered by this program in the 2021 NRMP Main Match`,-`# of positions filled by this program in the 2021 NRMP Main Match`,-`# of applications submitted to this program in 2021`) %>%
  mutate_at(vars("Program setting"), ~recode(., `University Hospital` = "1", Military = "1", `Affiliated Hospital` = "2", `Community Hospital` = "3", Other = "3", `Not Available` = "3")) %>% 
  mutate_at(vars("Program setting"), ~as.numeric(.)) %>% 
  select(-`Program setting.1`) %>% 
  relocate(`Program setting`,.after = `2019 NIH specialty funding`) %>% 
  mutate(`Most relevant specialty board pass rate` = ifelse(`Specialty` %in% internal_medicine_specialties, `Internal medicine board pass rate`,
                                                            ifelse(`Specialty` %in% family_medicine_specialties, `Family medicine board pass rate`,
                                                                   ifelse(`Specialty` %in% pediatrics_specialties, `Pediatrics board pass rate`,
                                                                          `Surgery board pass rate`)))) -> ranking_input_df

colnames_rank <- colnames(ranking_input_df)
ranking_list <- list()
for (spec in spec_dat$Specialty) {
  rank_df_it <- ranking_input_df %>% filter(Specialty == spec)
  # print(str(rank_df_it))
  #calculate individual score
  for (col in 3:ncol(rank_df_it)) {
    direction <- ranking_methodology$Direction[col-2]
    ecdf_func <- ecdf(rank_df_it[[col]])
    if (direction == "Pos") {
      rank_df_it[[paste0(colnames_rank[col], "_percentile")]] <- ecdf_func(rank_df_it[[col]])
    } else {
      rank_df_it[[paste0(colnames_rank[col], "_percentile")]] <- 1 - ecdf_func(rank_df_it[[col]])
    }
  }
  # claculate total ranking
  weights <- ranking_weights[ranking_weights$Specialty == spec, 2:ncol(ranking_weights)] %>% as.numeric(.)
  scores <- rank_df_it %>% select(contains("_percentile"))
  score_row <- function(row) {
    weighted_scores <- weights * as.numeric(scores[row,])
    return(sum(weighted_scores, na.rm = T))
  }
  rank_df_it %<>% 
    mutate(Overall_score = lapply(1:nrow(rank_df_it), function(i) score_row(i))) 
  overall_score_vec <- as.numeric(rank_df_it$Overall_score)
  rank_df_it[["Overall_percentile"]] <- ecdf(overall_score_vec)(overall_score_vec)
  rank_df_it[["Overall_rank"]] <- rank(overall_score_vec)
  
  #save dataframe
  ranking_list[[spec]] <- rank_df_it
}
