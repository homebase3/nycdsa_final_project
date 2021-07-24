# source incoming data
source('preprocess/Preprocess_residents.R')

score_step1 <- function(score, specialty_func) {
  spec_dist <- spec_dat[spec_dat$Specialty == specialty_func, 2]
  func <- dists[["USMLE STEP 1 Score"]][[spec_dist]]
  if (is.null(func)) {
    return(NA)
  } else {
    func(score) %>% 
      return(.)
  }
}

score_step2 <- function(score, specialty_func) {
  spec_dist <- spec_dat[spec_dat$Specialty == specialty_func, 2]
  func <- dists[["USMLE STEP 2 CK Score"]][[spec_dist]]
  if (is.null(func)) {
    return(NA)
  } else {
    func(score) %>% 
      return(.)
  }
}
score_research <- function(score, specialty_func) {
  spec_dist <- spec_dat[spec_dat$Specialty == specialty_func, 5]
  func <- dists[["Number of research experiences" ]][[spec_dist]]
  if (is.null(func)) {
    return(NA)
  } else {
    dists[["Number of research experiences" ]][[spec_dist]](score) %>% 
      return(.)
  }
}
score_publications <- function(score, specialty_func) {
  spec_dist <- spec_dat[spec_dat$Specialty == specialty_func, 5]
  func <-dists[["Number of abstracts, presentations, and publications"]][[spec_dist]]
  if (is.null(func)) {
    return(NA)
  } else {
    func(score) %>% 
      return(.)
  }
}
score_work <- function(score, specialty_func) {
  spec_dist <- spec_dat[spec_dat$Specialty == specialty_func, 5]
  func <- dists[["Number of work experiences"]][[spec_dist]]
  if (is.null(func)) {
    return(NA)
  } else {
    func(score) %>% 
      return(.)
  }
}
score_volunteer <- function(score, specialty_func) {
  spec_dist <- spec_dat[spec_dat$Specialty == specialty_func, 5]
  func <- dists[["Number of volunteer experiences"]][[spec_dist]]
  if (is.null(func)) {
    return(NA)
  } else {
    func(score) %>% 
      return(.)
  }
}

score_overall <- function(scores,specialty_func) {
  percentiles <- c(score_step1(scores[1], specialty_func),
             score_step2(scores[2], specialty_func),
             score_research(scores[3], specialty_func),
             score_publications(scores[4], specialty_func),
             score_work(scores[5], specialty_func),
             score_volunteer(scores[6], specialty_func))
  spec_weights <- spec_dat[spec_dat$Specialty == specialty_func, 8]
  weight <- as.numeric(PD_Survey_weights[PD_Survey_weights$Specialty == spec_weights,2:ncol(PD_Survey_weights)])
  vec <- weight*percentiles
  adj <- weight * !is.na(vec)
  return(sum(vec, na.rm = T)/sum(adj, na.rm = T))
}
