# source incoming data
score_step1 <- function(score, specialty_func, spec_dat_func,dists_func) {
  # print(specialty_func)
  # print(spec_dat_func$Specialty)
  spec_dist <- spec_dat_func[spec_dat_func$Specialty == specialty_func, 2]
  func <- dists_func[["USMLE STEP 1 Score"]][[spec_dist]]
  if (is.null(func)) {
    return(NA)
  } else {
    func(score) %>% 
      return(.)
  }
}

score_step2 <- function(score, specialty_func, spec_dat_func,dists_func) {
  spec_dist <- spec_dat_func[spec_dat_func$Specialty == specialty_func, 2]
  func <- dists_func[["USMLE STEP 2 CK Score"]][[spec_dist]]
  if (is.null(func)) {
    return(NA)
  } else {
    func(score) %>% 
      return(.)
  }
}
score_research <- function(score, specialty_func,spec_dat_func,dists_func) {
  spec_dist <- spec_dat_func[spec_dat_func$Specialty == specialty_func, 5]
  func <- dists_func[["Number of research experiences" ]][[spec_dist]]
  if (is.null(func)) {
    return(NA)
  } else {
    dists_func[["Number of research experiences" ]][[spec_dist]](score) %>% 
      return(.)
  }
}
score_publications <- function(score, specialty_func,spec_dat_func,dists_func) {
  spec_dist <- spec_dat_func[spec_dat_func$Specialty == specialty_func, 5]
  func <-dists_func[["Number of abstracts, presentations, and publications"]][[spec_dist]]
  if (is.null(func)) {
    return(NA)
  } else {
    func(score) %>% 
      return(.)
  }
}
score_work <- function(score, specialty_func,spec_dat_func,dists_func) {
  spec_dist <- spec_dat_func[spec_dat_func$Specialty == specialty_func, 5]
  func <- dists_func[["Number of work experiences"]][[spec_dist]]
  if (is.null(func)) {
    return(NA)
  } else {
    func(score) %>% 
      return(.)
  }
}
score_volunteer <- function(score, specialty_func,spec_dat_func,dists_func) {
  spec_dist <- spec_dat_func[spec_dat_func$Specialty == specialty_func, 5]
  func <- dists_func[["Number of volunteer experiences"]][[spec_dist]]
  if (is.null(func)) {
    return(NA)
  } else {
    func(score) %>% 
      return(.)
  }
}

score_overall <- function(scores,specialty_func,spec_dat_func,dists_func, weights_func) {
  percentiles <- c(score_step1(scores[1], specialty_func,spec_dat_func,dists_func),
             score_step2(scores[2], specialty_func, spec_dat_func,dists_func),
             score_research(scores[3], specialty_func,spec_dat_func,dists_func),
             score_publications(scores[4], specialty_func,spec_dat_func,dists_func),
             score_work(scores[5], specialty_func, spec_dat_func,dists_func),
             score_volunteer(scores[6], specialty_func, spec_dat_func,dists_func))
  spec_weights <- spec_dat_func[spec_dat_func$Specialty == specialty_func, 8]
  weight <- as.numeric(weights_func[weights_func$Specialty == spec_weights,2:ncol(weights_func)])
  vec <- weight*percentiles
  adj <- weight * !is.na(vec)
  return(sum(vec, na.rm = T)/sum(adj, na.rm = T))
}

UI_code_gen <- function() {
  for (group in filter_groups) {
    cat("<details>\n")
    cat(paste0("<summary>",group,"</summary>\n"))
    cat("```{r}\n")
    cat(paste0("uiOutput(paste0('UI_','",group,"'))"))
    cat("\n```\n")
    cat("</details>\n\n")
  }
}

UI_code_gen_2 <- function() {
  for (group in filter_groups) {
    cat("<details>\n")
    cat(paste0("<summary>",group,"</summary>\n"))
    cat("```{r, context='server'}\n")
    cat(paste0("output$UI",gsub(" ","_",group),"<- renderGroupUI('",group,"')"))
    cat("\n```\n")
    cat("```{r, context='render'}\n")
    cat(paste0("uiOutput('UI",gsub(" ","_",group),"')"))
    cat("\n```\n")
    cat("</details>\n\n")
  }
}
