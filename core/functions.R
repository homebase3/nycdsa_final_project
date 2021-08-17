# source incoming data
score_step1 <- function(score, specialty_func, spec_dat_func, dists_func) {
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

score_step2 <- function(score, specialty_func, spec_dat_func, dists_func) {
  spec_dist <- spec_dat_func[spec_dat_func$Specialty == specialty_func, 2]
  func <- dists_func[["USMLE STEP 2 CK Score"]][[spec_dist]]
  if (is.null(func)) {
    return(NA)
  } else {
    func(score) %>%
      return(.)
  }
}
score_research <- function(score, specialty_func, spec_dat_func, dists_func) {
  spec_dist <- spec_dat_func[spec_dat_func$Specialty == specialty_func, 5]
  func <- dists_func[["Number of research experiences"]][[spec_dist]]
  if (is.null(func)) {
    return(NA)
  } else {
    dists_func[["Number of research experiences"]][[spec_dist]](score) %>%
      return(.)
  }
}
score_publications <- function(score, specialty_func, spec_dat_func, dists_func) {
  spec_dist <- spec_dat_func[spec_dat_func$Specialty == specialty_func, 5]
  func <- dists_func[["Number of abstracts, presentations, and publications"]][[spec_dist]]
  if (is.null(func)) {
    return(NA)
  } else {
    func(score) %>%
      return(.)
  }
}
score_work <- function(score, specialty_func, spec_dat_func, dists_func) {
  spec_dist <- spec_dat_func[spec_dat_func$Specialty == specialty_func, 5]
  func <- dists_func[["Number of work experiences"]][[spec_dist]]
  if (is.null(func)) {
    return(NA)
  } else {
    func(score) %>%
      return(.)
  }
}
score_volunteer <- function(score, specialty_func, spec_dat_func, dists_func) {
  spec_dist <- spec_dat_func[spec_dat_func$Specialty == specialty_func, 5]
  func <- dists_func[["Number of volunteer experiences"]][[spec_dist]]
  if (is.null(func)) {
    return(NA)
  } else {
    func(score) %>%
      return(.)
  }
}

score_overall <- function(scores, specialty_func, spec_dat_func, dists_func, weights_func) {
  percentiles <- c(
    score_step1(scores[1], specialty_func, spec_dat_func, dists_func),
    score_step2(scores[2], specialty_func, spec_dat_func, dists_func),
    score_research(scores[3], specialty_func, spec_dat_func, dists_func),
    score_publications(scores[4], specialty_func, spec_dat_func, dists_func),
    score_work(scores[5], specialty_func, spec_dat_func, dists_func),
    score_volunteer(scores[6], specialty_func, spec_dat_func, dists_func)
  )
  spec_weights <- spec_dat_func[spec_dat_func$Specialty == specialty_func, 8]
  weight <- as.numeric(weights_func[weights_func$Specialty == spec_weights, 2:ncol(weights_func)])
  vec <- weight * percentiles
  adj <- weight * !is.na(vec)
  return(sum(vec, na.rm = T) / sum(adj, na.rm = T))
}

UI_code_gen <- function() {
  for (group in filter_groups) {
    cat("<details>\n")
    cat(paste0("<summary>", group, "</summary>\n"))
    cat("```{r}\n")
    cat(paste0("uiOutput(paste0('UI_','", group, "'))"))
    cat("\n```\n")
    cat("</details>\n\n")
  }
}

UI_code_gen_2 <- function() {
  for (group in filter_groups) {
    cat("<details>\n")
    cat(paste0("<summary>", group, "</summary>\n"))
    cat("```{r, context='server'}\n")
    cat(paste0("output$UI", gsub(" ", "_", group), "<- renderGroupUI('", group, "')"))
    cat("\n```\n")
    cat("```{r, context='render'}\n")
    cat(paste0("uiOutput('UI", gsub(" ", "_", group), "')"))
    cat("\n```\n")
    cat("</details>\n\n")
  }
}

UI_code_gen_3 <- function() {
  for (group in filter_groups) {
    cat("<details>\n")
    cat(paste0("<summary>", group, "</summary>\n"))
    cat("```{r, context='render'}\n")
    UI_helper_group(group)
    cat("\n```\n")
    cat("</details>\n\n")
  }
}

wrap_title <- function(vec) {
  sapply(vec, function(i) paste(str_wrap(i, width = 30), sep = "\n")) %>%
    as.character(.) %>%
    gsub(., pattern = "\n", replacement = "<br/>") %>%
    return(.)
}

UI_helper_func <- function(var, min_default = 0, max_default = 1, choices_default = c("0", "1")) {
  if (!is.null(classes[[var]])) {
    if (as.character(classes[[var]]) == "numeric") {
      paste0("sliderInput('", gsub(" ", "_", var), "','", var, "',", min_default, ",", 1, ", c(0,1),round=T)\n") %>%
        cat(.)
    } else {
      paste0("pickerInput('", gsub(" ", "_", var), "','", var, "',", "choices= c(", paste(choices_default, collapse = ", "), "), multiple = T, options=list(`liveSearch` = T, `actions-box` = TRUE,size = 10,`selected-text-format` = 'count > 3'))\n") %>%
        cat(.)
    }
  }
}
UI_helper_group <- function(group) {
  filters %>%
    filter(Class %in% group) %>%
    dplyr::select(Colname) %>%
    .[[1]] -> vars_
  for (var in vars_) {
    cat(UI_helper_func(var))
  }
}
UI_update_helper <- function() {
  for (group in filter_groups) {
    filters %>%
      filter(Class %in% group) %>%
      dplyr::select(Colname) %>%
      .[[1]] -> vars_
    for (var in vars_) {
      if (!is.null(classes[[var]])) {
        if (as.character(classes[[var]]) == "numeric") {
          cat(paste0(
            "updateSliderInput(session,inputId='", gsub(" ", "_", var), "', min=mins[['", var, "']], max= maxs[['",
            var, "']], value=c(mins[['", var, "']],maxs[['", var, "']]))\n"
          ))
        } else {
          cat(paste0("updatePickerInput(session, inputId ='", gsub(" ", "_", var), "', choices=vals[['", var, "']],selected = vals[['", var, "']])\n"))
        }
      }
    }
  }
}
