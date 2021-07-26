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
library(reticulate)

# load functions and objects
source('core/functions.R')
for (obj in list.files('objects/')) {
  load(paste0('objects/',obj))
}

# buils turicreate models
turicreate <- import("turicreate")
np <- import("numpy")
pd <- import("pandas")

p
turicreate$SFrame(imputed_df_list[[1]])
