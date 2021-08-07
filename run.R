library(shiny)
library(rmarkdown)
port <- Sys.getenv('PORT')
rmarkdown::run(
  file = "app.Rmd",
  dir = getwd(),
  shiny_args = list(host = '0.0.0.0',port = as.numeric(port))
)
