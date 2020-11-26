library(shiny)
library(shinyFiles)
library(shinyjs)
library(fs)
library(DT)
library(ggpubr)
library(stringr)
library(RColorBrewer)
library(shinybusy)

library(baseline)
library(permute)
library(shinyalert)

library(dplyr)

options(encoding = "UTF-8")

options(shiny.maxRequestSize = 6000 * 1024^2)

source("globals.R")
source("helpers.R")

# prepare colors
cols1 <- brewer.pal(12, "Paired")
cols1 <- cols1[-11]
cols2 <- brewer.pal(8, "Dark2")
cols <- c(cols1, cols2)


function(input, output, session) {
  # print one dot every minute to prevent gray-out
  autoInvalidate <- reactiveTimer(60000)
  observe({
    autoInvalidate()
    cat(".")
  })

  source(file.path("server", "unzip_scrs.R"), local = TRUE)$value
  source(file.path("server", "load_meta.R"), local = TRUE)$value
  source(file.path("server", "subsample.R"), local = TRUE)$value
  source(file.path("server", "trim.R"), local = TRUE)$value
  source(file.path("server", "smooth.R"), local = TRUE)$value
  source(file.path("server", "baseline.R"), local = TRUE)$value
  source(file.path("server", "normalize.R"), local = TRUE)$value
}
