# define global variables

library(compiler)
enableJIT(3)

library(shiny)
library(shinydashboard)
library(shinyjs)
library(shinyFiles)
library(shinybusy)
library(shinyalert)
library(shinydisconnect)
library(shinycssloaders)
library(fresh)
library(shinydashboardPlus)
library(shinytoastr)

library(fs)
library(zip)
library(DT)
library(ggpubr)
library(stringr)
library(RColorBrewer)
library(dplyr)
library(plotly)
library(ggplot2)
library(mongolite)

library(baseline)
library(permute)
library(hyperSpec)
library(hySpc.ggplot2)

if (Sys.getenv("SHINY_PORT") == "") options(shiny.maxRequestSize = 10000 * 1024^2)

user_name <- Sys.getenv("SHINYPROXY_USERNAME")
if (user_name == "") user_name <- "Anonymous"

theme_set(theme_bw())

set.seed(2020)

options(encoding = "UTF-8")
options(shiny.maxRequestSize = 6000 * 1024^2)

# prepare colors
cols1 <- brewer.pal(12, "Paired")
cols1 <- cols1[-11]
cols2 <- brewer.pal(8, "Dark2")
cols <- c(cols1, cols2)

options(encoding = "UTF-8")
options(spinner.type = 5, spinner.color = "#bf00ff", spinner.size = 1)

scrs <- reactiveValues(spc = NULL)
meta <- reactiveValues(tbl = NULL)

hs <- reactiveValues(val = list())

mongo_connection <- reactiveValues(obj = NULL)
dbstats <- reactiveValues(projects = NULL)
