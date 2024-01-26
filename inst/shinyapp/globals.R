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
library(ggord)
library(stringr)
library(RColorBrewer)
library(dplyr)
library(plotly)
library(ggplot2)
library(mongolite)
library(Rtsne)
library(randomForest)
library(caret)
library(MASS)

library(baseline)
library(permute)
library(hyperSpec)
library(hySpc.ggplot2)
library(OmicsPLS)
library(mdatools)
library(reshape2)
#library(MMINP)
library(foreach)
library(parallel)
library(doParallel)

user_name <- Sys.getenv("SHINYPROXY_USERNAME")
if (user_name == "") user_name <- "Anonymous"

theme_set(theme_bw())

set.seed(2023)

unlink(paste0(tempdir(),"/*"),recursive = T, force = T)

options(encoding = "UTF-8")
options(shiny.maxRequestSize = 8 * 1024^3) # 8 GB
options(spinner.type = 5, spinner.color = "#bf00ff", spinner.size = 1)

scrs <- reactiveValues(spc = NULL)
meta <- reactiveValues(tbl = NULL)

hs <- reactiveValues(val = list())

mongo_connection <- reactiveValues(obj = NULL)
dbstats <- reactiveValues(projects = NULL)

ml <- reactiveValues(results = NULL)
result <- reactiveValues(predict = NULL, prepare = NULL)
ig <- reactiveValues(upload_X = NULL, upload_Y = NULL)
