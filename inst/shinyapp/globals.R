# define global variables

library(compiler)
enableJIT(3)

if (Sys.getenv("SHINY_PORT") == "") options(shiny.maxRequestSize = 10000 * 1024^2)

user_name <- Sys.getenv("SHINYPROXY_USERNAME")
if (user_name == "") user_name <- "Anonymous"

scrs <- reactiveValues(spc = NULL)
meta <- reactiveValues(tbl = NULL)

hs <- reactiveValues(val = list())
