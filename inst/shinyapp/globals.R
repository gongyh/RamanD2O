# define global variables

library(compiler)
enableJIT(3)

if (Sys.getenv("SHINY_PORT") == "") options(shiny.maxRequestSize = 10000 * 1024^2)

scrs <- reactiveValues(spc = NULL)
meta <- reactiveValues(tbl = NULL)
