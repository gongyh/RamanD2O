# define global variables

library(compiler)
enableJIT(3)

if (Sys.getenv("SHINY_PORT") == "") options(shiny.maxRequestSize = 10000 * 1024^2)

scrs <- reactiveValues(spc = NULL)
meta <- reactiveValues(tbl = NULL)
hs_raw <- reactiveValues(val = NULL)
hs_cur <- reactiveValues(val = NULL)
hs_ss <- reactiveValues(val = NULL)
hs_sm <- reactiveValues(val = NULL)
hs_bl <- reactiveValues(val = NULL)
hs_nl <- reactiveValues(val = NULL)
