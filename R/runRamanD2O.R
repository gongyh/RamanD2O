#' Run RamanD2O Shiny App
#'
#' The entry point of shiny app.
#'
#' The source code is in the `shinyapp` subdirectory.
#'
#' @export
runRamanD2O <- function() {
  appDir <- system.file("shinyapp", package = "RamanD2O")
  if (appDir == "") {
    stop("Could not find shinyapp directory. Try re-installing `RamanD2O`.", call. = FALSE)
  }

  library(RamanD2O)
  shiny::runApp(appDir, display.mode = "normal")
}
