#' @export
runRamanD2O <- function() {
  appDir <- system.file("shinyapp", package = "RamanD2O")
  if (appDir == "") {
    stop("Could not find shinyapp directory. Try re-installing `RamanD2O`.", call. = FALSE)
  }

  shiny::runApp(appDir, display.mode = "normal")
}
