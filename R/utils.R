#' @import hyperSpec
#' @importFrom grDevices dev.off pdf
#' @importFrom graphics par
#' @importFrom methods new
#' @importFrom stats filter median runmed sd
#' @importFrom utils read.table write.csv write.table

required_package <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    stop(
      pkg, " package needed to be installed before using this function. ",
      "Type this in R: install.packages('", pkg, "')"
    )
  }
}
