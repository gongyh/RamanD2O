#' @import hyperSpec
#' @import ggpubr

required_package <- function(pkg){
  if (!requireNamespace(pkg, quietly = TRUE)) {
    stop(
      pkg, " package needed to be installed before using this function. ",
      "Type this in R: install.packages('", pkg, "')"
    )
  }
}
