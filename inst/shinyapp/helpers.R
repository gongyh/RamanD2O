round2 <- function(x) {round(x, digits = 2)}

unAsIs <- function(X) {
  if("AsIs" %in% class(X)) {
    class(X) <- class(X)[-match("AsIs", class(X))]
  }
  X
}
