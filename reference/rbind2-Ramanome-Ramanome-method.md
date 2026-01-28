# \#' Plot method for Ramanome objects \#' \#' @param x The Ramanome object \#' @param y Character specifying the plot type \#' @importFrom hyperSpec plot \#' @importFrom hyperSpec print \#' @importFrom methods setMethod

setMethod("plot", signature(x = "Ramanome", y = "character"),
function(x, y, ...) tmp \<- hyperSpec::plot(x, y, ...) if (is(tmp,
"trellis")) hyperSpec::print(tmp) invisible(tmp) ) Combine Ramanome
objects by row binding

## Usage

``` r
# S4 method for class 'Ramanome,Ramanome'
rbind2(x, y)
```

## Arguments

- x:

  The first Ramanome object

- y:

  The second Ramanome object
