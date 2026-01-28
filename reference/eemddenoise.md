# Denosing using EEMD method

This function smooths the input signal using the EEMD method.

## Usage

``` r
eemddenoise(
  xt,
  tt = NULL,
  cv.index,
  cv.level,
  cv.tol = 0.1^3,
  cv.maxiter = 20,
  by.imf = FALSE,
  trials.dir = "EEMD",
  noise.amp = sd(xt) * 0.1,
  trials = 5,
  nimf = 5,
  tol = sd(xt) * 0.1^2,
  max.sift = 20,
  stop.rule = "type1",
  boundary = "periodic",
  max.imf = 10
)
```

## Arguments

- xt:

  The input signal.

## Value

The smoothed signal.
