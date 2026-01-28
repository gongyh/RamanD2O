# Evaluate the importance of variables in O2PLS models

O2PLS-VIP, an approach for variable influence on projection (VIP) in
O2PLS models, is a model-based method for judging the importance of
variables. For both X and Y data blocks, it generates VIP profiles for
(i) the predictive part of the model, (ii) the orthogonal part, and
(iii) the total model.

## Usage

``` r
O2PLSvip(x, y, model)
```

## Arguments

- x:

  Training data of sequence features' relative abundances. Must have the
  exact same rows (subjects/samples) as `y`.

- y:

  Training data of metabolite relative abundances. Must have the exact
  same rows (subjects/samples) as `x`.

- model:

  List of class `"o2m"`. `x` and `y` must be the corresponding training
  data.

## Value

A list containing

- xvip:

  For the X-block, the VIP profiles for the predictive part of the
  model, the orthogonal part, the total model.

- yvip:

  For the Y-block, the VIP profiles for the predictive part of the
  model, the orthogonal part, the total model.

## Details

It generates 6 VIPO2PLS profiles in total: 1) Two VIP profiles for the
predictive components, which uncover the X- and Y-variables that are
more important for the model interpretation in relation to the variation
correlated to the Y- and X- data matrices respectively; 2) Two VIP
profiles for the orthogonal components for both the X-block and the
Y-block severally, profiles that uncover the X- and Y- variables that
are more relevant in relation to the variation uncorrelated to the Y-
and X- data matrices respectively; 3) Two VIP profiles for the total
model (i.e. including the contributions of both predictive and
orthogonal components) for both the X- and the Y- blocks severally,
these VIP profiles point at the X- and Y- variables that are more
significant for the whole model.

## References

Galindo-Prieto B, Trygg J, Geladi P. A new approach for variable
influence on projection (VIP) in O2PLS models. Chemometrics and
Intelligent Laboratory Systems 2017; 160: 110-124.
