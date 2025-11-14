# GEE fitting function

Custom fitting function to add GEE model with cluster variable to
parsnip GEE function call.

## Usage

``` r
gee_fit(formula, data, family = gaussian, ...)
```

## Arguments

- formula:

  Normal formula but uses the `gee_formula()` internal function for the
  `id_var()` specification for clustering.

- data:

  Modeling data

- family:

  a family object: a list of functions and expressions for defining link
  and variance functions. Families supported in gee are `gaussian`,
  `binomial`, `poisson`, `Gamma`, and `quasi`; see the `glm` and
  `family` documentation. Some links are not currently available:
  `1/mu^2` and `sqrt` have not been hard-coded in the `cgee` engine at
  present. The inverse gaussian variance function is not available. All
  combinations of remaining functions can be obtained either by family
  selection or by the use of `quasi.`

- ...:

  For additional parameters

## Value

A gee object

## Details

`gee()` always prints out warnings and output even when `silent = TRUE`.
`gee_fit()` will never produce output, even if `silent = FALSE`.

Also, because of issues with the `gee()` function, a supplementary call
to [`glm()`](https://rdrr.io/r/stats/glm.html) is needed to get the rank
and QR decomposition objects so that
[`predict()`](https://rdrr.io/r/stats/predict.html) can be used.
