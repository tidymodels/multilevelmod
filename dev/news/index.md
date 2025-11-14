# Changelog

## multilevelmod (development version)

- Switched to cli for errors
  ([\#70](https://github.com/tidymodels/multilevelmod/issues/70)).

## multilevelmod 1.0.0

CRAN release: 2022-06-17

- Maintainer change.

- Fix `lme4::lme()` for nested random effects.

- `a-difabio` added a `glmer` engine for
  [`linear_reg()`](https://parsnip.tidymodels.org/reference/linear_reg.html).

## multilevelmod 0.2.0

CRAN release: 2022-05-03

- The vignette now uses the `sleepstudy` data.

- Support for case weights has been added for
  `linear_reg(engine = "lmer")`, `logistic_reg(engine = "glmer")`, and
  `poisson_reg(engine = "glmer")`
  ([\#28](https://github.com/tidymodels/multilevelmod/issues/28)).

## multilevelmod 0.1.0

CRAN release: 2022-03-11

- First CRAN release.
