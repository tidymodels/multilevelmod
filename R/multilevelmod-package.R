#' parsnip methods for hierarchical models
#'
#' multilevelmod allows users to use the \pkg{parsnip} package to fit certain
#'  hierarchical models (e.g., linear, logistic, and Poisson regression). The
#'  package relies on the formula method to specify the random effects.
#'
#' As an example, the package includes simulated longitudinal data where
#'  subjects were measured over time points. The outcome was the number of
#'  counts and the predictors are the time point as well as an additional
#'  numeric covariate.
#'
#' We can fit the model using [lme4::glmer()]:
#'
#' ```{r, child = "man/rmd/example.Rmd", error = FALSE}
#' ```
#' @keywords internal
"_PACKAGE"

NULL
