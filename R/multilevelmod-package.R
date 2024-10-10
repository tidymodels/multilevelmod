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

## usethis namespace: start
#' @import rlang
#' @importFrom cli cli_abort
#' @importFrom parsnip set_engine fit fit_xy fit_control mars decision_tree
#' @importFrom parsnip set_new_model multi_predict update_dot_check show_fit
#' @importFrom parsnip new_model_spec null_value update_main_parameters
#' @importFrom parsnip check_final_param model_printer show_call
#' @importFrom utils globalVariables
#' @importFrom stats sd na.exclude glm as.formula
#' @importFrom stats binomial gaussian poisson terms
## usethis namespace: end
NULL

utils::globalVariables(
  c(
    "new_data", "object"
  )
)
