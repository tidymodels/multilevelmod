#' @import rlang
#'
#' @importFrom parsnip set_engine fit fit_xy fit_control mars decision_tree
#' @importFrom parsnip set_new_model multi_predict update_dot_check show_fit
#' @importFrom parsnip new_model_spec null_value update_main_parameters
#' @importFrom parsnip check_final_param model_printer show_call
#' @importFrom utils globalVariables
#' @importFrom stats sd na.exclude

# ------------------------------------------------------------------------------

utils::globalVariables(
  c(
    "new_data", "object"
  )
)

# ------------------------------------------------------------------------------

# The functions below define the model information. These access the model
# environment inside of parsnip so they have to be executed once parsnip has
# been loaded.

.onLoad <- function(libname, pkgname) {
  # This defines model functions in the parsnip model database
  make_stan_linear_reg()
  make_lme4_linear_reg()
}
