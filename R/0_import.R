#' @import rlang
#'
#' @importFrom parsnip set_engine fit fit_xy fit_control mars decision_tree
#' @importFrom parsnip set_new_model multi_predict update_dot_check show_fit
#' @importFrom parsnip new_model_spec null_value update_main_parameters
#' @importFrom parsnip check_final_param model_printer show_call
#' @importFrom utils globalVariables
#' @importFrom stats sd na.exclude glm as.formula
#' @importFrom stats binomial gaussian poisson terms


# ------------------------------------------------------------------------------

utils::globalVariables(
  c(
    "new_data", "object"
  )
)
