# These functions are tested indirectly when the models are used. Since this
# function is executed on package startup, you can't execute them to test since
# they are already in the parsnip model database. We'll exclude them from
# coverage stats for this reason.

# nocov start

# ------------------------------------------------------------------------------

make_gee_logistic_reg <- function() {

  parsnip::set_model_engine("logistic_reg", "classification", "gee")
  parsnip::set_dependency("logistic_reg", "gee", "gee")
  parsnip::set_dependency("logistic_reg", "gee", "multilevelmod")

  parsnip::set_encoding(
    model = "logistic_reg",
    eng = "gee",
    mode = "classification",
    options = list(
      predictor_indicators = "none",
      compute_intercept = FALSE,
      remove_intercept = FALSE,
      allow_sparse_x = FALSE
    )
  )

  parsnip::set_fit(
    model = "logistic_reg",
    eng = "gee",
    mode = "classification",
    value = list(
      interface = "formula",
      protect = c("formula", "data"),
      func = c(pkg = "multilevelmod", fun = "gee_fit"),
      defaults = list(family = rlang::expr(binomial))
    )
  )

  parsnip::set_pred(
    model = "logistic_reg",
    eng = "gee",
    mode = "classification",
    type = "numeric",
    value = list(
      pre = NULL,
      post =  NULL,
      func = c(fun = "predict"),
      args = list(
        object = rlang::expr(object$fit),
        newdata = rlang::expr(new_data),
        type = "response"
      )
    )
  )

  parsnip::set_pred(
    model = "logistic_reg",
    eng = "gee",
    mode = "classification",
    type = "raw",
    value = list(
      pre = NULL,
      post = NULL,
      func = c(fun = "predict"),
      args = list(
        object = rlang::expr(object$fit),
        newdata = rlang::expr(new_data)
      )
    )
  )

}

# nocov end
