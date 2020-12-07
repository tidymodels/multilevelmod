# These functions are tested indirectly when the models are used. Since this
# function is executed on package startup, you can't execute them to test since
# they are already in the parsnip model database. We'll exclude them from
# coverage stats for this reason.

# nocov start

make_stan_linear_reg <- function() {

  parsnip::set_model_engine("linear_reg", "regression", "stan-glmer")
  parsnip::set_dependency("linear_reg", "stan-glmer", "rstanarm")
  parsnip::set_dependency("linear_reg", "stan-glmer", "multilevelmod")

  parsnip::set_encoding(
    model = "linear_reg",
    eng = "stan-glmer",
    mode = "regression",
    options = list(
      predictor_indicators = "none",
      compute_intercept = FALSE,
      remove_intercept = FALSE,
      allow_sparse_x = FALSE
    )
  )

  parsnip::set_fit(
    model = "linear_reg",
    eng = "stan-glmer",
    mode = "regression",
    value = list(
      interface = "formula",
      protect = c("formula", "data", "weights"),
      func = c(pkg = "rstanarm", fun = "stan_glmer"),
      defaults = list(family = rlang::expr(stats::gaussian), refresh = 0)
    )
  )

  parsnip::set_pred(
    model = "linear_reg",
    eng = "stan-glmer",
    mode = "regression",
    type = "numeric",
    value = list(
      pre = NULL,
      post =  function(results, object) {
        tibble::tibble(.pred = apply(results, 2, mean, na.rm = TRUE))
      },
      func = c(pkg = "rstanarm", fun = "posterior_predict"),
      args = list(
        object = rlang::expr(object$fit),
        newdata = rlang::expr(new_data),
        seed = rlang::expr(sample.int(10 ^ 5, 1))
      )
    )
  )

  parsnip::set_pred(
    model = "linear_reg",
    eng = "stan-glmer",
    mode = "regression",
    type = "pred_int",
    value = list(
      pre = NULL,
      post = function(results, object) {
        res <-
          tibble::tibble(
            .pred_lower =
              parsnip::convert_stan_interval(
                results,
                level = object$spec$method$pred$pred_int$extras$level
              ),
            .pred_upper =
              parsnip::convert_stan_interval(
                results,
                level = object$spec$method$pred$pred_int$extras$level,
                lower = FALSE
              ),
          )
        if (object$spec$method$pred$pred_int$extras$std_error)
          res$.std_error <- apply(results, 2, sd, na.rm = TRUE)
        res
      },
      func = c(pkg = "rstanarm", fun = "posterior_predict"),
      args =
        list(
          object = rlang::expr(object$fit),
          newdata = rlang::expr(new_data),
          seed = rlang::expr(sample.int(10^5, 1))
        )
    )
  )

  parsnip::set_pred(
    model = "linear_reg",
    eng = "stan-glmer",
    mode = "regression",
    type = "raw",
    value = list(
      pre = NULL,
      post = NULL,
      func = c(fun = "posterior_predict"),
      args = list(object = rlang::expr(object$fit), newdata = rlang::expr(new_data))
    )
  )

}

# ------------------------------------------------------------------------------

make_lme4_linear_reg <- function() {

  parsnip::set_model_engine("linear_reg", "regression", "lmer")
  parsnip::set_dependency("linear_reg", "lmer", "lme4")
  parsnip::set_dependency("linear_reg", "lmer", "multilevelmod")

  parsnip::set_encoding(
    model = "linear_reg",
    eng = "lmer",
    mode = "regression",
    options = list(
      predictor_indicators = "none",
      compute_intercept = FALSE,
      remove_intercept = FALSE,
      allow_sparse_x = FALSE
    )
  )

  parsnip::set_fit(
    model = "linear_reg",
    eng = "lmer",
    mode = "regression",
    value = list(
      interface = "formula",
      protect = c("formula", "data"),
      func = c(pkg = "lme4", fun = "lmer"),
      defaults = list()
    )
  )

  parsnip::set_pred(
    model = "linear_reg",
    eng = "lmer",
    mode = "regression",
    type = "numeric",
    value = list(
      pre = NULL,
      post =  NULL,
      func = c(fun = "predict"),
      args = list(
        object = rlang::expr(object$fit),
        newdata = rlang::expr(new_data),
        allow.new.levels = TRUE,
        na.action = rlang::expr(na.exclude),
        type = "response"
      )
    )
  )

  parsnip::set_pred(
    model = "linear_reg",
    eng = "lmer",
    mode = "regression",
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

# ------------------------------------------------------------------------------

make_gee_linear_reg <- function() {

  parsnip::set_model_engine("linear_reg", "regression", "gee")
  parsnip::set_dependency("linear_reg", "gee", "gee")
  parsnip::set_dependency("linear_reg", "gee", "multilevelmod")

  parsnip::set_encoding(
    model = "linear_reg",
    eng = "gee",
    mode = "regression",
    options = list(
      predictor_indicators = "none",
      compute_intercept = FALSE,
      remove_intercept = FALSE,
      allow_sparse_x = FALSE
    )
  )

  parsnip::set_fit(
    model = "linear_reg",
    eng = "gee",
    mode = "regression",
    value = list(
      interface = "formula",
      protect = c("formula", "data"),
      func = c(pkg = "multilevelmod", fun = "gee_fit"),
      defaults = list(family = rlang::expr(gaussian))
    )
  )

  parsnip::set_pred(
    model = "linear_reg",
    eng = "gee",
    mode = "regression",
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
    model = "linear_reg",
    eng = "gee",
    mode = "regression",
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
