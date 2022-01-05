# These functions are tested indirectly when the models are used. Since this
# function is executed on package startup, you can't execute them to test since
# they are already in the parsnip model database. We'll exclude them from
# coverage stats for this reason.

# nocov start

make_stan_poisson_reg <- function() {

  parsnip::set_model_engine("poisson_reg", "regression", "stan_glmer")
  parsnip::set_dependency("poisson_reg",
                          eng = "stan_glmer",
                          pkg = "rstanarm",
                          mode = "regression")
  parsnip::set_dependency("poisson_reg",
                          eng = "stan_glmer",
                          pkg = "multilevelmod",
                          mode = "regression")

  parsnip::set_encoding(
    model = "poisson_reg",
    eng = "stan_glmer",
    mode = "regression",
    options = list(
      predictor_indicators = "none",
      compute_intercept = FALSE,
      remove_intercept = FALSE,
      allow_sparse_x = FALSE
    )
  )

  parsnip::set_fit(
    model = "poisson_reg",
    eng = "stan_glmer",
    mode = "regression",
    value = list(
      interface = "formula",
      protect = c("formula", "data", "weights"),
      func = c(pkg = "rstanarm", fun = "stan_glmer"),
      defaults = list(family = rlang::expr(stats::poisson), refresh = 0)
    )
  )

  parsnip::set_pred(
    model = "poisson_reg",
    eng = "stan_glmer",
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
    model = "poisson_reg",
    eng = "stan_glmer",
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
    model = "poisson_reg",
    eng = "stan_glmer",
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

make_lme4_poisson_reg <- function() {

  parsnip::set_model_engine("poisson_reg", "regression", "glmer")
  parsnip::set_dependency("poisson_reg",
                          eng = "glmer",
                          pkg = "lme4",
                          mode = "regression")
  parsnip::set_dependency("poisson_reg",
                          eng = "glmer",
                          pkg = "multilevelmod",
                          mode = "regression")

  parsnip::set_encoding(
    model = "poisson_reg",
    eng = "glmer",
    mode = "regression",
    options = list(
      predictor_indicators = "none",
      compute_intercept = FALSE,
      remove_intercept = FALSE,
      allow_sparse_x = FALSE
    )
  )

  parsnip::set_fit(
    model = "poisson_reg",
    eng = "glmer",
    mode = "regression",
    value = list(
      interface = "formula",
      protect = c("formula", "data"),
      func = c(pkg = "lme4", fun = "glmer"),
      defaults = list(family = rlang::expr(stats::poisson))
    )
  )

  parsnip::set_pred(
    model = "poisson_reg",
    eng = "glmer",
    mode = "regression",
    type = "numeric",
    value = list(
      pre = reformat_lme_pred_data,
      post =  NULL,
      func = c(fun = "predict"),
      args = list(
        object = rlang::expr(object$fit),
        newdata = rlang::expr(new_data),
        allow.new.levels = TRUE,
        re.form = NA,
        na.action = rlang::expr(na.exclude),
        type = "response"
      )
    )
  )

  parsnip::set_pred(
    model = "poisson_reg",
    eng = "glmer",
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

make_gee_poisson_reg <- function() {

  parsnip::set_model_engine("poisson_reg", "regression", "gee")
  parsnip::set_dependency("poisson_reg",
                          eng = "gee",
                          pkg = "gee",
                          mode = "regression")
  parsnip::set_dependency("poisson_reg",
                          eng = "gee",
                          pkg = "multilevelmod",
                          mode = "regression")

  parsnip::set_encoding(
    model = "poisson_reg",
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
    model = "poisson_reg",
    eng = "gee",
    mode = "regression",
    value = list(
      interface = "formula",
      protect = c("formula", "data"),
      func = c(pkg = "multilevelmod", fun = "gee_fit"),
      defaults = list(family = rlang::expr(stats::poisson))
    )
  )

  parsnip::set_pred(
    model = "poisson_reg",
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
    model = "poisson_reg",
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
