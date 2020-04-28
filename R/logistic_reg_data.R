# These functions are tested indirectly when the models are used. Since this
# function is executed on package startup, you can't execute them to test since
# they are already in the parsnip model database. We'll exclude them from
# coverage stats for this reason.

# nocov

make_stan_logistic_reg <- function() {
  parsnip::set_model_engine("logistic_reg", "classification", "stan-glmer")
  parsnip::set_dependency("logistic_reg", "stan-glmer", "rstanarm")

  parsnip::set_fit(
    model = "logistic_reg",
    eng = "stan-glmer",
    mode = "classification",
    value = list(
      interface = "formula",
      protect = c("formula", "data", "weights"),
      func = c(pkg = "rstanarm", fun = "stan_glmer"),
      defaults = list(family = expr(stats::binomial), refresh = 0)
    )
  )

  parsnip::set_pred(
    model = "logistic_reg",
    eng = "stan-glmer",
    mode = "classification",
    type = "class",
    value = list(
      pre = NULL,
      post = function(x, object) {
        x <- object$fit$family$linkinv(x)
        x <- ifelse(x >= 0.5, object$lvl[2], object$lvl[1])
        unname(x)
      },
      func = c(fun = "predict"),
      args =
        list(
          object = quote(object$fit),
          newdata = quote(new_data)
        )
    )
  )

  parsnip::set_pred(
    model = "logistic_reg",
    eng = "stan-glmer",
    mode = "classification",
    type = "prob",
    value = list(
      pre = NULL,
      post = function(x, object) {
        x <- object$fit$family$linkinv(x)
        x <- tibble::tibble(v1 = 1 - x, v2 = x)
        colnames(x) <- object$lvl
        x
      },
      func = c(fun = "predict"),
      args =
        list(
          object = quote(object$fit),
          newdata = quote(new_data)
        )
    )
  )


  parsnip::set_pred(
    model = "logistic_reg",
    eng = "stan-glmer",
    mode = "classification",
    type = "raw",
    value = list(
      pre = NULL,
      post = NULL,
      func = c(fun = "predict"),
      args =
        list(
          object = quote(object$fit),
          newdata = quote(new_data)
        )
    )
  )

  parsnip::set_pred(
    model = "logistic_reg",
    eng = "stan-glmer",
    mode = "classification",
    type = "conf_int",
    value = list(
      pre = NULL,
      post = function(results, object) {
        res_2 <-
          tibble::tibble(
            lo =
              parsnip::convert_stan_interval(
                results,
                level = object$spec$method$pred$conf_int$extras$level
              ),
            hi =
              parsnip::convert_stan_interval(
                results,
                level = object$spec$method$pred$conf_int$extras$level,
                lower = FALSE
              ),
          )
        res_1 <- res_2
        res_1$lo <- 1 - res_2$hi
        res_1$hi <- 1 - res_2$lo
        res <- dplyr::bind_cols(res_1, res_2)
        lo_nms <- paste0(".pred_lower_", object$lvl)
        hi_nms <- paste0(".pred_upper_", object$lvl)
        colnames(res) <- c(lo_nms[1], hi_nms[1], lo_nms[2], hi_nms[2])

        if (object$spec$method$pred$conf_int$extras$std_error)
          res$.std_error <- apply(results, 2, sd, na.rm = TRUE)
        res
      },
      func = c(pkg = "rstanarm", fun = "posterior_linpred"),
      args =
        list(
          object = quote(object$fit),
          newdata = quote(new_data),
          transform = TRUE,
          seed = expr(sample.int(10^5, 1))
        )
    )
  )

  parsnip::set_pred(
    model = "logistic_reg",
    eng = "stan-glmer",
    mode = "classification",
    type = "pred_int",
    value = list(
      pre = NULL,
      post = function(results, object) {
        res_2 <-
          tibble::tibble(
            lo =
              parsnip::convert_stan_interval(
                results,
                level = object$spec$method$pred$pred_int$extras$level
              ),
            hi =
              parsnip::convert_stan_interval(
                results,
                level = object$spec$method$pred$pred_int$extras$level,
                lower = FALSE
              ),
          )
        res_1 <- res_2
        res_1$lo <- 1 - res_2$hi
        res_1$hi <- 1 - res_2$lo
        res <- dplyr::bind_cols(res_1, res_2)
        lo_nms <- paste0(".pred_lower_", object$lvl)
        hi_nms <- paste0(".pred_upper_", object$lvl)
        colnames(res) <- c(lo_nms[1], hi_nms[1], lo_nms[2], hi_nms[2])

        if (object$spec$method$pred$pred_int$extras$std_error)
          res$.std_error <- apply(results, 2, sd, na.rm = TRUE)
        res
      },
      func = c(pkg = "rstanarm", fun = "posterior_predict"),
      args =
        list(
          object = quote(object$fit),
          newdata = quote(new_data),
          seed = expr(sample.int(10^5, 1))
        )
    )
  )

}

make_lme4_logistic_reg <- function() {

  parsnip::set_model_engine("logistic_reg", "classification", "glmer")
  parsnip::set_dependency("logistic_reg", "glmer", "lme4")

  parsnip::set_fit(
    model = "logistic_reg",
    eng = "glmer",
    mode = "classification",
    value = list(
      interface = "formula",
      protect = c("formula", "data"),
      func = c(pkg = "lme4", fun = "glmer"),
      defaults = list(family = expr(stats::binomial))
    )
  )

  parsnip::set_pred(
    model = "logistic_reg",
    eng = "glmer",
    mode = "classification",
    type = "class",
    value = list(
      pre = NULL,
      post = prob_to_class_2,
      func = c(fun = "predict"),
      args =
        list(
          object = quote(object$fit),
          newdata = quote(new_data),
          type = "response"
        )
    )
  )

  parsnip::set_pred(
    model = "logistic_reg",
    eng = "glmer",
    mode = "classification",
    type = "prob",
    value = list(
      pre = NULL,
      post = function(x, object) {
        x <- tibble::tibble(v1 = 1 - x, v2 = x)
        colnames(x) <- object$lvl
        x
      },
      func = c(fun = "predict"),
      args =
        list(
          object = quote(object$fit),
          newdata = quote(new_data),
          type = "response"
        )
    )
  )

  parsnip::set_pred(
    model = "logistic_reg",
    eng = "glmer",
    mode = "classification",
    type = "raw",
    value = list(
      pre = NULL,
      post = NULL,
      func = c(fun = "predict"),
      args =
        list(
          object = quote(object$fit),
          newdata = quote(new_data)
        )
    )
  )

}
# nocov end
