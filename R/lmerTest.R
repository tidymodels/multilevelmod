make_lmerTest_linear_reg <- function() {

  parsnip::set_model_engine("linear_reg", "regression", "lmerTest")
  parsnip::set_dependency(model= "linear_reg", eng = "lmerTest", pkg = "lmerTest", mode = "regression")
  parsnip::set_dependency(model ="linear_reg", eng = "lmerTest", pkg = "multilevelmod", mode = "regression")

  parsnip::set_encoding(
    model = "linear_reg",
    eng = "lmerTest",
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
    eng = "lmerTest",
    mode = "regression",
    value = list(
      interface = "formula",
      protect = c("formula", "data", "weights"),
      func = c(pkg = "lmerTest", fun = "lmer"),
      defaults = list()
    )
  )

  parsnip::set_pred(
    model = "linear_reg",
    eng = "lmerTest",
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
    model = "linear_reg",
    eng = "lmerTest",
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
