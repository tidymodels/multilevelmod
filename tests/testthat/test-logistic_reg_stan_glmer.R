context("Hierarchical models, logistic")

library(rlang)
source(test_path("helper-logistic.R"))

q# ------------------------------------------------------------------------------

test_that('logistic stan_glmer execution', {
  skip_if_not_installed("rstanarm")
  skip_on_cran()

  # ----------------------------------------------------------------------------

  stan_cl <- call2("stan_glmer", .ns = "rstanarm", f,
                   data = expr(riesby_tr), seed = 9284, iter = 500, refresh = 0,
                   family = binomial)
  set.seed(1)
  stan_fit <- eval_tidy(stan_cl)

  # ----------------------------------------------------------------------------

  # Check for error
  expect_error({
    set.seed(1)
    ps_mod <-
      logistic_reg() %>%
      set_engine("stan-glmer", seed = 9284, iter = 500, refresh = 0) %>%
      fit(f, data = riesby_tr)
  },
  regex = NA
  )

  # ----------------------------------------------------------------------------

  # See if coefficients for both model runs are the same
  expect_equal(
    coef(ps_mod$fit),
    coef(stan_fit)
  )

  # ----------------------------------------------------------------------------

  stan_cl <- call2("posterior_predict", .ns = "rstanarm", expr(stan_fit),
                   expr(riesby_te), type = "response", seed = 1)
  set.seed(1)
  glmer_prob <- eval_tidy(stan_cl)
  glmer_prob <- apply(glmer_prob, 2, mean)
  set.seed(1)
  pa_prob <- predict(ps_mod, riesby_te, type = "prob")
  expect_equal(
    unname(glmer_prob),
    pa_prob$.pred_low,
    tolerance = .1
  )

  glmer_cls <- ifelse(glmer_prob > 0.5, "low", "high")
  glmer_cls <- factor(glmer_cls, levels = levels(riesby_tr$depressed))
  pa_cls <- predict(ps_mod, riesby_te, type = "class")
  expect_equal(
    unname(glmer_cls),
    pa_cls$.pred_class
  )

})
