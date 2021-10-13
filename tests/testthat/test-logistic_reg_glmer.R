context("Generalized mixed models, logistic")

library(rlang)
source(test_path("helper-logistic.R"))

q# ------------------------------------------------------------------------------

test_that('logistic glmer execution', {
  skip_if_not_installed("lme4")
  skip_on_cran()

  # ----------------------------------------------------------------------------

  # Run both regular and glmer model
  set.seed(1234)
  glmer_mod <- lme4::glmer(f, family = binomial, data = riesby_tr)

  # ----------------------------------------------------------------------------

  # Check for error
  expect_error(
    ps_mod <-
      logistic_reg() %>%
      set_engine("glmer") %>%
      fit(f, data = riesby_tr),
    regex = NA
  )

  # ----------------------------------------------------------------------------

  # See if coefficients for both model runs are the same
  expect_equal(
    coef(ps_mod$fit),
    coef(glmer_mod)
  )

  # ----------------------------------------------------------------------------

  glmer_prob <- unname(predict(glmer_mod, riesby_te, type = "response"))
  pa_prob <- predict(ps_mod, riesby_te, type = "prob")
  expect_equal(
    glmer_prob,
    pa_prob$.pred_low
  )

  glmer_cls <- ifelse(glmer_prob > 0.5, "low", "high")
  glmer_cls <- factor(glmer_cls, levels = levels(riesby_tr$depressed))
  pa_cls <- predict(ps_mod, riesby_te, type = "class")
  expect_equal(
    glmer_cls,
    pa_cls$.pred_class
  )

})

