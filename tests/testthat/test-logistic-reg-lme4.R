context("lme4 mixed-level logistic regression model")

data("baseball")
library(rlang)

# ------------------------------------------------------------------------------

test_that('lme4 execution', {
  skip_if_not_installed("rstanarm")
  skip_on_cran()

  lme4_cl <- call2("glmer", .ns = "lme4",
                   hit ~ hand + (1 | subject),
                   data = expr(baseball), family = expr(binomial))

  set.seed(2452)
  lme4_mod <- eval_tidy(lme4_cl)

  set.seed(2452)
  expect_error(
    ps_mod <-
      logistic_reg() %>%
      set_engine("glmer") %>%
      fit(hit ~ hand + (1 | subject), data = baseball),
    regex = NA
  )


expect_equal(
    coef(ps_mod$fit)$subject,
    coef(lme4_mod)$subject
  )

  lme4_pred <- predict(lme4_mod, baseball, type = "response")
  lme4_pred <- factor(ifelse(lme4_pred >= 0.5, "no_hit", "hit"))
  lme4_pred <- unname(lme4_pred)

  ps_pred <- predict(ps_mod, baseball)

  expect_equal(unname(lme4_pred), ps_pred$.pred_class)

  lme4_prob <- predict(lme4_mod, baseball, type = "response")

  ps_prob <- predict(ps_mod, baseball, type = "prob")

  expect_equal(1 - unname(lme4_prob), ps_prob$.pred_hit)
  expect_equal(    unname(lme4_prob), ps_prob$.pred_no_hit)

})

