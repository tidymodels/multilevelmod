context("lme4 mixed-level model")

data("riesby")
library(rlang)

# ------------------------------------------------------------------------------

test_that('lme4 execution', {
  skip_if_not_installed("rstanarm")
  skip_on_cran()

  lme4_cl <- call2("lmer", .ns = "lme4",
                   depr_score ~ week + (week | subject),
                   data = expr(riesby))

  set.seed(2452)
  expect_warning(lme4_mod <- eval_tidy(lme4_cl), "failed to converge")

  expect_warning({
    set.seed(2452)
    expect_error(
      ps_mod <-
        linear_reg() %>%
        set_engine("lmer") %>%
        fit(depr_score ~ week + (week | subject), data = riesby),
      regex = NA
    )
  },
  "failed to converge"
  )

  expect_equal(
    coef(ps_mod$fit)$subject,
    coef(lme4_mod)$subject
  )

  lme4_pred <- predict(lme4_mod, head(riesby))

  ps_pred <- predict(ps_mod, head(riesby))

  expect_equal(unname(lme4_pred), ps_pred$.pred)

})

