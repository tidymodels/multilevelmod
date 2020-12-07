context("Generalized estimating equation models, logistic")

library(rlang)
data(riesby)
riesby$sad[riesby$depr_score < -10] <- 0
riesby$sad[riesby$depr_score >= -10] <- 1
riesby$sad <- factor(riesby$sad)

# ------------------------------------------------------------------------------

test_that('logistic gee execution', {
  skip_if_not_installed("gee")
  skip_on_cran()

  # Create function
  gee_cl <- call2(
    "gee", .ns = "gee",
    sad ~ week + imipramine, id = expr(subject), family = binomial, data = expr(riesby)
  )

  # Run both regular and GEE model
  set.seed(1234)
  gee_mod <- eval_tidy(gee_cl)
  ps_mod <-
    logistic_reg() %>%
    set_engine("gee") %>%
    fit(factor(sad) ~ week + imipramine + id_var(subject), data = riesby)

  # Check for error
  expect_error(
    ps_mod <-
      logistic_reg() %>%
      set_engine("gee") %>%
      fit(factor(sad) ~ week + imipramine + id_var(subject), data = riesby),
    regex = NA
  )

  # See if coefficients for both model runs are the same
  expect_equal(
    coef(ps_mod$fit)[2],
    coef(gee_mod)[2]
  )

  # Check predictions (not working for either model)

})
