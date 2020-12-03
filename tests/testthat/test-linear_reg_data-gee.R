context("Generalized estimating equation models")

data("riesby")
library(rlang)

# ------------------------------------------------------------------------------

test_that('gee execution', {
  skip_if_not_installed("gee")
  skip_on_cran()

  lme4_cl <- call2("lmer", .ns = "lme4",
                   depr_score ~ week + (week | subject),
                   data = expr(riesby))

  # Create function
  gee_cl <- call2(
    "gee", .ns = "gee",
    #depr_score ~ week + id_var(subject), data = expr(riesby)
    depr_score ~ week, id = expr(subject), data = expr(riesby)
  )

  # Run both regular and GEE model
  set.seed(1234)
  gee_mod <- eval_tidy(gee_cl)
  ps_mod <-
    linear_reg() %>%
    set_engine("gee") %>%
    fit(depr_score ~ week + id_var(subject), data = riesby)

  # Check for error
  expect_error(
    ps_mod <-
      linear_reg() %>%
      set_engine("gee") %>%
      fit(depr_score ~ week + id_var(subject), data = riesby),
    regex = NA
  )

  # See if coefficients for both model runs are the same
  expect_equal(
    coef(ps_mod$fit)[2],
    coef(gee_mod)[2]
  )

  # Check predictions (not working for either model)

})
