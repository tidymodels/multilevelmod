
test_that('logistic gee execution', {
  skip_if_not_installed("gee")
  skip_on_cran()

  # ----------------------------------------------------------------------------

  dat <- make_binary_data()

  # ----------------------------------------------------------------------------

  # Run both regular and GEE model
  set.seed(1234)
  gee_mod <- gee::gee(binary ~ week + imipramine, id = dat$train$subject,
                      family = binomial, data = dat$train)
  # gee doesn't have all of the elements that are needed from prediction. Get
  # them from glm
  glm_mod <- glm(binary ~ week + imipramine,  data = dat$train, family = binomial)
  gee_mod$rank <- glm_mod$rank
  gee_mod$qr <- glm_mod$qr
  class(gee_mod) <- c(class(gee_mod), "lm")

  # ----------------------------------------------------------------------------

  # Check for error
  expect_error(
    ps_mod <-
      logistic_reg() %>%
      set_engine("gee") %>%
      fit(depressed ~ week + imipramine + id_var(subject), data = dat$train),
    regex = NA
  )

  # ----------------------------------------------------------------------------

  # See if coefficients for both model runs are the same
  expect_equal(
    coef(ps_mod$fit),
    coef(gee_mod)
  )

  # ----------------------------------------------------------------------------

  gee_prob <- unname(predict(gee_mod, dat$test, type = "response"))
  pa_prob <- predict(ps_mod, dat$test, type = "prob")
  expect_equal(
    gee_prob,
    pa_prob$.pred_low
  )

  gee_cls <- ifelse(gee_prob > 0.5, "low", "high")
  gee_cls <- factor(gee_cls, levels = levels(dat$train$depressed))
  pa_cls <- predict(ps_mod, dat$test, type = "class")
  expect_equal(
    gee_cls,
    pa_cls$.pred_class
  )

})

