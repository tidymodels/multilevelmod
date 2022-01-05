
test_that('logistic glmer execution', {
  skip_if_not_installed("lme4")
  skip_on_cran()

  # ----------------------------------------------------------------------------

  # Run both regular and glmer model
  set.seed(1234)
  glmer_mod <- lme4::glmer(f_bin, family = binomial, data = riesby_bin_tr)

  # ----------------------------------------------------------------------------

  # Check for error
  expect_error(
    ps_mod <-
      logistic_reg() %>%
      set_engine("glmer") %>%
      fit(f_bin, data = riesby_bin_tr),
    regex = NA
  )

  # ----------------------------------------------------------------------------

  # See if coefficients for both model runs are the same
  expect_equal(
    coef(ps_mod$fit),
    coef(glmer_mod)
  )

  # ----------------------------------------------------------------------------

  glmer_prob <- unname(predict(glmer_mod, riesby_bin_te, type = "response",
                               allow.new.levels = TRUE))
  pa_prob <- predict(ps_mod, riesby_bin_te, type = "prob")
  expect_equal(
    glmer_prob,
    pa_prob$.pred_low
  )

  glmer_cls <- ifelse(glmer_prob > 0.5, "low", "high")
  glmer_cls <- factor(glmer_cls, levels = levels(riesby_bin_tr$depressed))
  pa_cls <- predict(ps_mod, riesby_bin_te, type = "class")
  expect_equal(
    glmer_cls,
    pa_cls$.pred_class
  )
})

test_that('mode specific package dependencies', {
  expect_identical(
    get_from_env(paste0("logistic_reg", "_pkgs")) %>%
      dplyr::filter(engine == "glmer", mode == "classification") %>%
      dplyr::pull(pkg),
    list(c("lme4", "multilevelmod"))
  )

  expect_identical(
    get_from_env(paste0("logistic_reg", "_pkgs")) %>%
      dplyr::filter(engine == "glmer", mode == "regression") %>%
      dplyr::pull(pkg),
    list()
  )
})
