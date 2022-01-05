
test_that('logistic gee execution', {
  skip_if_not_installed("gee")
  skip_on_cran()

  # ----------------------------------------------------------------------------

  # Run both regular and GEE model
  set.seed(1234)
  gee_mod <- gee::gee(binary ~ week + imipramine, id = riesby_bin_tr$subject,
                      family = binomial, data = riesby_bin_tr)
  # gee doesn't have all of the elements that are needed from prediction. Get
  # them from glm
  glm_mod <- glm(binary ~ week + imipramine,  data = riesby_bin_tr, family = binomial)
  gee_mod$rank <- glm_mod$rank
  gee_mod$qr <- glm_mod$qr
  class(gee_mod) <- c(class(gee_mod), "lm")

  # ----------------------------------------------------------------------------

  # Check for error
  expect_error(
    ps_mod <-
      logistic_reg() %>%
      set_engine("gee") %>%
      fit(depressed ~ week + imipramine + id_var(subject), data = riesby_bin_tr),
    regex = NA
  )

  # ----------------------------------------------------------------------------

  # See if coefficients for both model runs are the same
  expect_equal(
    coef(ps_mod$fit),
    coef(gee_mod)
  )

  # ----------------------------------------------------------------------------

  gee_prob <- unname(predict(gee_mod, riesby_bin_te, type = "response"))
  pa_prob <- predict(ps_mod, riesby_bin_te, type = "prob")
  expect_equal(
    gee_prob,
    pa_prob$.pred_low
  )

  gee_cls <- ifelse(gee_prob > 0.5, "low", "high")
  gee_cls <- factor(gee_cls, levels = levels(riesby_bin_tr$depressed))
  pa_cls <- predict(ps_mod, riesby_bin_te, type = "class")
  expect_equal(
    gee_cls,
    pa_cls$.pred_class
  )

})

test_that('mode specific package dependencies', {
  expect_identical(
    get_from_env(paste0("logistic_reg", "_pkgs")) %>%
      dplyr::filter(engine == "gee", mode == "classification") %>%
      dplyr::pull(pkg),
    list(c("gee", "multilevelmod"))
  )

  expect_identical(
    get_from_env(paste0("logistic_reg", "_pkgs")) %>%
      dplyr::filter(engine == "gee", mode == "regression") %>%
      dplyr::pull(pkg),
    list()
  )
})
