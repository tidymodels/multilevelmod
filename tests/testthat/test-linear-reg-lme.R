
test_that('lme execution', {
  skip_if_not_installed("nlme")
  skip_on_cran()

  set.seed(2452)
  lme_mod <-
    nlme::lme(depr_score ~ week + imipramine,
              data = riesby_tr,
              random = ~ 1 | subject)

  set.seed(2452)
  expect_error(
    ps_mod <-
      linear_reg() %>%
      set_engine("lme", random = ~ 1 | subject) %>%
      fit(depr_score ~ week + imipramine, data = riesby_tr),
    regex = NA
  )

  expect_equal(
    nlme::fixef(ps_mod$fit),
    nlme::fixef(lme_mod)
  )
  expect_equal(
    coef(ps_mod$fit),
    coef(lme_mod)
  )

  lme_pred <- predict(lme_mod, riesby_te, level = 0)

  ps_pred <- predict(ps_mod, riesby_te)

  expect_equal(as.numeric(lme_pred), ps_pred$.pred)

})

test_that('mode specific package dependencies', {
  expect_identical(
    get_from_env(paste0("linear_reg", "_pkgs")) %>%
      dplyr::filter(engine == "lme", mode == "classification") %>%
      dplyr::pull(pkg),
    list()
  )

  expect_identical(
    get_from_env(paste0("linear_reg", "_pkgs")) %>%
      dplyr::filter(engine == "lme", mode == "regression") %>%
      dplyr::pull(pkg),
    list(c("nlme", "multilevelmod"))
  )
})
