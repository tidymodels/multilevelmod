
test_that('gls execution', {
  skip_if_not_installed("nlme")
  skip_on_cran()

  set.seed(2452)
  gls_mod <-
    nlme::gls(depr_score ~ week + imipramine,
              data = riesby_tr,
              correlation  = nlme::corSymm(form = ~ 1 | subject))

  set.seed(2452)
  expect_error(
    ps_mod <-
      linear_reg() %>%
      set_engine("gls", correlation  = nlme::corSymm(form = ~ 1 | subject)) %>%
      fit(depr_score ~ week + imipramine, data = riesby_tr),
    regex = NA
  )

  expect_equal(
    gls_mod$modelStruct,
    ps_mod$fit$modelStruct
  )
  expect_equal(
    coef(ps_mod$fit),
    coef(gls_mod)
  )

  gls_pred <- predict(gls_mod, riesby_te, level = 0)

  ps_pred <- predict(ps_mod, riesby_te)

  expect_equal(as.numeric(gls_pred), ps_pred$.pred)

})

test_that('mode specific package dependencies', {
  expect_identical(
    get_from_env(paste0("linear_reg", "_pkgs")) %>%
      dplyr::filter(engine == "gls", mode == "classification") %>%
      dplyr::pull(pkg),
    list()
  )

  expect_identical(
    get_from_env(paste0("linear_reg", "_pkgs")) %>%
      dplyr::filter(engine == "gls", mode == "regression") %>%
      dplyr::pull(pkg),
    list(c("nlme", "multilevelmod"))
  )
})
