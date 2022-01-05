
test_that('lme4 execution', {
  skip_if_not_installed("lme4")
  skip_on_cran()

  lme4_cl <- call2("lmer", .ns = "lme4", f, data = expr(riesby_tr))

  set.seed(2452)
  lme4_mod <- eval_tidy(lme4_cl)

  set.seed(2452)
  expect_error(
    ps_mod <-
      linear_reg() %>%
      set_engine("lmer") %>%
      fit(f, data = riesby_tr),
    regex = NA
  )

  expect_equal(
    coef(ps_mod$fit)$subject,
    coef(lme4_mod)$subject
  )

  lme4_pred <- predict(lme4_mod, riesby_te, allow.new.levels = TRUE)

  ps_pred <- predict(ps_mod, riesby_te)

  expect_equal(unname(lme4_pred), ps_pred$.pred)

})


test_that('mode specific package dependencies', {
  expect_identical(
    get_from_env(paste0("linear_reg", "_pkgs")) %>%
      dplyr::filter(engine == "lmer", mode == "classification") %>%
      dplyr::pull(pkg),
    list()
  )

  expect_identical(
    get_from_env(paste0("linear_reg", "_pkgs")) %>%
      dplyr::filter(engine == "lmer", mode == "regression") %>%
      dplyr::pull(pkg),
    list(c("lme4", "multilevelmod"))
  )
})
