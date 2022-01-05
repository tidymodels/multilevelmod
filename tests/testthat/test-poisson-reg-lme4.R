
test_that('lme4 execution', {
  skip_if_not_installed("rstanarm")
  skip_on_cran()

  lme4_cl <- call2("lmer", .ns = "lme4",
                   f_counts,
                   data = expr(counts_tr))

  set.seed(2452)
  lme4_mod <- eval_tidy(lme4_cl)

  set.seed(2452)
  expect_error(
    ps_mod <-
      linear_reg() %>%
      set_engine("lmer") %>%
      fit(f_counts, data = counts_tr),
    regex = NA
  )

  expect_equal(
    coef(ps_mod$fit)$subject,
    coef(lme4_mod)$subject
  )

  lme4_pred <- predict(lme4_mod, counts_te, allow.new.levels = TRUE)

  ps_pred <- predict(ps_mod, counts_te)

  expect_equal(unname(lme4_pred), ps_pred$.pred)

})


test_that('mode specific package dependencies', {
  expect_identical(
    get_from_env(paste0("poisson_reg", "_pkgs")) %>%
      dplyr::filter(engine == "glmer", mode == "classification") %>%
      dplyr::pull(pkg),
    list()
  )

  expect_identical(
    get_from_env(paste0("poisson_reg", "_pkgs")) %>%
      dplyr::filter(engine == "glmer", mode == "regression") %>%
      dplyr::pull(pkg),
    list(c("lme4", "multilevelmod"))
  )
})
