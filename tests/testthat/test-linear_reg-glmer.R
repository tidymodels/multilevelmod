
test_that('glmer execution', {
  skip_if_not_installed("lme4")
  skip_on_cran()

  glmer_cl <- call2("glmer", .ns = "lme4", f, data = expr(riesby_tr), family = gaussian(make.link("identity")))

  set.seed(2452)
  glmer_mod <- eval_tidy(glmer_cl)

  set.seed(2452)
  expect_error(
    ps_mod <-
      linear_reg() %>%
      set_engine("glmer", family = gaussian(make.link("identity"))) %>%
      fit(f, data = riesby_tr),
    regex = NA
  )

  expect_equal(
    coef(ps_mod$fit)$subject,
    coef(glmer_mod)$subject
  )

  glmer_pred <- predict(glmer_mod, riesby_te, allow.new.levels = TRUE)

  ps_pred <- predict(ps_mod, riesby_te)

  expect_equal(unname(glmer_pred), ps_pred$.pred)

})


test_that('mode specific package dependencies', {
  expect_identical(
    get_from_env(paste0("linear_reg", "_pkgs")) %>%
      dplyr::filter(engine == "glmer", mode == "classification") %>%
      dplyr::pull(pkg),
    list()
  )

  expect_identical(
    get_from_env(paste0("linear_reg", "_pkgs")) %>%
      dplyr::filter(engine == "glmer", mode == "regression") %>%
      dplyr::pull(pkg),
    list(c("lme4", "multilevelmod"))
  )
})
