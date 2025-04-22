test_that('lmerTest execution', {
  skip_if_not_installed("lmerTest")
  skip_on_cran()

  set.seed(2452)
  lmer_mod <-
    lmerTest::lmer(depr_score ~ week + imipramine +  (1 | subject),
                   data = riesby_tr)

  lmer_spec <-
    linear_reg() %>%
    set_engine("lmerTest")

  set.seed(2452)
  expect_error(
    ps_mod <-
      lmer_spec %>%
      fit(depr_score ~ week + imipramine + (1|subject), data = riesby_tr),
    regex = NA
  )

  expect_equal(
    lme4::fixef(ps_mod$fit),
    lme4::fixef(lmer_mod)
  )
  expect_equal(
    coef(ps_mod$fit),
    coef(lmer_mod)
  )

  lmer_pred <- predict(lmer_mod, riesby_te, allow.new.levels = T) #level = 0,

  ps_pred <- predict(ps_mod, riesby_te)

  expect_equal(as.numeric(lmer_pred), ps_pred$.pred)

})

test_that('mode specific package dependencies', {
  expect_identical(
    get_from_env(paste0("linear_reg", "_pkgs")) %>%
      dplyr::filter(engine == "lmerTest", mode == "regression") %>%
      dplyr::pull(pkg),
    list(c("lmerTest", "multilevelmod"))
  )
})
