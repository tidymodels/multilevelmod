
test_that('logistic stan_glmer execution', {
  skip_if_not_installed("rstanarm")
  skip_on_cran()

  stan_cl <- call2("stan_glmer", .ns = "rstanarm", f_bin,
                   data = expr(riesby_bin_tr), seed = 9284, iter = 500, refresh = 0,
                   family = binomial)
  set.seed(1)
  suppressWarnings(
    stan_fit <- eval_tidy(stan_cl)
  )

  set.seed(1)
  suppressWarnings(
    ps_mod <-
      logistic_reg() %>%
      set_engine("stan_glmer", seed = 9284, iter = 500, refresh = 0) %>%
      fit(f_bin, data = riesby_bin_tr)
  )

  expect_equal(
    coef(ps_mod$fit),
    coef(stan_fit)
  )

  stan_cl <- call2("posterior_predict", .ns = "rstanarm", expr(stan_fit),
                   expr(riesby_bin_te), type = "response", seed = 1)
  set.seed(1)
  glmer_prob <- eval_tidy(stan_cl)
  glmer_prob <- apply(glmer_prob, 2, mean)
  set.seed(1)
  pa_prob <- predict(ps_mod, riesby_bin_te, type = "prob")
  expect_equal(
    pa_prob$.pred_low,
    unname(glmer_prob),
    tolerance = .1
  )

  glmer_cls <- ifelse(glmer_prob > 0.5, "low", "high")
  glmer_cls <- factor(glmer_cls, levels = levels(riesby_bin_tr$depressed))
  pa_cls <- predict(ps_mod, riesby_bin_te, type = "class")
  expect_equal(
    pa_cls$.pred_class,
    unname(glmer_cls)
  )

})

test_that('mode specific package dependencies', {
  expect_identical(
    get_from_env(paste0("logistic_reg", "_pkgs")) %>%
      dplyr::filter(engine == "stan_glmer", mode == "classification") %>%
      dplyr::pull(pkg),
    list(c("rstanarm", "multilevelmod"))
  )

  expect_identical(
    get_from_env(paste0("logistic_reg", "_pkgs")) %>%
      dplyr::filter(engine == "stan_glmer", mode == "regression") %>%
      dplyr::pull(pkg),
    list()
  )
})
