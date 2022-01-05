
test_that('stan_glm execution', {
  skip_if_not_installed("rstanarm")
  skip_on_cran()

  stan_cl <- call2("stan_glmer", .ns = "rstanarm",
                   depr_score ~ week + (week | subject),
                   data = expr(riesby), seed = 9284, iter = 500, refresh = 0)

  set.seed(2452)
  expect_warning(stan_mod <- eval_tidy(stan_cl), "ESS")

  expect_warning({
    set.seed(2452)
    expect_error(
      ps_mod <-
        linear_reg() %>%
        set_engine("stan_glmer", seed = 9284, refresh = 0, iter = 500) %>%
        fit(depr_score ~ week + (week | subject), data = riesby),
      regex = NA
    )
  },
  "ESS"
  )

  expect_equal(
    coef(ps_mod$fit)$subject,
    coef(stan_mod)$subject
  )

  pred_cl <- call2("posterior_predict", .ns = "rstanarm",
                   stan_mod, head(riesby))

  set.seed(124321)
  stan_post <- eval_tidy(pred_cl)
  stan_pred <- unname(apply(stan_post, 2, mean))
  stan_pi_lower <- unname(apply(stan_post, 2, quantile, probs = 0.05))
  stan_pi_upper <- unname(apply(stan_post, 2, quantile, probs = 0.95))

  set.seed(124321)
  ps_pred <- predict(ps_mod, head(riesby))

  expect_equal(stan_pred, ps_pred$.pred, tolerance = 0.1)

  set.seed(124321)
  ps_pred_int <- predict(ps_mod, head(riesby), level = 0.90, type = "pred_int")

  expect_equal(stan_pi_lower, ps_pred_int$.pred_lower, tolerance = 0.1)
  expect_equal(stan_pi_upper, ps_pred_int$.pred_upper, tolerance = 0.1)

})

test_that('mode specific package dependencies', {
  expect_identical(
    get_from_env(paste0("linear_reg", "_pkgs")) %>%
      dplyr::filter(engine == "stan_glmer", mode == "classification") %>%
      dplyr::pull(pkg),
    list()
  )

  expect_identical(
    get_from_env(paste0("linear_reg", "_pkgs")) %>%
      dplyr::filter(engine == "stan_glmer", mode == "regression") %>%
      dplyr::pull(pkg),
    list(c("rstanarm", "multilevelmod"))
  )
})
