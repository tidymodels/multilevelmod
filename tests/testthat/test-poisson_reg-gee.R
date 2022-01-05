
test_that('poisson gee execution', {
  skip_if_not_installed("gee")
  skip_on_cran()

  # ----------------------------------------------------------------------------

  # Run both regular and GEE model
  set.seed(1234)
  gee_mod <- gee::gee(y ~ time + x, id = counts_tr$subject,
                      family = poisson, data = counts_tr)
  # gee doesn't have all of the elements that are needed from prediction. Get
  # them from glm
  glm_mod <- glm(y ~ time + x,  data = counts_tr, family = poisson)
  gee_mod$rank <- glm_mod$rank
  gee_mod$qr <- glm_mod$qr
  class(gee_mod) <- c(class(gee_mod), "lm")

  # ----------------------------------------------------------------------------

  # Check for error
  expect_error(
    ps_mod <-
      poisson_reg(engine = "gee") %>%
      set_engine("gee") %>%
      fit(y ~ time + x + id_var(subject), data = counts_tr),
    regex = NA
  )

  # See if coefficients for both model runs are the same
  expect_equal(
    coef(ps_mod$fit),
    coef(gee_mod)
  )

  # Check predictions
  expect_equal(
    unname(predict(gee_mod, counts_te, type = "response")),
    predict(ps_mod, counts_te)$.pred
  )
})


test_that('mode specific package dependencies', {
  expect_identical(
    get_from_env(paste0("poisson_reg", "_pkgs")) %>%
      dplyr::filter(engine == "gee", mode == "classification") %>%
      dplyr::pull(pkg),
    list()
  )

  expect_identical(
    get_from_env(paste0("poisson_reg", "_pkgs")) %>%
      dplyr::filter(engine == "gee", mode == "regression") %>%
      dplyr::pull(pkg),
    list(c("gee", "multilevelmod"))
  )
})
