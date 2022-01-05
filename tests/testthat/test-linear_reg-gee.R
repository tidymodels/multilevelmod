
test_that('linear gee execution', {
  skip_if_not_installed("gee")
  skip_on_cran()

  # ----------------------------------------------------------------------------

  # Run both regular and GEE model
  set.seed(1234)
  gee_mod <- gee::gee(depr_score ~ week, id = riesby_tr$subject,
                      family = quasi, data = riesby_tr)
  # gee doesn't have all of the elements that are needed from prediction. Get
  # them from glm
  glm_mod <- glm(depr_score ~ week,  data = riesby_tr)
  gee_mod$rank <- glm_mod$rank
  gee_mod$qr <- glm_mod$qr
  class(gee_mod) <- c(class(gee_mod), "lm")

  # ----------------------------------------------------------------------------

  # Check for error
  expect_error(
    ps_mod <-
      linear_reg() %>%
      set_engine("gee", family = quasi) %>%
      fit(depr_score ~ week + id_var(subject), data = riesby_tr),
    regex = NA
  )

  # See if coefficients for both model runs are the same
  expect_equal(
    coef(ps_mod$fit),
    coef(gee_mod)
  )

  # Check predictions
  expect_equal(
    unname(predict(gee_mod, riesby_te)),
    predict(ps_mod, riesby_te)$.pred
  )
})

test_that('mode specific package dependencies', {
  expect_identical(
    get_from_env(paste0("linear_reg", "_pkgs")) %>%
      dplyr::filter(engine == "gee", mode == "classification") %>%
      dplyr::pull(pkg),
    list()
  )

  expect_identical(
    get_from_env(paste0("linear_reg", "_pkgs")) %>%
      dplyr::filter(engine == "gee", mode == "regression") %>%
      dplyr::pull(pkg),
    list(c("gee", "multilevelmod"))
  )
})
