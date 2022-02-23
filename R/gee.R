# Both `gee:gee(a)` and `gee:geepack()` specifies the id/cluster variable using an argument 'id' that
# requires a vector. parsnip doesn't work that way so we enable this model to
# be fit using a artificial function called `id_var()` to be used in the
# formula. So, in the original package, the call would look like:
#
# gee(breaks ~ tension, id = wool, data = warpbreaks, corstr = "exchangeable")
#
# we use
#
# gee_fit(breaks ~ tension + id_var(wool), data = warpbreaks,
#         corstr = "exchangeable")
#
# gee_formula() parses this formula, pulls out the id variable and fixes
# the formula (= no id term). gee_fit() uses this to fit the model.


#' @description Function to add single clustering variable for GEE
#' @noRd
gee_formula <- function(f) {
  cl <- match.call()
  trms <- terms(f, specials = "id_var")
  form_terms <- attr(trms, "variables")
  id_ind <- attr(trms,"specials")$id_var + 1
  # check length
  if (length(id_ind) != 1) {
    rlang::abort(
      paste(
        "There should be a single 'id' column specified using the `id_vars()`",
        "function (e.g. `y ~ x + id_vars(id_col)`"
        )
    )
  }
  # find column with id variable
  id_expr <- form_terms[[id_ind]]
  id_var <- all.vars(id_expr)


  # repair formula: get predictors and remake
  rhs <- form_terms[-c(1:2, id_ind)]
  if (length(rhs) == 0) {
    rhs <- rlang::expr(1)
  } else if (length(rhs) > 1) {
    rhs <- purrr::reduce(rhs, function(l, r) rlang::expr(!!l + !!r))
  } else {
    rhs <- rlang::expr(!!rhs[[1]])
  }
  f[[3]] <- rhs
  list(formula = f, id = id_var)
}

#' @title GEE fitting function
#' @description Custom fitting function to add GEE model with cluster
#'   variable to parsnip GEE function call.
#' @param formula Normal formula but uses the `gee_formula()` internal
#'  function for the `id_var()` specification for clustering.
#' @param data Modeling data
#' @param family a family object: a list of functions and expressions for
#'  defining link and variance functions. Families supported in gee are
#'  `gaussian`, `binomial`, `poisson`, `Gamma`, and `quasi`; see the `glm` and
#'  `family` documentation. Some links are not currently available: `1/mu^2` and
#'  `sqrt` have not been hard-coded in the `cgee` engine at present. The inverse
#'  gaussian variance function is not available. All combinations of remaining
#'  functions can be obtained either by family selection or by the use of
#'  `quasi.`
#' @param ... For additional parameters
#' @details `gee()` always prints out warnings and output even when
#' `silent = TRUE`. `gee_fit()` will never produce output, even if
#' `silent = FALSE`.
#'
#' Also, because of issues with the `gee()` function, a supplementary call to
#' `glm()` is needed to get the rank and QR decomposition objects so that
#' `predict()` can be used.
#' @return A gee object
#' @keywords internal
#' @export
gee_fit <- function(formula, data, family = gaussian, ...) {
  f <- gee_formula(formula)
  id_sym <- f$id
  id_sym <- rlang::expr(data[[!!id_sym]])
  cl <- rlang::call2("gee", .ns = "gee", as.formula(f$formula), data = rlang::expr(data),
                     id = data[[f$id]], family = rlang::expr(family),
                     ...)

  # While undocumented in `gee()`, binomial data should be binary
  # (unlike `glm()`).
  y_name <- deparse(f$formula[[2]])
  if (is.factor(data[[y_name]])) {
    lvl <- levels(data[[y_name]])
    data[[y_name]] <- ifelse(data[[y_name]] == lvl[2], 1, 0)
  }

  withr::with_output_sink(
    tempfile(),
    withr::with_message_sink(
      tempfile(),
      res <- rlang::eval_tidy(cl)
    )
  )

  # gee() objects inherit from glm but do not have `qr` or rank` elements. For
  # this reason, predict() does not work since `predict.lm()` needs it. We do a
  # hopefully quick fit with `glm()` to get those objects.
  tmp_glm <- try(glm(f$formula, data = data, family = family), silent = TRUE)
  if (inherits(tmp_glm, "glm")) {
    res$rank <- tmp_glm$rank
    res$qr <- tmp_glm$qr
  } else {
    rlang::abort("Cannot compute the rank of the design matrix.")
  }
  # Now to avoid a warning about "calling predict.lm(<fake-lm-object>)"
  class(res) <- c(class(res), "lm")

  # Now fix some call arguments since they have data and functions
  # embedded into it.
  org_cl <- match.call()
  dat_nm <- rlang::expr_text(org_cl$data)
  res$call$id <- rlang::parse_expr(paste(dat_nm, f$id, sep = "$"))
  res$call$data <- org_cl$data
  res$call$family <- org_cl$family
  res
}
