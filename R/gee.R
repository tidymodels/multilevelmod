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

#' @export
gee_fit <- function(formula, data, family = gaussian, ...) {
  f <- gee_formula(formula)
  cl <- rlang::call2("gee", .ns = "gee", f$formula, data = data,
                     id =  data[[f$id]], family = family, ...)
  res <- rlang::eval_tidy(cl)
  # Now fix some call arguments since they have data and functions
  # embedded into it.
  org_cl <- match.call()
  dat_nm <- rlang::expr_text(org_cl$data)
  res$call$id <- rlang::parse_expr(paste(dat_nm, f$id, sep = "$"))
  res$call$data <- org_cl$data
  res$call$family <- org_cl$family
  res
}
