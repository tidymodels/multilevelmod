
# The functions below define the model information. These access the model
# environment inside of parsnip so they have to be executed once parsnip has
# been loaded.

.onLoad <- function(libname, pkgname) {
  # This defines model functions in the parsnip model database
  make_stan_linear_reg()
  make_lme4_linear_reg()

  make_stan_logistic_reg()
  make_lme4_logistic_reg()
}


prob_to_class_2 <- function(x, object) {
  x <- ifelse(x >= 0.5, object$lvl[2], object$lvl[1])
  unname(x)
}
