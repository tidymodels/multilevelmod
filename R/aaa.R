
# The functions below define the model information. These access the model
# environment inside of parsnip so they have to be executed once parsnip has
# been loaded.

.onLoad <- function(libname, pkgname) {
  # This defines model functions in the parsnip model database
  make_stan_linear_reg()
  make_lme4_linear_reg()
  make_gee_linear_reg()
  make_gee_logistic_reg()
  make_lme4_logistic_reg()
  make_stan_logistic_reg()
  make_gee_poisson_reg()
  make_lme4_poisson_reg()
  make_stan_poisson_reg()
  make_lme_linear_reg()
  make_gls_linear_reg()
}


