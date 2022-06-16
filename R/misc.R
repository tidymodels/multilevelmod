# Even when the predict() call uses allow.new.levels = TRUE and re.form = NA,
# we sometimes need to have a subject/random effect value in the new data that
# has to be one of the levels in the original data.

# This function rewrites the Subject column(s) with the first known level.
# This does not affect the results; in other words, the specific random effect
# contribution for the first subject does not effect the prediction.

reformat_lme_pred_data <- function(x, object) {
  random_effects <- names(lme4::ranef(object$fit))
  random_effects <- paste(random_effects, collapse = "+")
  random_effects_f <- as.formula(paste("~", random_effects))
  random_effect_cols <- all.vars(random_effects_f)
  vals <- dplyr::select(object$fit@frame, dplyr::all_of(random_effect_cols))
  vals <- vals[1,,drop = FALSE]

  for (i in random_effect_cols) {
    lvl <- levels(vals[[i]])
    x[[i]] <- factor(vals[[i]][1], levels = lvl)
  }
  x
}

