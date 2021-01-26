# Even when the predict() call uses allow.new.levels = TRUE and re.form = NA,
# we sometimes need to have a subject/random effect value in the new data that
# has to be one of the levels in the original data.

# This function rewrites the Subject column(s) with the first known level.
# This does not affect the results; in other words, the specific random effect
# contribution for the first subject does not effect the prediction.

reformat_lme_pred_data <- function(x, object) {
  random_effects <- names(lme4::ranef(object$fit))
  for (i in random_effects) {
    lvl <- levels(object$fit@frame[[i]])
    x[[i]] <- factor(lvl[1], levels = lvl)
  }
  x
}

