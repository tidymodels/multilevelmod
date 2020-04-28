#' Measurement systems analysis data
#'
#' @details A biological assay (i.e. a lab test) was run on 56 separate samples
#' twice. The goal is to measure what percentage of the total variation in the
#' results is related to the measurement system and how much is attributable to
#' the true systematic difference (sample-to-sample).
#'
#' @name msa_data
#' @aliases msa_data
#' @docType data
#' @return \item{msa_data}{a tibble}
#'
#' @keywords datasets
#' @examples
#' data(msa_data)
#' str(msa_data)
NULL

#' Imipramine longitudinal data
#'
#' @details These data are from a longitudinal clinical trial for depression.
#' The outcome is the change in depression scores week-to-week. The `endogenous`
#' column is an indicator for whether the subject fit the WHO Depression Scale
#' classification of endogenous. The `imipramine` and `desipramine` columns are
#' measurements of plasma levels for both substances.
#'
#' @name riesby
#' @aliases riesby
#' @docType data
#' @return \item{riesby}{a tibble}
#'
#' @source Reisby, N., Gram, L.F., Bech, P. et al. Imipramine: Clinical effects
#' and pharmacokinetic variability. Psychopharmacology 54, 263-272 (1977).
#'
#'
#' @keywords datasets
#' @examples
#' data(riesby)
#' str(riesby)
NULL

#' Simulated lung cancer data
#'
#' @details A simulated data set with a binary outcome with levels "0" and "1".
#' Created by the UCLA Statistical Consulting group.
#'
#' @name hdp
#' @aliases hdp
#' @docType data
#' @return \item{hdp}{a tibble}
#'
#' @source
#' \url{https://stats.idre.ucla.edu/r/dae/mixed-effects-logistic-regression/},
#' \url{https://stats.idre.ucla.edu/stat/data/hdp.csv}
#'
#'
#'
#' @keywords datasets
#' @examples
#' data(hdp)
#' str(hdp)
NULL

#' Baseball hits
#'
#' @details From website of Jerry Brunner:
#'
#' "Right-handed basketball players take right and left-handed shots from 3
#'  locations in a different random order for each player. Hit or miss is
#'  recorded. This is a 2x3 factorial design with repeated measures on both
#'  factors: Hand they are shooting with and spot on the court."
#'
#' @name baseball
#' @aliases baseball
#' @docType data
#' @return \item{baseball}{a tibble}
#'
#' @source
#' \url{http://www.utstat.toronto.edu/~brunner/workshops/mixed/BinaryBballWithR.pdf}
#'
#' @keywords datasets
#' @examples
#' data(baseball)
#' str(baseball)
NULL

