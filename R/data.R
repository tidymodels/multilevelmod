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
