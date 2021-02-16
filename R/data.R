#' SHURP_syn.
#'
#' Synthesized data from the SHURP2013-study.
#'
#' @format A data frame with answers from 5311 nurses in  69 variables. More
#' information is found in the vignette.
"SHURP_syn"

#' QC-Item text data in all languages
#'
#' A dataset containing corresponding item texts for QC (including cutoffs for
# "pct agreement") in all languages
#'
#' @format A data frame with 2 rows and 168 variables:
#' \describe{
#'   \item{Version}{language, french or german}
#'   \item{C1}{Text to item C1}
#'   ...
#' }
#' @source SHURP 2018 study
"QC_text"


#' QE-Item text data in all languages
#'
#' A dataset containing corresponding item texts for QE (including cutoffs for
# "pct agreement") in all languages
#'
#' @format A data frame with 2 rows and 91 variables:
#' \describe{
#'   \item{Version}{language, french or german}
#'   \item{E1}{Text to item E1}
#'   ...
#' }
#' @source SHURP 2018 study
"QE_text"


#' Item labels used for params reporting
#'
#' A dataset containing item labels
#'
#' @format A data frame with 2 rows and 91 variables:
#' \describe{
#'   \item{var_name}{indicating what label}
#'   \item{german}{german text to label}
#'   \item{french}{french text to label}
#' }
#' @source SHURP 2018 study
"label_data"
