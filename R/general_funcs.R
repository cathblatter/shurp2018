#' get text of variables from a specified text spreadsheet
#'
#' @param x a vector
#'
#' @return
#' @export
#'
#' @examples
getvartext <- function(x){

  df <- x[1]

  text <- df

  return(text)

}


#' round all numeric variables of a df according to no. of digits
#'
#' @param df a dataframe
#' @param digits a numeric value
#'
#' @return a df with rounded numeric variables
#' @export
#'
#' @examples
round_df <- function(df, digits) {
  nums <- vapply(df, is.numeric, FUN.VALUE = logical(1))

  df[,nums] <- round(df[,nums], digits = digits)

  (df)
}


#' get sum of not NAs of a vector
#'
#' @param x a vector
#'
#' @return n.valid() returns the number of fields not being NA in a vector
#' @export
#'
#' @examples
n.valid <- function(x){

  sum(is.na(x) %in% FALSE)

}

#' get number of positive answers in a vector
#'
#' @param cutoff a vector defining positive answers, default = 1 in a binary dataset
#' @param x a vector
#'
#' @return a value
#' @export
#'
#' @examples
#' SHURP_syn <- SHURP_syn
#' # this is not a dichotomized dataset, therefore you need
#' # to change the cutoff
#' n.yes(SHURP_syn$P1, cutoff = c(3,4))
#'
n.yes <- function(x, cutoff = 1){

  sum(x %in% cutoff, na.rm = T)

}

#' get percentage of positive answers in a vector
#'
#' @param x a vector
#' @param round TRUE/FALSE, default = T
#' @param digits no. of digits to round, default = 2
#' @param restrict should answers be restricted to 'NA' when n < 10 'NA', default F
#' @param cutoff a vector defining positive answers, default = 1 in a binary dataset
#'
#' @return
#' @export
#'
#' @examples
prop.yes <- function(x, cutoff = 1, round = T, digits = 2, restrict = F){

  n.valid <- n.valid(x)

  if (restrict == T) {
    n.valid <- dplyr::case_when(n.valid >= 10 ~ n.valid)}

  result <- 100*sum(x %in% cutoff, na.rm = T)/n.valid

  if (round == T) {
    round(result, digits = digits)
  } else {
    result
  }
}


#' get number of NAs in a vector
#'
#' @param x a vector
#'
#' @return
#' @export
#'
#' @examples
n.na <- function(x){

  sum(is.na(x))

}
