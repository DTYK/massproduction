#' Test whether the Independent Variable is a factor variable
#'
#' @param df Data Frame
#' @param var Independent Variable (IV)
#' @return Data Frame with IV as a factor data type
#'
#' @export
test_factor <- function(df, var) {
  if (!is.factor(df[, var])) {
    df[, var] <- as.factor(df[, var])
  }
}
