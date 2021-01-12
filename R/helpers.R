#' Convert variable to a Factor Data Type
#'
#' Test whether a variable is a Factor Data Type. If it isn't,
#' convert it to a Factor Data Type and return the data frame
#' back to the user. If it is, do nothing and return the data
#' frame as it is to the user.
#'
#' @param df Data Frame
#' @param var Independent Variable (IV)
#' @return Data Frame with IV as a factor data type
#'
#' @export
convert_to_factor <- function(df, var) {

  # Test whether var is a factor variable. If it is not
  # a factor variable, convert it into a factor variable
  if (!is.factor(df[, var])) {

    # Force var into a Factor Data Type
    df[, var] <- as.factor(df[, var])

    # Indicate to user that var is hardcoded into a
    # Factor Data Type
    print(paste0("Coercing ", var, " into a Factor"))

    # Return data frame
    return(df)
  }

  # If var is a factor variable, do nothing. Return the
  # data frame as it is
  else {
    return(df)
  }
}
