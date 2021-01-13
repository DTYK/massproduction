#' Compute the descriptive statistics
#'
#' @param df data frame
#' @param IV Independent/Grouping Variable
#' @param DV Dependent Variable
#'
#' @return Data Frame of Mean, Standard Deviation, and N by each IV/Grouping Variable
#'
#' @export

desc <- function(df, IV, DV) {

  # Test whether df argument is present
  if (missing(df)) {
    stop("Data frame required. Please input data frame")
  }

  # Test whether IV argument is present
  if (missing(IV)) {
    stop("IV missing. Please input IV")
  }

  # Test whether DV argument is present
  if (missing(DV)) {
    stop("DV missing. Please input DV")
  }

  # Use convert_to_factor function to ensure that
  # IV is a Factor Data Type
  factor_df <- convert_to_factor(df, IV)

  # Exclude incomplete rows
  complete_df <- factor_df[complete.cases(factor_df), ]

  # Obtain the Mean of the DV by each IV
  mean_scores <- tapply(complete_df[, DV], complete_df[, IV], mean)

  # Obtain the Standard Deviation of the DV by each IV
  sd_scores <- tapply(complete_df[, DV], complete_df[, IV], sd)

  # Obtain the Sample size of the DV by each IV
  sample_size <- tapply(complete_df[, DV], complete_df[, IV], length)

  # Combine the Mean scores, SDs, and Sample sizes by each IV
  # into a data frame
  total_df <- data.frame(mean_scores, sd_scores, sample_size)

  # Return the Data Frame
  return(total_df)
}
