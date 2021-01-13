#' Conduct an Analysis of Variance (ANOVA)
#'
#' @param df data frame
#' @param IV Independent/Grouping Variable
#' @param DV Dependent Variable
#'
#' @return ANOVA results
#'
#' @importFrom car Anova
#'
#' @export

anova <- function(df, IV, DV) {

  linearModel <- lm(paste0(DV, " ~ ", IV), data = df)

  aov.model <- car::Anova(linearModel, type = 3)

  return(aov.model)
}
