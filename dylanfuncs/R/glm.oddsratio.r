#' Add ORs and 95% CIs to glm model summary
#'
#' This function takes a glm model and adds odds ratio and 95% CIs to model output
#' @param mod
#' @keywords glm, oddsratio, summary
#' @export
#' @examples
#' glm.oddsratio()
glm.oddsratio <- function(mod) {
  summ <- summary(mod)
  df <- as.data.frame(summ$coefficients)
  df$OddsRatio = exp(df$Estimate)
  df$Lower = exp(df$Estimate + (qnorm(0.025)*df$`Std. Error`))
  df$Upper = exp(df$Estimate + (qnorm(0.975)*df$`Std. Error`))
  summ$coefficients = df
  return(summ)
}
