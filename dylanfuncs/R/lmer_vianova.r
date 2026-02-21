#' Interpret lmer with car::Anova func
#'
#' This function calculates the p-values of a lmer model from the t-values of the fixed effects coefficients
#' @param lmermodel model
#' @keywords lmermodel
#' @export
#' @examples
#' lmer.vianova()
lmer.vianova <- function(lmermodel) {
  require(car)
  summ = data.frame(coef(summary(lmermodel))) 
  aov = Anova(lmermodel)
  peas = c(NA, aov$`Pr(>Chisq)`)
  df = cbind(summ, peas)
  print(summary(lmermodel))
  return(df)
}
