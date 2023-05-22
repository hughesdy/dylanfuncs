#' Interpret lmer
#'
#' This function calculates the p-values of a lmer model from the t-values of the fixed effects coefficients
#' @param lmermodel model
#' @keywords lmermodel
#' @export
#' @examples
#' lmer.interpret()
lmer.interpret <- function(lmermodel) {
  summ = data.frame(coef(summary(lmermodel)))
  peas = c(1:nrow(summ))
  for (i in peas) {
    peas[i] = 2*(1-pnorm(abs(summ[i,3])))
  }
  df = cbind(summ, peas)
  return(df)
}
