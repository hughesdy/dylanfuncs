#' Generate odds ratio outputs from glmer model
#'
#' This function takes a glmer model and outputs odds ratios and CIs
#' @param mod
#' @keywords OR
#' @export
#' @examples
#' lmeroddsratio()
lmeroddsratio <- function(mod) {
  summ <- as.data.frame(lmer.interpret(mod))%>%
    mutate(low.ci = exp(Estimate + (qnorm(0.025) * Std..Error)))%>%
    mutate(high.ci = exp(Estimate + (qnorm(0.975) * Std..Error)))%>%
    mutate(OddsRatio = exp(Estimate))
  
  return(summ)
}