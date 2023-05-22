#' Add 95% CIs to lmer.interpret output
#'
#' This function takes a lmer mod and appends 95% CIs to lmer.interpret output
#' @param mod
#' @keywords lmer, confidence intervals
#' @export
#' @examples
#' lmer.interpret.ci()
lmer.interpret.ci <- function(mod) {
  library(devtools)
  devtools::document('/path/to/dylanfuncs')
  interpret <- lmer.interpret(checkmod)
  interpret <- interpret[c(2:nrow(interpret)),]%>%
    mutate(LowCI = Estimate + (qnorm(0.025)*`Std..Error`))%>%
    mutate(HighCI = Estimate + (qnorm(0.975)*`Std..Error`))
  return(interpret)
}
