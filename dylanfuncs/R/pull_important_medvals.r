#' Pull mediation values needed for mediation diagram
#'
#' This function takes the output from a mediation model via the mediation package and returns a2b, b2c, and a2c values so that you can make a digestible diagram!
#' @param a this is the initial model (i.e., a to b in the diagram; i.e., main predictor to mediator)
#' @param medi this is the mediation model (i.e., the full model; i.e., a to c controlling for b)
#' @param mdres this is the output from the mediation model you ran with the mediation package
#' @keywords pull_important_medvals, mediation
#' @export
#' @examples

pull_important_medvals <- function(a, medi, mdres) {
  summary <- summary(mdres)
  model.a <- lmer.interpret(a)
  model.med <- lmer.interpret(medi)
  treat.row = which(grepl(summary$treat.value, rownames(model.a)))
  med.row = which(grepl(summary$mediator, rownames(model.med)))
  total = round(summary$tau.coef,2)
  totalp = summary$tau.p
  ade = round(summary$z0, 2)
  adep = summary$z.avg.p
  atob <- round(model.a[treat.row,1],2)
  atobp <- model.a[treat.row,4]
  btoc <- round(model.med[med.row,1],2)
  btocp <- model.med[med.row,4]
  propmed <- round(summary$n0,2)
  results = list('Total effect' = c(total, totalp), 'AtoB' = c(atob, atobp), 'BtoC' = c(btoc, btocp), 'ADE' = c(ade, adep), 'Prop Med' = propmed)
  return(results)
}
