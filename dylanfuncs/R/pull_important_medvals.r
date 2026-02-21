#' Pull mediation values needed for mediation diagram
#'
#' This function takes the output from a mediation model via the mediation package and returns a2b, b2c, and a2c values so that you can make a digestible diagram!
#' @param a this is the initial model (i.e., a to b in the diagram; i.e., main predictor to mediator)
#' @param medi this is the mediation model (i.e., the full model; i.e., a to c controlling for b)
#' @param mdres this is the output from the mediation model you ran with the mediation package
#' @keywords pull_important_medvals, mediation
#' @export
#' @examples

pull_important_medvals <- function(m, y, mdres) {
  summary <- summary(mdres)
  model.m <- lmer.interpret(m)
  model.y <- lmer.interpret(y)
  treat.row = which(grepl(summary$treat.value, rownames(model.m)))
  med.row = which(grepl(summary$mediator, rownames(model.y)))
  total = round(summary$tau.coef,2)
  totalp = summary$tau.p
  direct = round(summary$z0, 2)
  directp = summary$z.avg.p
  apath <- round(model.m[treat.row,1],2)
  apathp <- model.m[treat.row,4]
  bpath <- round(model.y[med.row,1],2)
  bpathp <- model.y[med.row,4]
  propmed <- round(summary$n0,2)
  results = list('Total effect' = c(total, totalp), 'apath' = c(apath, apathp), 'bpath' = c(bpath, bpathp), 'ADE' = c(direct, directp), 'Prop Med' = propmed)
  return(results)
}
