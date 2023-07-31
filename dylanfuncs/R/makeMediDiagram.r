#' Makes mediation diagram
#'
#' Takes output from pull_important_medvals and plots them in a digestible mediation path diagram. Mediation models must be from the mediation package
#' @param medvals this is the output from pull_important_medvals
#' @param a.lab the label to be added to the "a" box which represents the main predictor
#' @param b.lab the label to be added to the "b" box which represents the mediator variable
#' @param c.lab the label top be added to the "c" box which represents the main outcome
#' @keywords makeMediDiagram, mediation, pull_important_medvals
#' @export
#' @examples

makeMediDiagram = function(medvals, a.lab, b.lab, c.lab) {
  library(diagram)

  if (medvals$AtoB[2] < 0.001) {
    a2b = paste0("'",medvals$AtoB[1], "**'")
  } else if (medvals$AtoB[2] < 0.05) {
    a2b = paste0("'",medvals$AtoB[1], "*'")
  } else {
    a2b = paste0("'",medvals$AtoB[1],"'")
  }

  if (medvals$BtoC[2] < 0.001) {
    b2c = paste0("'",medvals$BtoC[1], "**'")
  } else if (medvals$BtoC[2] < 0.05) {
    b2c = paste0("'",medvals$BtoC[1],"*'")
  } else {
    b2c = paste0("'",medvals$BtoC[1],"'")
  }

  if (medvals$ADE[2] < 0.001) {
    ADE = paste0("'",medvals$ADE[1], "**'")
  } else if (medvals$ADE[2] < 0.05) {
    ADE = paste0("'",medvals$ADE[1], "*'")
  } else {
    ADE = paste0("'",medvals$ADE[1],"'")
  }

  if (medvals$`Total effect`[2] < 0.001) {
    total = paste0("'",medvals$`Total effect`[1], "**'")
  } else if (medvals$`Total effect`[2] < 0.05) {
    total = paste0("'",medvals$`Total effect`[1], "*'")
  } else {
    total = paste0("'",medvals$`Total effect`[1],"'")
  }

  data <- c(0, a2b, 0,
            0, 0, 0,
            b2c, paste0(total,' (', ADE,')'), 0)
  M <- matrix(nrow=3, ncol=3, byrow = T, data=data)

  plot <- plotmat(M, pos=c(1,2), name = c(b.lab, a.lab, c.lab), box.type = 'rect', box.size = 0.12, box.prop = 0.5, curve = 0, cex=0.8, box.cex = 0.8, main = paste0('Proportion of effect mediated: ', round(medvals$`Prop Med`,2)))

  print(plot)
}
