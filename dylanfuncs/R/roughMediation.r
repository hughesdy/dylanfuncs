#' Performs a rough mediation using simple linear regression framework
#'
#' Given x, a continuous y, a mediator, and optional covariates, this function will generate the total, direct, and indirect effect as well as provide the proportion of the total effect mediated by the mediator and the p-value of X given M. It will not provide the p-value associated with the indirect effect.
#' @param data Dataframe containing variables of interest
#' @param x Your x variable, aka independent variable, as a character
#' @param y Your y variable, aka dependent variable, as a character
#' @param mediator Your mediator variable as a character
#' @param covariates Your covariates
#' @param treat If your x variable is categorical, this specifies what the level we are interested in is. For example, if we have a gender variable with 3 levels, man, woman, and trans, and we are interested in the effect associated with the 'trans' level, we would provide the name of the level i.e., 'trans' to this argument
#' @keywords mediation, rough
#' @export
#' @examples

roughMediation <- function(data, x, y, mediator, covariates, treat=NULL) {
  totalMod.char = paste0("lmer(", y, "~", x, "+", paste0(covariates, collapse = "+"), ", data)")
  aPath.char = paste0("lmer(", mediator, "~", x, "+", paste0(covariates, collapse = "+"), ", data)")
  mediMod.char = paste0("lmer(", y, "~", x, "+", mediator, "+", paste0(covariates, collapse = "+"), ", data)")

  totalMod = summary(eval(parse(text = totalMod.char)))$coefficients
  aPath = summary(eval(parse(text = aPath.char)))$coefficients
  mediMod = summary(eval(parse(text = mediMod.char)))$coefficients

  # If main term is an interaction term
  if (grepl("*", x, fixed = T)) {
    split = strsplit(x, split = "*", fixed = T)[[1]]
    findRow = paste0(split[1], treat, ":", split[2])

    totalRow = which(grepl(findRow, rownames(totalMod)))
    directRow = which(grepl(findRow, rownames(mediMod)))

    totalEffect = totalMod[totalRow, 1]
    directEffect = mediMod[directRow, 1]
  } else {
    totalEffect = totalMod[which(rownames(totalMod)==paste0(x, treat)), 1]
    directEffect = mediMod[which(rownames(mediMod)==paste0(x, treat)), 1]
  }

  indirectEffect = totalEffect - directEffect
  propMediated = indirectEffect / totalEffect

  if (grepl("*", x, fixed = T)) {
    treat.p = mediMod[directRow, 5]
  } else if (class(data[,x]) == "factor" | class(data[,x]) == "character") {
    treat.p = mediMod[which(rownames(mediMod)==paste0(x, treat)), 5]
  } else {
    treat.p = mediMod[which(rownames(mediMod) == x), 5]
  }


  if (treat.p < 0.05) {
    medType = "partial"
    print("Partial mediation")
  } else {
    medType = "full"
    print("Full mediation")
  }

  returnList = list("totalEffect" = totalEffect, "directEffect" = directEffect, "indirectEffect" = indirectEffect, "propMediated" = propMediated, "treat.p" = treat.p)

  return(returnList)

}
