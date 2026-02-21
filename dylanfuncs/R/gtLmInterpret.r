#' Make a pretty gt table out of lm (or glm or lmerTest) model output
#'
#' This function takes a model as input and outputs the coefficients from the model in a nice table
#' @param model An object (model) of class lm or lmerModLmerTest
#' @keywords kableLmerInterpret gt table summary
#' @export
#' @examples

gtLmInterpret <- function(model) {
  class = class(model)

  glm=0

  if (length(class) == 1) {
    if (class == 'lm') {
      pcol = 4
    } else if (class(model) == 'lmerModLmerTest' | class(model) == 'lmerMod') {
      pcol = 5
    }
  } else if (length(class) == 2) {
    if (class[1] == 'glm') {
      pcol = 4
      glm=T
    }
  } else {
    print("This type of model is not yet supported. Accepted types = 'lm','lmerMod',and 'glm'")
  }


  summ <- summary(model)$coefficients
  new <- data.frame('Estimate' = round(summ[,1], 3), 'Std.Error' = round(summ[,2], 4), 'P' = format(summ[,pcol], scientific = T, digits = 3))
  rownames(new) = rownames(summ)
  new$P = ifelse(summ[,pcol] < 0.05, paste0(new$P, '*'), new$P)

  if (glm == T) {
    new$Estimate = exp(new$Estimate)
    new$Std.Error = exp(new$Std.Error)
    colnames(new)[colnames(new) == 'Estimate'] = 'OR'
  }

  gt(new, rownames_to_stub = T) %>%
    tab_style(style = cell_text(weight = 'bold'), locations = cells_column_labels()) %>%
    tab_style(style = cell_text(weight = 'bold'), locations = cells_stub()) %>%
    tab_header(title = paste0('Outcome = ', pullOutcome(model)))

  return(new)
}
