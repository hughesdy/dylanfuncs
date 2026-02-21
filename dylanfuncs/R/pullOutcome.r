#' Pull outcome variable name from input model
#'
#' This function takes a model (lm, lmerMod, or glm) as input and outputs the name of the outcome variable
#' @param model An object (model) of class lm or lmerModLmerTest or glm
#' @keywords pullOutcome kableLmerInterpret gt table summary
#' @export
#' @examples

pullOutcome <- function(model) {
  class = class(model)
  if (length(class) == 1) {

    if (class(model) == 'lmerModLmerTest') {
      call = as.character(model@call)
    } else if (class(model) == 'lm') {
      call = as.character(model$call)
    } else if (class(model) == "glmerMod") {
      call = as.character(model@call)
    }

  } else if (length(class) == 2) {
    if (class(model)[1] == 'glm') {
      call = as.character(model$call)
    } else {
      stop("pullOutcome: This type of model is not yet supported")
    }
  } else {
    stop("pullOutcome: This type of model is not yet supported")
  }

  split = strsplit(call[2], split = '')[[1]]
  tilde = which(split == '~')
  outcome = substr(call[2], 1, tilde-2)

  return(outcome)
}
