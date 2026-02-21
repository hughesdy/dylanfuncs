#' Determine lme4 direction for heatmap
#'
#' This function determines direction of the effect from lme4 betas
#' @param x y
#' @keywords directionofeffect
#' @export
#' @examples
#' direction()
direction <- function(x,y) {   #This function determines the direction of the effect for heatmap
  for (i in c(1:ncol(x))) {
    for (j in c(1:nrow(x))) {
      if (y[j,i] < 0) {
        x[j,i] = x[j,i]*-1
      }
    }
  }
  return(x)
}
