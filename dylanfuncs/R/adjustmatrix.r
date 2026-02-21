#' Make easy peas
#'
#' This function replaces p-values with numbers according to a threshold
#' @param x
#' @keywords adjustmatrix
#' @export
#' @examples
#' adjustmatrix()
adjustmatrix <- function(x) { #This function converts p-values to 5 different categories for heatmap
  for (i in c(1:ncol(x))) {
    for (j in c(1:nrow(x))) {
      if (x[j,i] > 0.1) {
        x[j,i] = 0
        next
      }
      if (x[j,i] > 0.05 & x[j,i] <= 0.1) {
        x[j,i] = 1
        next
      }
      if (x[j,i] <= 0.05 & x[j,i] > 0.01) {
        x[j,i] = 2
        next
      }
      if (x[j,i] <= 0.01 & x[j,i] > 0.005) {
        x[j,i] = 3
        next
      }
      if (x[j,i] <= 0.005) {
        x[j,i] = 4
        next
      }
    }
  }
  return(x)
}
