#' Make easy peas
#'
#' This function replaces p-values with numbers according to a threshold
#' @param x
#' @keywords adjustmatrix
#' @export
#' @examples
#' adjustmatrixmore()
adjustmatrixmore <- function(x) {  ##This function converts the p-values to 8 different categories for heatmap
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
      if (x[j,i] <= 0.005 & x[j,i] > 0.0005) {
        x[j,i] = 4
        next
      }
      if (x[j,i] <= 0.0005 & x[j,i] > 0.00005) {
        x[j,i] = 5
        next
      }
      if (x[j,i] <= 0.00005 & x[j,i] > 0.000005) {
        x[j,i] = 6
        next
      }
      if (x[j,i] <= 0.000005 & x[j,i]) {
        x[j,i] = 7
        next
      }
    }
  }
  return(x)
}
