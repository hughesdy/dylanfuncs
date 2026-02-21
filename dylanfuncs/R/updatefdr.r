#' Make fdr labels
#'
#' This function adjusts labels for heatmap to show fdr significant boxes
#' @param x
#' @keywords fdrcorrect
#' @export
#' @examples
#' updatefdr()
updatefdr <- function(x){   ##This function replaces P<0.05 with a * to make the heatmap
  peasfhm <- matrix(ncol=ncol(x),nrow=nrow(x))
  for (a in c(1:(ncol(x)))) {
    for (i in c(1:nrow(x))) {
      if (x[i,a] < 0.05) {
        peasfhm[i,a] = '*'
      } 
      else {
        peasfhm[i,a] = ' '
      }
    }
  }
  return(peasfhm)
}
