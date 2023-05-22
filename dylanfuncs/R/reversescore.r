#' Reverse scores items
#'
#' This function reverse scores items. The levels argument should be the levels of the original variables e.g. if you have a question where 1 = yes, 2 = maybe, 3 = no, and you want to flip those around, the levels argument would be c(1,2,3).
#' @param x levels
#' @keywords reverse score
#' @export
#' @examples
#' reversescore()

reversescore <- function(x, levels) {
  rev.lev = rev(levels)
  for (i in c(1:length(x))) {
    if (is.na(x[i])) {
      x[i] = NA
      next
    }
    x[i] = rev.lev[which(levels==x[i])]
    next
  }
  return(x)
}
