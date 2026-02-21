#' Recode 22q data to match NDA data structures
#'
#' This function recodes 22q data to make it compatible with NDA submission
#' @param raw x y
#' @keywords recode.nda
#' @export
#' @examples recode.nda(raw = raw, x = 22q.coding, y = nda.coding)
#' recode.nda()
recode.nda <- function(raw, x, y) {
  output = y[which(x == raw)]
  return(output)
}

