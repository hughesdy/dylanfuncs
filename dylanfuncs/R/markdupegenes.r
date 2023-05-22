#' Marks duplicate genes
#'
#' This function marks duplicate genes and is necessary for convert2mappable function.
#' @param x
#' @keywords dupes genes convert2mappable
#' @export
#' @examples
#' markdupegenes()

markdupegenes <- function(x) {
  while (length(which(duplicated(colnames(x))))>0) {
    for (i in c(1:length(which(duplicated(colnames(x)))))) {
      colnames(x)[which(duplicated(colnames(x)))[i]] = paste(colnames(x)[which(duplicated(colnames(x)))][i], '.2', sep='')
    }
  }
  return(x)
}
