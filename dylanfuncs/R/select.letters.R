#' Select only letters from a string
#'
#' This function selects only letters from a string (i.e., removes numbers)
#' @param x
#' @keywords select.letters
#' @export
#' @examples
#' select.letters()
select.letters <- function(x) {
  split = strsplit(x, split = '')[[1]] ## split name into parts
  let = suppressWarnings(which(is.na(as.numeric(split))))  ## find non-numeric
  final <- paste(split[let], collapse = '', sep = '')
  return(final)
}

