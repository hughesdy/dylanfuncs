#' Converts columns to numeric
#'
#' This function converts character/factor columns in a data frame (x) to class numeric. The function will assume the first column is the subject key and will convert all but the first column to numeric values. Inputting skipcols will tell the function which columns to skip i.e. non-numeric ones.
#' @param x skipcols
#' @keywords conversion, numeric, from character, from factor
#' @export
#' @examples
#' convert2num()

convert2num <- function(x, skipcols=NULL) {
  if (is.null(skipcols)) {
    for (i in c(2:ncol(x))) {
      x[,i] = as.numeric(as.character(x[,i]))
    }
  } else if (length(skipcols > 0)) {
    for (i in c(2:ncol(x))) {
      if (i %in% skipcols) {
        x[,i] = x[,i]
        next
      } else {
        x[,i] = as.numeric(as.character(x[,i]))
      }
    }
  }

  return(x)
}
