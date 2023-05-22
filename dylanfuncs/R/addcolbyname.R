#' Add an empty column to a dataframe
#'
#' This function will append an empty (NA-filled) column to the dataframe with name "name"
#' @param data Dataframe to which to append empty column
#' @param name Name of the new column
#' @keywords addcolbyname
#' @export
#' @examples addnumbersequence(data = data, name = 'New Column Name')
addcolbyname <- function(data, name) {
  new = as.data.frame(rep(NA, nrow(data)))

  final = cbind(data, new)

  colnames(final)[ncol(final)] = name

  return(final)
}
