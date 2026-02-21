#' Rename column names by prs
#'
#' This function attaches the prs name to column names of a dataframe
#' @param dataframe prsname
#' @keywords prs prsice
#' @export
#' @examples
#' renameprsice()
renameprsice <- function(df, prs) {
  names = colnames(df)
  newnames = c(1:length(names))
  for (i in c(2:length(names))) {
    newnames[i] = paste(prs, names[i], sep = '_')
  }
  newnames[1] = names[1]
  return(newnames)
}
