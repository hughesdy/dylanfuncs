#' Update CBCL names to the Roffman convention
#'
#' This function takes the ABCD cbcl names and turns them into our convention (i.e. capital, preceded by timepoint ex: BL, Y2, etc)
#' @param df, cols, timepoint
#' @keywords renaming, cbcl, column names
#' @export
#' @examples
#' renamecbclcolnames()
renamecbclcolnames <- function(df, timepoint) {
  cols = grep('cbcl', colnames(df))
  names = colnames(df)[cols]
  for (name in names) {
    split = strsplit(name, split = '')[[1]]
    title = split[c(14:(length(split)-2))]
    upper = toupper(paste(title, collapse = ''))
    final = paste(toupper(timepoint),'.CBCL_', upper, '_T', sep = '')
    names[which(names == name)] = final 
  }
  colnames(df)[cols] = names
  return(df)
}
