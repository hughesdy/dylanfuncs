#' Turn 999s and 777s to NAs
#'
#' This function turns 999s and 777s into Nas
#' @param df dataframe
#' @keywords nas
#' @export
#' @examples
#' ninestonas()

ninestonas <- function(df) {
  for (col in c(1:ncol(df))) {
    df[,col][df[,col]==999 | df[,col]==777 | df[,col]==555 | df[,col]==888 | df[,col] == ''] = NA
  }
  return(df)
}
