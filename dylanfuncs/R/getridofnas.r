#' Replaces 'fake' NA values with 'true' NAs
#'
#' Sometimes NAs are coded as 999 or 777 or just as blank spaces. This function replaces those substitute or 'fake' values with 'true' NAs that R can interpret as such. the nas arugment by default is set to a 'true' NA. You can set it to a list. I usually do nas=c(777,999,''). Note that there is no space between the two single quotes.
#' @param df nas
#' @keywords replace nas
#' @export
#' @examples getridofnas(df = data, nas = c(777,999,''))
#' getridofnas()

getridofnas <- function(df, nas=NA) {
  for (i in c(1:ncol(df))) {
    df[,i][which(df[,i]%in%nas)] = NA
  }
  return(df)
}
