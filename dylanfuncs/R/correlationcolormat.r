#' Assist heatmap function
#'
#' This function is for the heatmap function, helps to pick colors and breaks
#' @param ref empty
#' @keywords color, heatmap
#' @export
#' @examples
#' correlationcolormat()
correlationcolormat <- function(ref, empty, slide = T, max = NULL) {
  if (slide == T) {
    max = max(ref)
  }
  
  breaks = c(max/10, 2*(max/10), 3*(max/10), 4*(max/10), 5*(max/10), 6*(max/10), 7*(max/10), 8*(max/10), 9*(max/10), 10*(max/10))
  breaks.char = c(round(max/10, 3), round((max/10)*2, 3), round((max/10)*3, 3), round((max/10)*4, 3), round((max/10)*5, 3), round((max/10)*6, 3), round((max/10)*7, 3), round((max/10)*8, 3), round((max/10)*9, 3), round((max/10)*10, 3)) 
  
  for (i in c(1:ncol(ref))) {
    for (a in c(1:nrow(ref))) {
      if (ref[a,i]<0) {
        empty[a,i] = -1
      }
      if (ref[a,i]>0 & ref[a,i]<breaks[1]) {  ##simplify correlation matrix (boys)
        empty[a,i] = 1
      }
      if (ref[a,i]>=breaks[1] & ref[a,i]<breaks[2]) {
        empty[a,i] = 2
      }
      if (ref[a,i]>=breaks[2] & ref[a,i]<breaks[3]) {
        empty[a,i] = 3
      }
      if (ref[a,i]>=breaks[3] & ref[a,i]<breaks[4]) {
        empty[a,i] = 4
      }
      if (ref[a,i]>=breaks[4] & ref[a,i]<breaks[5]) {
        empty[a,i] = 5
      }
      if (ref[a,i]>=breaks[5] & ref[a,i]<breaks[6]) {
        empty[a,i] = 6
      }
      if (ref[a,i]>=breaks[6] & ref[a,i]<breaks[7]) {
        empty[a,i] = 7
      }
      if (ref[a,i]>=breaks[7] & ref[a,i]<breaks[8]) {
        empty[a,i] = 8
      }
      if (ref[a,i]>=breaks[8] & ref[a,i]<breaks[9]) {
        empty[a,i] = 9
      }
      if (ref[a,i]>=breaks[9] & ref[a,i]<=breaks[10]) {
        empty[a,i] = 10
      }
    }
  }
  return(list('filled' = empty, 'breaks' = as.character(breaks.char)))
}
