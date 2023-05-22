#' Make colors for heatmap
#'
#' This function selects colors for heatmap
#' @param x
#' @keywords colors
#' @export
#' @examples
#' pickcolors.fhm()
pickcolors.fhm <- function(x) {  ###This function selects colors for the heatmap below
  result = c()
  index = c(-7,-6,-5,-4,-3,-2,-1,0,1,2,3,4,5,6,7,8,9)
  colors = c('darkblue','turquoise4','cornflowerblue','darkturquoise','turquoise','turquoise2','turquoise1','snow',"#FFFFBF","#FFFF40","#FFFF00","#FFCC00","#FF9900","#FF6600","#FF3300","#FF0000", 'red3')
  labels = c('<5x10-6','<5x10-5','<5x10-4','<0.005','<0.01','<0.05','<0.1','>0.1','<0.1', '<0.05','<0.01','<0.005','<5x10-4','<5x10-5','<5x10-6','<5x10-7', '<5x10-8')
  for (i in c(1:ncol(x))) {
    for (a in c(1:nrow(x))) {
      result=append(result, x[a,i])
    }
  }
  uniqs = unique(sort(result))
  coloring=colors[c(which(colors==colors[which(index==min(uniqs))]):which(colors==colors[which(index==max(uniqs))]))]
  if (length(uniqs)>1) {
    legendlabels=labels[c(which(labels==labels[which(index==min(uniqs))]):which(colors==colors[which(index==max(uniqs))]))]
  }
  if (length(uniqs)==1) {
    legendlabels=labels[which(index==uniqs)]
  }
  legendbreaks = c(min(uniqs):(max(uniqs)))
  
  return(list('colors'=coloring, 'legendbreaks'=legendbreaks, 'legendlabels'=legendlabels))
} 
