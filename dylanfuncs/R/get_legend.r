#' Grab legend in glob format from ggplot plot
#'
#' This function grabs a legend from your ggplot plot to be used later with gridExtra functions. This was taken from http://stackoverflow.com/questions/12539348/ggplot-separate-legend-and-plot. makePie() calls this function, so they must be loaded together. I'm not sure how exactly this function can be helpful outside of the context of makePie().
#' @param myggplot a ggplot object from which to grab legend
#' @keywords get_legend makePie
#' @export
#' @examples

get_legend<-function(myggplot){
  tmp <- ggplot_gtable(ggplot_build(myggplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}
