#' Make Rmarkdown-friendly pie chart
#'
#' This function makes a pie chart out of data you feed it! Compatible with loopPieThroughYears()
#' @param data vector to be pie'd
#' @param print if True, then the pie chart will be printed. The default is false so that it is compatible with gridExtra later on and won't make your Rmd messy. Defaults to FALSE
#' @param showLegend if True, a legend will be plotted. Default is FALSE to make things cleaner in Rmd when you grid your pies. Defaults to FALSE
#' @param grabLegend Relevant for grid.arrange function. If True, a legend will be grabbed to be fed into gridExtra functions later. Again this is FALSE to be more compatible with gridExtra. If you want to make a omni-legend for your grid plots, then you can change the argument to TRUE and the function will output a legend AND the pie chart as two separate objects
#' @param labelSize Sets the size of labels on pie chart. Defaults to 6.
#' @keywords makePie
#' @export
#' @examples

makePie <- function(data, title, print = F, showLegend = F, grabLegend = F, labelSize = 6) {

  tab <- table(data)

  percentages = round(tab / sum(tab), 2) * 100

  df.tab <- as.data.frame(tab) %>%
    arrange(desc(data)) %>%
    mutate(pctg = ifelse((Freq/sum(tab))<0.01, round((Freq/sum(tab)) * 100, 2), round(Freq/sum(tab), 2)*100)) %>%
    mutate(label = sprintf("%s%%",pctg))

  count=c()
  for (i in c(1:nrow(df.tab))) {
    df.tab$ypos[i] = ((df.tab$Freq[i])/2) + sum(count)
    count = append(df.tab$Freq[i], count)
  }

  plot <- ggplot(df.tab, aes(x="", y=Freq, fill=data)) +
    geom_bar(stat="identity", width=1) +
    coord_polar("y", start=0) +
    theme_void() +
    geom_label_repel(aes(y = ypos, label = label),
                     size = 4.5, nudge_x = 1, show.legend = FALSE) +
    ggtitle(title) +
    theme(legend.title = element_blank())

  if (grabLegend == T) {
    legend <- get_legend(plot)
  }

  if (showLegend == F) {
    plot = plot + theme(legend.position = 'none')
  }

  if (print == T) {
    print(plot)
  }

  if (grabLegend == T) {
    return(list('plot'= plot+theme(legend.position='none'), 'legend' = legend))
  } else {
    return(plot)
  }
}
