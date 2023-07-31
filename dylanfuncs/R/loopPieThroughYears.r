#' Make pie charts for multiple timepoints at once
#'
#' This function iterates through user-provided timepoints and generates a pie chart for each timepoint. Assumes your data is in long format. Calls makePie() argument. On the last iteration, the loop will automatically grab the legend from the plot. The output is a bit wonky. See "Examples" for an example of how to use it with grid.arrange().
#' @param data A vector within a dataframe to be pie'd
#' @param times A vector containing the timepoints you would like to iterate through
#' @keywords loopPieThroughYears loop pie makePie
#' @export
#' @examples
#' events = levels(as.factor(dataframe$time_variable))[2:5] # Will grab the second through fifth timepoints in your data assuming your time variable is called "time_variable" within your dataframe.
#' pieList <- loopPieThroughYears(data = dataframe$categorical_variable1, times = events)
#' grid.arrange(pieList[[1]], pieList[[2]], pieList[[3]], pieList[[4]][1]$plot, pieList[[4]][2]$legend, nrow = 3, ncol = 2, layout_matrix = rbind(c(1,2), c(3,4), c(5,5)))

loopPieThroughYears <- function(data, times) {
  list = list()

  data.char = deparse(substitute(data))

  money = which(strsplit(data.char, split = '')[[1]] == '$')

  df.name = substr(data.char, start = 1, stop = money - 1)

  var.name = substr(data.char, start = money+1, stop = nchar(data.char))

  for (time in times) {
    if (time == times[length(times)]) {
      list = append(list, list(makePie(data = data[eval(parse(text = df.name))[,'eventname'] == time], title = paste0('Y', strsplit(time, split='')[[1]][1]), print = F, showLegend = F, grabLegend = T)))
    } else {
      list = append(list, list(makePie(data = data[eval(parse(text = df.name))[,'eventname'] == time], title = paste0('Y', strsplit(time, split='')[[1]][1]), print = F, showLegend = F, grabLegend = F)))
    }
  }

  return(list)
}
