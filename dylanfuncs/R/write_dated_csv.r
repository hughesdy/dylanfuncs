#' Write csv with date appended to end of filename
#'
#' @description
#'  This function takes a dataframe and writes to a csv with the date appended to the end of the filename (e.g., "data_20251129.csv")
#' @usage write_dated_csv(data = datframe, filepath="path_to_out/file.csv")
#' @param data dataframe to be written to csv
#' @param filepath full path and filename to be output. Please include the .csv, otherwise the function will error out
#' @details
#' This function just adds the date to the csv name. Nothing fancy. Just saves me 2.3 seconds everytime I want to add the date to the csv. Though it took me about 10 minutes to write all of this so it will only pay off after 600/2.3 times of using it.. oh well
#' @author Original code by Dylan E. Hughes
#' @examples
#' data <- data.frame("a"=sample(1:100, 50), "b" = sample(1:100,50))
#' write_dated_csv(data=data, filepath="path_to_out/file.csv")
#'

write_dated_csv <- function(data, filepath) {
  currentDate = format(Sys.time(), "%Y%m%d")

  last4 = substr(filepath, nchar(filepath)-3, nchar(filepath))

  if (last4 != ".csv") {
    stop(".csv should be at the end of your file name")
  }

  firstPart = substr(filepath, 1, nchar(filepath)-4)

  outname = paste0(firstPart, "_", currentDate, ".csv")

  write.csv(data,outname,row.names=F)

  cat("Data written to:\n", outname)
}
