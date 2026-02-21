#' Fill NA values with specified values
#'
#' This function is used in conjunction with pullNAaction and will replace NAs with NDAR-specified missing values
#' @param data cols
#' @keywords fillinNAs ndatools
#' @export
#' @examples
#' fillinNAs()
fillinNAs <- function(data, cols, fill=NULL) {
  if (is.null(fill)) {
    for (i in cols) {
      naction = pullNAaction(colnames(data)[i])
      if (naction == 'NoNA') {
        data[,i][which(is.na(data[,i]))] = ''
        print(paste(colnames(data)[i], ":", " no NA found, no last known NA, so defaulting to blank space"))

      } else {
        data[,i][which(is.na(data[,i]))] = naction
      }
    }
  } else {
    for (i in cols) {
      data[,i][which(is.na(data[,i]))] = fill
    }
  }

  return(data)
}

