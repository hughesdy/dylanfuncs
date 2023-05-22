#' Add header to NDA upload files
#'
#' This function will take text and add a range of numbers to it so the user can copy and paste into their script. For example if you have a 21-item questionnaire (e.g., pqb) and want to add up all of the items, you can specify the text (e.g.., "pqb"), the numbers that follow (e.g., 1:21), and the delimiter (e.g., '+'), and you'll get a nice list that you can print and copy and paste.
#' @param test delimiter range
#' @keywords sequence numbers letters
#' @export
#' @examples addnumbersequence(text = 'pqb', delimiter = ' + ', range = c(1:21))
#' addnumbersequence()
addnumbersequence <- function(text, delimiter, range) {
  start = min(range)
  end = max(range)

  for (i in c(start:end)) {
    if (i == start) {
      init = paste(text, start, sep = '')
    } else {
      init = paste(init, paste(text, i, sep = ''), sep = delimiter)
    }
  }
  return(init)
}

