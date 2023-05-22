#' Strip AB bit from SUBJECTKEY
#'
#' This function strips the "AB" bit from subject IDs output from PRSice
#' @param vec
#' @keywords ab strip prsice
#' @export
#' @examples
#' stripab()
stripab <- function(vec) {
  new = c(1:length(vec))
  for (i in c(1:length(vec))) {
    stripped = strsplit(vec[i], split = '')[[1]]
    ndar = stripped[c(11:length(stripped))]
    new[i] = paste(ndar, sep = '', collapse ='')
  }
  return(new)
}
