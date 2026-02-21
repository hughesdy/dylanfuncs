#' Convert to nsub format
#'
#' This function takes converts the NDAR_INV subject key format to nsub-NDARINV subject key format
#' @param og.names
#' @keywords subject key
#' @export
#' @examples
#' subjectkey2nsubformat()
subjectkey2nsubformat <- function(og.names) {
  new.names = rep(NA, length(og.names))
  for (i in c(1:length(og.names))) {
    new.names[i] = sub('NDAR_INV', 'nsub-NDARINV', og.names[i])
  }
  return(new.names)
}