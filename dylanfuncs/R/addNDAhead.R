#' Add header to NDA upload files
#'
#' This function adds the header to data to be uploaded to NDA
#' @param data
#' @keywords addNDAhead ndatools
#' @export
#' @examples
#' addNDAhead()
addNDAhead <- function(data) {
  ## This function should only be used within the "data_wrangle_4ndaupload.." loop
  write.csv(data, paste('intermediate/', var, '_intermed.csv', sep = ''), row.names=F)
  intermed <- read.csv(paste('intermediate/', var, '_intermed.csv', sep = ''), header=F)
  header = c(docdic$template.head1[which(docdic$variable==var)], docdic$template.head2[which(docdic$variable==var)], rep('', (ncol(data)-2)))
  final <- rbind(header, intermed)

  return(final)
}

