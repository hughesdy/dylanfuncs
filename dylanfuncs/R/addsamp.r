#' Add brainspan sample (donor) to mappable df. 
#'
#' This function adds sample (donor) to mappable df and is necessary for expression.compare function.
#' @param mappable
#' @keywords add donor mappable brainspan
#' @export
#' @examples
#' addsamp()

addsamp <- function(mappable) {
    sample = rep(NA, nrow(mappable))
    for (i in c(1:nrow(mappable))) {
      str = strsplit(rownames(mappable)[i], split = '')[[1]]
      num = (as.numeric(paste(str[-which(str=='V')], sep = '', collapse = ''))) - 1
      sample[i] = cols.cel$donor_name[which(cols.cel$column_num==num)]
    }
    return(sample)
  }
