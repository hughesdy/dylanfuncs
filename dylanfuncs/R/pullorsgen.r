#' Pull ORs from glmer mice models
#'
#' This function takes a glmer mice model as an argument and prints ORs and Peas
#' @param x model
#' @keywords glmer, mice, ors, odds ratios
#' @export
#' @examples
#' pullorsgen()

pullorsgen <- function(x) {
  summ <- summary(pool(x),conf.int=TRUE, conf.level=0.95)
  df = as.data.frame(matrix(ncol=4, nrow = nrow(summ)-1))
  sumcol = c(2,7,8,6)
  for (i in c(1:4)) {
    for (a in c(2:(nrow(df)+1))) {
      if (i<4) {
        df[(a-1),i] = exp(1)^(summ[a, sumcol[i]]) 
      }
      if (i==4) {
        df[(a-1),i] = summ[a,6]
      }
    }
  }
  rownames(df) = summ[c(2:nrow(summ)),1]
  colnames(df) = c('OR','LowEnd','HighEnd','P')
  return(df)
}