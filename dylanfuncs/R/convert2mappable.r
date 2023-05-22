#' Convert gene expression data into mappable data..
#'
#' This function converts expression data to interpretable/mappabale data
#' @param x ageref include.dupes
#' @keywords convert mappable expression
#' @export
#' @examples
#' convert2mappable()
convert2mappable <- function(x, ageref, include.dupes = T) {
  
  flipped <- as.data.frame(t(as.matrix(x[,c(1:(ncol(x)-1))])))##invert matrix to make it mappable
  
  colnames(flipped) = x$gene ###add gene names to column names
  
  ageref$track = c(1:nrow(ageref))
  
  if (include.dupes == F) {
    if (length(which(duplicated(ageref$age)))>0) {    ###if there are two samples from the same age, we combine them (mean) 
      dupeind <- which(duplicated(ageref$age))
      for (i in c(1:length(dupeind))) {
        for (col in c(1:ncol(flipped)))
          flipped[(dupeind[i]-1),col] = (flipped[(dupeind[i]-1), col] + flipped[(dupeind[i]),col])/2
      }
      flipped <- flipped[-dupeind,]
    }
  
    flipped$time = as.factor(unique(ageref$age)) ##add ages to df 
    print('fuck')

  } 
  
  if (include.dupes == T) {
    flipped$time = factor(ageref$age, levels = unique(ageref$age[order(ageref$track)]))
    
  }
  
  
  
  flipped.nodupegenes <- markdupegenes(flipped) ##mark the duplicated gene names in the columns by appending a .2 to them so that we
  ## don't have duplicates in the column names
  for (i in c(1:nrow(flipped.nodupegenes))) {
    if (i ==1) {
      flipped.nodupegenes$mean[i] = mean(as.numeric(flipped.nodupegenes[i,c(1:(ncol(flipped.nodupegenes)-1))]))
    }
    if (i>1) {
      flipped.nodupegenes$mean[i] = mean(as.numeric(flipped.nodupegenes[i,c(1:(ncol(flipped.nodupegenes)-2))]))
    }
  }
  
  flipped.nodupegenes$pre0post1 = c(1:nrow(flipped.nodupegenes))
  for (i in c(1:nrow(flipped.nodupegenes))) {
    string = strsplit(as.character(flipped.nodupegenes$time[i]), split = '')
    together = paste(string[[1]][c((length(string[[1]])-2):length(string[[1]]))], sep='', collapse='')
    if (together == 'pcw') {
      flipped.nodupegenes$pre0post1[i] = 'Prenatal'
    }
    if (together != 'pcw') {
      flipped.nodupegenes$pre0post1[i] = 'Postnatal'
    }
  }
  
  flipped.nodupegenes$pre0post1 <- factor(flipped.nodupegenes$pre0post1, levels = c('Prenatal', 'Postnatal'))
  
  return(flipped.nodupegenes)
}
