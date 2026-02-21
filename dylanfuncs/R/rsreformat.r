#' Convert snp names to rs format
#'
#' This function converts chr bp to rs
#' @param sumstats, output
#' @keywords sap, rs, a1a2
#' @export
#' @examples
#' rsreformat()
rsreformat <- function(sumstats, output) {
  trans.snp <- read.table('/cluster/roffman2/users/ABCD_Genetics_Analysis_REDO_CASEY_AND_DYLAN/MAGMA/ref_data/g1000_eur.bim', header=F)
  
  snp.dictionary <- mutate(trans.snp, chrome = paste(V1, V4, V5, V6, sep = ':'))%>%
    mutate(chrome2 = paste(V1,V4,V6,V5, sep = ':'))
  
  snp1 <- select(snp.dictionary, chrome, V2)%>%
    rename(snp1 = V2)
  
  snp2 <- select(snp.dictionary, chrome2, V2)%>%
    rename(snp2 = V2)
  
  setwd('/cluster/roffman2/users/ABCD_Genetics_Analysis_REDO_CASEY_AND_DYLAN/PGC_Files/')
  
  one <- left_join(sumstats, snp1, by = c('SNP' = 'chrome'))
  
  two <- left_join(one, snp2, by = c('SNP' = 'chrome2'))
  
  final <- two %>%
    mutate(SNPID = ifelse(is.na(snp1) & !is.na(snp2), snp2, ifelse(is.na(snp2) & !is.na(snp1), snp1, NA)))%>%
    filter(!is.na(SNPID))%>%
    select(-'SNP', -snp1, -snp2)%>%
    rename('SNP'=SNPID)
  
  print('Writing table...')
  
  write.table(final, output, quote = F, row.names=F)
  
  return(final)
}