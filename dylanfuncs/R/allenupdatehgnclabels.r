#' Updates deprecated HGNC gene symbols in allen expression
#'
#' This function updates old HGNC gene symbols in allen expression (via freesurfer paper).
#' @param x
#' @keywords allen atlas, hgnc labels
#' @export
#' @examples
#' allenupdatehgnclabels()

allenupdatehgnclabels <- function(x) {
  
  library(biomaRt)
  
  hugofound <- read.table('/cluster/roffman2/users/ABCD_Genetics_Analysis_REDO_CASEY_AND_DYLAN/FUMA/neurodev/allen/hugo_genes/results.txt', header=T, sep = '\t')%>%
    select(Approved.symbol, Previous.symbol)%>%
    filter(!duplicated(Previous.symbol))
  
  df.updated <- x
  
  for (i in c(1:nrow(x))) {
    if (x$gene[i]%in%hugofound$Previous.symbol) {
      new = hugofound$Approved.symbol[which(hugofound$Previous.symbol == df.updated$gene[i])]
      if (new %in% x$gene) {
        next
      } else {
        df.updated$gene[i] = new
      }
    } else {
      next
    }
  }
  
  lookup <- getBM(mart = ensembl, attributes = c('ensembl_gene_id','hgnc_symbol'), filters = 'hgnc_symbol', values = df.updated$gene)
  
  allen.updated.new <- df.updated %>%
    right_join(lookup, ., by = c('hgnc_symbol' = 'gene'))%>%
    filter(!is.na(ensembl_gene_id))%>%
    select(-hgnc_symbol)
  
  return(allen.updated.new)
}
