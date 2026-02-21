#' Read PRS ranges
#'
#' This function reads in plink polygenic risk scores
#' @param x nofsubjects
#' @keywords prs
#' @export
#' @examples
#' readpcas()
readpcas <- function(x, nofsubjects) {  ## x = the name of the disorder (e.g. 'adhd' in the example above) as a string/character; nofsubjects = number of subjects in your dataset (more specifically, the number of subjects with genetics data)
  require(dplyr)
  c = c(1:8)
  total = matrix(nrow = nofsubjects, ncol = 8)
  for (i in c(1:8)) {
    which <- paste('abcd_', sprintf("%s",x),
                   '_prs_plink.S',sprintf("%d", i), ".profile", sep = "")
    profile = read.table(which, header=TRUE, sep = '')
    if ('SCORESUM'%in%colnames(profile)) {
	    profile1 = dplyr::select(profile, SCORESUM)
    	total[,i]=as.numeric(profile1[,1])
    }
    if ('SCORE'%in%colnames(profile)) {
	    profile1 = dplyr::select(profile, SCORE)
	    total[,i] = as.numeric(profile1[,1])
    }
  }
  colnames(total)=c(paste(sprintf('%s',x),'_prs_0.01',sep=''),
                    paste(sprintf('%s',x),'_prs_0.05',sep=''),
                    paste(sprintf('%s',x),'_prs_0.1',sep=''),
                    paste(sprintf('%s',x),'_prs_0.2',sep=''),
                    paste(sprintf('%s',x),'_prs_0.3',sep=''),
                    paste(sprintf('%s',x),'_prs_0.4',sep=''),
                    paste(sprintf('%s',x),'_prs_0.5',sep=''),
                    paste(sprintf('%s',x),'_prs_1.0',sep=''))
  keys <- read.table(which, header = TRUE, sep = '')%>%
    dplyr::select(IID)%>%
    rename(SUBJECTKEY=IID)
  total = cbind(keys, total)
  return((total))
}
