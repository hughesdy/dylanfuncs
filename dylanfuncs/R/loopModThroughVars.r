#' Function to loop through DVs and IVs and return results from lme4::lmer() models
#'
#' This function iterates through mixed effects models with a specified set of dependent and independent variables as well as covariates.
#' @param dvs A vector of characters corresponding to variables in your dataframe that represent dependent variables
#' @param ivs A vector of characters corresponding to your main predictors/independent variables
#' @param rownames Vector of characters specifying row names of the resultant table. These will correspond to your main predictors/IVs
#' @param colnames Vector of characters specifying column names of the resultant table. These correspond to your outcomes/DVs
#' @param data Dataframe in which your variables live
#' @param na.omit If true, will first remove instances of NAs using the na.omit function. Defaults to FALSE.
#' @keywords loopMod loopThroughMod
#' @export
#' @examples
#' master <- read.csv('path/to/master/file')
#' all.dvs <- c('flanker','processingSpeed','dimChangeCard', 'DistressScoreSum')
#' cpbrain.pgs <- colnames(select(master, contains('z.CP_M')))
#' row.names=substr(cpbrain.pgs, start = 6, stop = nchar(cpbrain.pgs)) # picks the 6th through last character of each element of cpbrain.pgs
#' col.names = substr(all.dvs, start = 1, stop = 15) # picks 1st through 15th character of each element of all.dvs
#' covariates = c('z.pc1','z.pc2','z.pc3','z.pc4','z.pc5','interview_age_new','demo_sex_v2','(1|site_id_l/rel_family_id/subjectkey)')
#' cpbrainspan <- loopModThroughVars(dvs = all.dvs, ivs = cpbrain.pgs, rownames=row.names, colnames = col.names, covariates = covariates, data = master)


loopModThroughVars <- function(dvs, ivs, rownames, colnames, covariates, data, na.omit = F) {

  library(car)

  betas <- as.data.frame(matrix(nrow = length(ivs), ncol = length(dvs)))
  rownames(betas) = rownames
  colnames(betas) = colnames

  peas <- betas
  dimnames(peas) = dimnames(betas)

  covar.model = paste(covariates, collapse = ' + ')


  counter = 0
  for (iv in ivs) {
    rownum = which(ivs==iv)
    for (dv in dvs) {
      colnum = which(dvs==dv)

      ### Adjust this model accordingly
      model.char = paste0("lmer(", dv," ~ ", iv, ' + ', covar.model,',data)')
      model <- eval(parse(text = model.char))

      betas[rownum, colnum] = model@beta[2]
      peas[rownum, colnum] = Anova(model)$`Pr(>Chisq)`[1]

      counter=counter+1
      print((length(ivs) * length(dvs)) - counter)
    }
  }

  ## Create fdr adjusted p
  fdr=c()
  fdr <- sapply(peas, function(x) append(fdr, x))
  fdr <- p.adjust(fdr, method = 'fdr')

  fdr.df = as.data.frame(matrix(fdr, ncol = ncol(peas)))
  dimnames(fdr.df) = dimnames(peas)

  return.list = list('betas' = betas, 'peas' = peas, 'fdr' = fdr.df)
}
