#' Plot PRS distributions across thresholds
#'
#' This function reads in plink polygenic risk scores
#' @param df prsname
#' @keywords distribution
#' @export
#' @examples
#' plotprsdist()
plotprsdist <- function(df, prsname) {
  require(e1071)
  prs <- colnames(df)[grep(prsname, colnames(df))]

  skewcoef <- c(1:8)
  for (i in c(1:8)) {
    skewcoef[i] = round(skewness(df[,prs[i]]), 4)
  }

  prs.scores <- c(df[,prs[1]], df[,prs[2]],df[,prs[3]],df[,prs[4]],df[,prs[5]],df[,prs[6]],df[,prs[7]],df[,prs[8]])

  prsdf <- data.frame('score' = prs.scores, 'thresh' = c(rep(paste('prs_0.01, skew = ',skewcoef[1]), nrow(df)),rep(paste('prs_0.05, skew = ',skewcoef[2]), nrow(df)),rep(paste('prs_0.1, skew = ',skewcoef[3]), nrow(df)),rep(paste('prs_0.2, skew = ',skewcoef[4]), nrow(df)),rep(paste('prs_0.3, skew = ',skewcoef[5]), nrow(df)),rep(paste('prs_0.4, skew = ',skewcoef[6]), nrow(df)),rep(paste('prs_0.5, skew = ',skewcoef[7]), nrow(df)),rep(paste('prs_1.0, skew = ',skewcoef[8]), nrow(df))))
  
  ggplot(prsdf, aes(x = score)) + geom_density() + facet_wrap(~thresh, scales = 'free') + ggtitle(prsname)
  
}
