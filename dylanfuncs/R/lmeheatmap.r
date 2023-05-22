#' Make lme4 heatmap
#'
#' This function makes a heatmap (cbcl vs prs) with scaled principal components, age, sex, and site as a random effect
#' @param dvs ivs pcs age sex x
#' @keywords prs
#' @export
#' @examples
#' lmeheatmap()
lmeheatmap <- function(dvs, ivs, pcs, age, sex, x) {
  name = colnames(x)
  cbcl=c(1:length(dvs))
  for (i in cbcl) {
    cbcl[i] = grep(dvs[i],name)
  }
  prs=c(1:length(ivs))
  for (i in prs) {
    prs[i] = grep(ivs[i], name)
  }
  peas = c()
  bees = c()
  for (i in prs) {
    for (a in cbcl) {
      model = paste("scale(",name[a],')~scale(',name[i],')+
                    scale(',pcs[1],')+scale(',pcs[2],')+scale(',pcs[3],')+scale(',pcs[4],')+scale(',pcs[5],')+scale(',sex,')+
                    scale(',age,')+(1|SITE_ID)')
      summ = lmer.interpret(lmer(model,x))
      peas = append(peas, summ[2,4])
      bees = append(bees, summ[2,1])
    }
  }
  pea.df = as.data.frame(matrix(ncol=length(prs), nrow=length(cbcl)))
  bee.df = as.data.frame(matrix(ncol=length(prs), nrow=length(cbcl)))
  for (i in c(1:ncol(pea.df))) {
    pea.df[,i]=peas[c((((i-1)*length(cbcl))+1):(i*length(cbcl)))]
    bee.df[,i]=bees[c((((i-1)*length(cbcl))+1):(i*length(cbcl)))]
    colnames(pea.df)[i]=paste(ivs[i],'P',sep='.')
    colnames(bee.df)[i]=paste(ivs[i],'Beta',sep='.')
  }
  pea.df.adj <- adjustmatrix(pea.df)
  pea.df.adj.dir <- direction(pea.df.adj, bee.df)
  
  fhm.fdr.p <- p.adjust(peas, method = 'fdr')
  
  fhm.fdr.mat <- matrix(fhm.fdr.p,ncol=ncol(pea.df),nrow=nrow(pea.df))
  
  fhm.fdr <- updatefdr(fhm.fdr.mat)
  
  pea.df.adj.more <- adjustmatrixmore(pea.df)
  pea.df.adj.dir.more <- direction(pea.df.adj.more, bee.df)
  rownames(pea.df.adj.dir) = dvs
  rownames(pea.df.adj.dir.more) = dvs
  
  peasfhm = matrix(nrow = nrow(pea.df),ncol=ncol(pea.df))
  for (a in c(1:ncol(peasfhm))) {
    for (i in c(1:nrow(peasfhm))) {
      if (pea.df[i,a]>0.0001) {
        peasfhm[i,a] = sprintf("%.4f", pea.df[i,a])
      }
      else {
        peasfhm[i,a] = sprintf("%.1e", pea.df[i,a])
      }
    }
  }
  
  list <- list("adj" = pea.df.adj.dir, "adj.more" = pea.df.adj.dir.more, "fdr" = fhm.fdr, 'raw'=df, 'peas' = pea.df)
  return(list)
}
