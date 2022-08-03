#'
#' @export
Power<-function(n1,n2,parameters,N=N) {
  p.values<-numeric(N)
  for(i in 1:N) {
    db<-Data.Generator(n1=n1,
                       n2=n2,
                       parameters = parameters)
    res<-Test(db)
    p.values[i]<-res$p.value
  }
  pval_0.01 = mean(p.values<0.01)
  pval_0.05 = mean(p.values<0.05)
  pval_0.1 = mean(p.values<0.1)

  q_0.01 = round(qnorm(1-(0.01/2)),digit=3)
  q_0.05 = round(qnorm(1-(0.05/2)),digit=3)
  q_0.1 = round(qnorm(1-(0.1/2)),digit=3)

  ci_0.01=c(pval_0.01-q_0.01*sqrt(pval_0.01*(1-pval_0.01)/N),pval_0.01+q_0.01*sqrt(pval_0.01*(1-pval_0.01)/N))
  ci_0.05=c(pval_0.05-q_0.05*sqrt(pval_0.05*(1-pval_0.05)/N),pval_0.05+q_0.05*sqrt(pval_0.05*(1-pval_0.05)/N))
  ci_0.1=c(pval_0.1-q_0.1*sqrt(pval_0.1*(1-pval_0.1)/N),pval_0.1+q_0.1*sqrt(pval_0.1*(1-pval_0.1)/N))

  return(c(pval_0.01,ci_0.01,
           pval_0.05,ci_0.05,
           pval_0.1,ci_0.1))
}

