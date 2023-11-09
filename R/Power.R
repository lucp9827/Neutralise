#'
#' @export
Power<-function(n1,n2,parameters,N=N) {
  p.values<-c()
  ct_0.10<-0
  ct_0.05<-0
  ct_0.01<-0
  na_ct <- 0
  for(i in 1:N) {
    db<-Data.Generator(n1=n1,
                       n2=n2,
                       parameters = parameters)


    res<-Test(db)
    #p.values[i]<-res$p.value
    if(is.na(res$p.value)){
      na_ct = na_ct+1
      next
    }


    p.values<-c(p.values,res$p.value)


    if(res$p.value<0.10){
      ct_0.10 = ct_0.10+1
    }
    if(res$p.value<0.05){
      ct_0.05 = ct_0.05+1
    }
    if(res$p.value<0.01){
      ct_0.01 = ct_0.01+1
    }

  }
  pval_0.01 = mean(p.values<0.01,na.rm = TRUE)
  pval_0.05 = mean(p.values<0.05,na.rm = TRUE)
  pval_0.1 = mean(p.values<0.1,na.rm = TRUE)

  q_0.01 = round(qnorm(1-(0.01/2)),digit=3)
  q_0.05 = round(qnorm(1-(0.05/2)),digit=3)
  q_0.1 = round(qnorm(1-(0.1/2)),digit=3)

  ci_0.01=c(pval_0.01-q_0.01*sqrt(pval_0.01*(1-pval_0.01)/N),pval_0.01+q_0.01*sqrt(pval_0.01*(1-pval_0.01)/N))
  ci_0.05=c(pval_0.05-q_0.05*sqrt(pval_0.05*(1-pval_0.05)/N),pval_0.05+q_0.05*sqrt(pval_0.05*(1-pval_0.05)/N))
  ci_0.1=c(pval_0.1-q_0.1*sqrt(pval_0.1*(1-pval_0.1)/N),pval_0.1+q_0.1*sqrt(pval_0.1*(1-pval_0.1)/N))

  return(c(pval_0.01,ci_0.01,
           pval_0.05,ci_0.05,
           pval_0.1,ci_0.1,ct_0.10,ct_0.05,ct_0.01,na_ct))
}

