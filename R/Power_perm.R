#'
#' @export
Power_perm<-function(n1,n2,parameters,N=N,B=B) {
  p.values<-numeric(N)
  ct_0.10<-0
  ct_0.05<-0
  ct_0.01<-0
  na_ct <- 0
  p_val_perm<-c()

  for(i in 1:N) {
    db<-Data.Generator(n1=n1,
                       n2=n2,
                       parameters = parameters)

    if (!is.null(B)){
      null_dist=c()
      for (j in (1:B)){
        shuffle <- sample(db[,2])
        shuffled_data <- cbind(y=db[,1],group=shuffle)

        res<-Test(shuffled_data)
        null_dist[j]=res$stat
      }
    }

    res<-Test(db)


    if (!is.null(B)){
      p_val_perm=cbind(mean(null_dist >= res$stat),p_val_perm)
      p_val_perm=mean(p_val_perm,na.rm = T)
    }

    p.values[i]<-res$p.value
    if(is.na(res$p.value)){
      na_ct = na_ct+1
      next
    }
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
           pval_0.1,ci_0.1,ct_0.10,ct_0.05,ct_0.01,p_val_perm))
}

