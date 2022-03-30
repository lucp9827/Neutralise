#'
#' @export
Power<-function(n1,n2,parameters,N=100) {
  p.values<-numeric(N)
  for(i in 1:N) {
    db<-Data.Generator(n1=n1,
                       n2=n2,
                       parameters = parameters)
    res<-Test(db)
    p.values[i]<-res$p.value
  }
  return(c(mean(p.values<0.01),
           mean(p.values<0.05),
           mean(p.values<0.10)))
}

