# DESCRIPTION
# simulation of two normal distributions with equal variance

Data.Generator<-function(n1=10,n2=10,parameters=c(0,1)) {
  delta<-parameters[1]
  sds<-parameters[2]
  y<-c(rnorm(n1,sd=sds),
       rnorm(n2,mean = delta,sd=sds))
  db<-data.frame(y=y, group=rep(c(1,2),c(n1,n2)))
  return(db)
}

