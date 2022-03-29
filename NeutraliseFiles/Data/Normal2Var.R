# DESCRIPTION
# simulation of two normal distributions with unequal variance

Data.Generator<-function(n1=10,n2=10,parameters=c(0,1,2)) {
  delta<-parameters[1]
  sd1<-parameters[2]
  sd2<-parameters[3]
  y<-c(rnorm(n1,sd=sd1),
       rnorm(n2,mean = delta,sd=sd2))
  db<-data.frame(y=y, group=rep(c(1,2),c(n1,n2)))
  return(db)
}


