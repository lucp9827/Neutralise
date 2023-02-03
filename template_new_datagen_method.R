# DESCRIPTION
# 
Data.Generator<-function(n1=10,n2=10,parameters=c(0,1,2,)) { # adjust parameters to the amount you need to simulate data 
  y<-c() # simulate data with 1:n1 obersvations from group 1 en n2:n observations from group 2
  db<-data.frame(y=y, group=rep(c(1,2),c(n1,n2)))
  return(db)
}