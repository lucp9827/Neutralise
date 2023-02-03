# NAME
# 
# DESCRIPTION
# 
# REFERENCES
#
# 
# END
Test<-function(db) {
  results<-package::function(db$y[db$group==1],db$y[db$group==2],
                   alternative="two.sided")
  return(list(
    stat=results$statistic,
    p.value=results$p.value
  ))
}