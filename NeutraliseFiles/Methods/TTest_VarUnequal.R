# NAME
# Welch two-sample t-test
# DESCRIPTION
# Two sample t-test without equal variance assumption
# REFERENCES
# None
# END

Test<-function(db) {
  results<-t.test(y~group,data = db,var.equal=FALSE)
  return(list(
    stat=results$statistic,
    p.value=results$p.value
  ))
}
