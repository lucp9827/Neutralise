# NAME
# Two-sample t-test
# DESCRIPTION
# Two sample t-test under equal variance assumption
# REFERENCES
# None
# END

Test<-function(db) {
  results<-t.test(y~group,data = db,var.equal=TRUE)
  return(list(
    stat=results$statistic,
    p.value=results$p.value
  ))
}
