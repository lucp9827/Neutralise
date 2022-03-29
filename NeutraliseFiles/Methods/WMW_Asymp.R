# NAME
# Asymptotic Wilcoxon-Mann-Whitney test
# DESCRIPTION
# Two sample Wilcoxon rank sum test = Mann-Whitney test. P-values based on asymptotic approximation
# REFERENCES
# Wilcoxon, F. (1945). Individual comparisons by ranking methods. Biom. Bull., 1, 80-83.
# END

Test<-function(db) {
  results<-wilcox.test(y~group,data = db, exact=FALSE)
  return(list(
    stat=results$statistic,
    p.value=results$p.value
  ))
}
