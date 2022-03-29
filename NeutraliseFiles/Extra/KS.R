# NAME
# Asymptotic Kolmogorov-Smirnov test
# DESCRIPTION
# Two sample Kolmogorov-Smirnov test . P-values based on asymptotic approximation
# REFERENCES
# Kolmogorov, A. 1933. ???Sulla Determinazione Empirica di una Legge di Distributione???. Giornale dell'Istituto Italiano degli Attuari, 4: 1???11.
# Smirnov, H. 1939. ???Sur les Ecarts de la Courbe de Distribution Empirique???. Recueil MathematiqueMatematiceskii Sbornik), : 3???26. N.S. 6
# END

Test<-function(db) {
    results<-ks.test(db$y[db$group==1],db$y[db$group==2],
                     alternative="two.sided")
    return(list(
      stat=results$statistic,
      p.value=results$p.value
    ))
}
  
