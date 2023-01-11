#' @export
filter_significance = function(results,alpha){

  x= colnames(results)

  if (alpha==0.05){

    x_0.10 = 0.10
    colnr = grep(as.numeric(x_0.10 ),x)
    results_tmp=results[,-c(colnr)]

    x= colnames(results_tmp)
    x_0.01 = 0.01

    colnr = grep(as.numeric(x_0.01 ),x)
    results_tmp=results_tmp[,-c(colnr)]
  }
  if (alpha==0.01){

    x_0.10 = 0.10
    colnr = grep(as.numeric(x_0.10 ),x)
    results_tmp=results[,-c(colnr)]

    x= colnames(results_tmp)
    x_0.05 = 0.05

    colnr = grep(as.numeric(x_0.05 ),x)
    results_tmp=results_tmp[,-c(colnr)]
  }
  if (alpha==0.10){

    x_0.01 = 0.01
    colnr = grep(as.numeric(x_0.01 ),x)
    results_tmp=results[,-c(colnr)]

    x= colnames(results_tmp)
    x_0.05 = 0.05

    colnr = grep(as.numeric(x_0.05 ),x)
    results_tmp=results_tmp[,-c(colnr)]

  }
  x= colnames(results_tmp)
  colnr = grep(as.numeric(alpha),x)[1:3]
  colnames(results_tmp)[colnr]=c("power","l_CI","u_CI")

  return(results_tmp)
}
