#' Neutralise Main function
#'
#' @param path is the directory path of neutralizefiles
#' @param Test, run simulations for this test
#' @param Data.Generator, generates data
#' @return results
#' @examples
#'
Neutralise<-function(path,
                     Test=NULL,
                     Data.Generator=NULL,
                     settings=NULL, B=10000, N=1000) {

  if(is.null(Test)&is.null(Data.Generator)&is.null(settings)) {
    # run code that checks directories to see what need to be run: Run_All
    Run_All(path)
  }

  if((!is.null(Test))&(!is.null(Data.Generator))&(!is.null(settings))) {
    assign("Test",Test, envir = .GlobalEnv)
    assign("Data.Generator",Data.Generator, envir = .GlobalEnv)
    assign("settings",settings,envir = .GlobalEnv)

    res<-Run_Single_Method(path,
                           method.name = deparse(substitute(Test)),
                           data.name = deparse(substitute(Data.Generator)),
                           settings = settings,
                           N=N,
                           mode="single")
  }
}
