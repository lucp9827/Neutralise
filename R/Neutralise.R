#' Neutralise Main function
#'
#' @param path is the directory path of NeutraliseFiles
#' @param Test is a function of a statistical test, if not specified (=NULL default) it will evaluate all statistical methods in the Methods directory if these are not evaluated yet.
#' @param Data.Generator  is a function of a data generator, if not specified (=NULL default) it will simulate data using all data generators in the Data directory and settings in the Settings directory, if these are not simulated yet.
#' @param settings is a data frame where the parameters for a specific data generator are defined, if not specified (=NULL default) it will use the settings available in the Settings directory, if these are not evaluated yet.
#' @param B the amount of permutations when neccessary
#' @param N the amount of simulations you want to run (default = 10000)
#' @return results of the analysis (test statistic - p-value - power calculations) in the Results directory
#' @examples see Demonstration.Rmd
#' @export
Neutralise<-function(path,
                     Test=NULL,
                     Data.Generator=NULL,
                     settings=NULL, B=NULL, N=10000,reproduce=FALSE) {

  if(is.null(Test)&is.null(Data.Generator)&is.null(settings)) {
    # run code that checks directories to see what need to be run: Run_All
    Run_All(path,N=N,B=B)
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
                           mode="single",B=B)
  }

  If(reproduce){

    assign("Test",Test, envir = .GlobalEnv)
    assign("Data.Generator",Data.Generator, envir = .GlobalEnv)
    assign("settings",settings,envir = .GlobalEnv)

    res<-Run_Single_Method(path,
                           method.name = deparse(substitute(Test)),
                           data.name = deparse(substitute(Data.Generator)),
                           settings = settings,
                           N=N,
                           mode="all",B=B)
  }

}
