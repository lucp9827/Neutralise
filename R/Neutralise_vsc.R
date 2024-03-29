#' Neutralise Main function
#'
#' @param path is the directory path of neutralisefiles
#' @param Test is a function of a statistical test, if not specified (=NULL default) it will evaluate all statistical methods in the Methods directory if these are not evaluated yet.
#' @param Data.Generator  is a function of a data generator, if not specified (=NULL default) it will simulate data using all data generators in the Data directory and settings in the Setting directory, if these are not simulated yet.

#' @return results of the analysis (test statistic - p-value - power calculations) in the Results directory
#' @examples see Demonstration.Rmd
#' @export
Neutralise_vsc<-function(path,
                     Test=NULL,
                     Data.Generator=NULL,
                     settings=NULL, B=NULL, N=1000,data.name,method.name) {

  if(is.null(Test)&is.null(Data.Generator)&is.null(settings)) {
    # run code that checks directories to see what need to be run: Run_All
    Run_All(path,N=N)
  }

  if((!is.null(Test))&(!is.null(Data.Generator))&(!is.null(settings))) {
    assign("Test",Test, envir = .GlobalEnv)
    assign("Data.Generator",Data.Generator, envir = .GlobalEnv)
    assign("settings",settings,envir = .GlobalEnv)

    res<-Run_Single_Method(path,
                           method.name = method.name,
                           method.filename = paste0(method.name,'.R'),
                           data.name = data.name,
                           data.filename = paste0(data.name,'.R'),
                           settings = settings,
                           N=N,
                           mode="all",B=B)
  }
}
