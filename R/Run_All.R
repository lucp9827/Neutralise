#'
#' @export
Run_All<-function(path,N,B) {

  test.method<-Test_New_Method(path)
  if(max(test.method$check.method.result)==1) {
    # run new tests on all the data
    Run_All_New_Methods(path,N=N,B=B)
  }
  else {
    # call function to report the issues
  }

  test.data<-Test_New_Data(path)
  if(max(test.data$check.data.result)==1) {
    # run all tests on new data
    Run_All_New_Data(path,N=N,B=B)
  }
  else {
    # call function to report the issues
  }

  test.settings<-Test_New_Setting(path)
  if(max(test.settings$check.settings.result)==1) {
    # run all tests on new data
    Run_All_New_Data(path,N=N,B=B)
  }
  else {
    # call function to report the issues
  }

}

