Check_Data_Description<-function(filename) {
  check<-TRUE
  con=file(filename,"r")
  tmp<-readLines(con,n=-1)
  close(con)
  if(max(tmp=="# DESCRIPTION")!=1) {
    check<-FALSE
  }
  return(check)
}
