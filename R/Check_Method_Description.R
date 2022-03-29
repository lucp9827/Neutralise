Check_Method_Description<-function(filename) {
  check<-TRUE
  con=file(filename,"r")
  tmp<-readLines(con,n=-1)
  close(con)
  if(max(tmp=="# NAME")!=1) {
    check<-FALSE
  }
  if(max(tmp=="# DESCRIPTION")!=1) {
    check<-FALSE
  }
  if(max(tmp=="# REFERENCES")!=1) {
    check<-FALSE
  }
  return(check)
}
