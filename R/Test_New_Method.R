Test_New_Method<-function(path) {
  db.test<-data.frame(y=rnorm(20),
                      group=rep(c(1,2),c(10,10)))
  
  check.method.result<-c()
  check.method.issues<-list()
  check.method.names<-c()
  cnt<-1
  method.files<-dir(path=paste(path,"/Methods",sep=""))
  load(paste(path,"/Results/NeutraliseStatus.RData",sep=""))
  method.exists<-
    !method.files%in%neutralise.status$file.name[
      (neutralise.status$type=="method")&
        (neutralise.status$check==TRUE)]
  
  
  if(sum(method.exists)>0) {
    for(method in method.files[method.exists]) {
      issues<-c()
      filename<-paste(path,"/Methods/",method,sep="")
      header<-Check_Method_Description(filename)
      if(!header) {
        issues<-c(issues,
                  "Header of method R file does not have a correct header")
      }
      method.name<-strsplit(method,".R")[[1]]
      if(method.name%in%neutralise.status$name) {
        issues<-c(issues,
                  "methods name already exists")
      }
      source(filename)
      res<-try(Test(db.test), silent=TRUE)
      if(inherits(res, "try-error")) {
        issues<-c(issues,"error on running Test function")
      }
      else {
        if(max((names(res)!=c("stat","p.value")))==1) {
          issues<-c(issues,"names of returned object are wrong")
        }
      }
      if(length(issues)==0) {
        neutralise.status<-neutralise.status%>%
          add_row(file.name=method,
                  name=method.name,
                  type="method",
                  check=TRUE,
                  to.run=TRUE,
                  neutralised=FALSE)
      }
      if(length(issues)>0) {
        neutralise.status<-neutralise.status%>%
          add_row(file.name=method,
                  name=method.name,
                  type="method",
                  check=FALSE,
                  to.run=FALSE,
                  neutralised=FALSE)
        write(issues,
              file=paste(path,"/Issues/issues_",method.name,".txt",sep=""))
      }
      save(neutralise.status,
           file=paste(path,"/Results/NeutraliseStatus.RData",sep=""))
      
      check.method.issues[[cnt]]<-issues
      check.method.names<-c(check.method.names,method.name)
      check.method.result<-c(check.method.result,
                             ifelse(length(issues)==0,TRUE,FALSE))
      cnt<-cnt+1
    }
  }
  if(sum(method.exists)==0) {
    check.method.result<-0
  }
  return(list(check.method.issues=check.method.issues,
              check.method.names=check.method.names,
              check.method.result=check.method.result))
}

