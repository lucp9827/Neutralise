Test_New_Data<-function(path) {
  check.data.result<-c()
  check.data.issues<-list()
  check.data.names<-c()

  cnt<-1
  data.files<-dir(path=paste(path,"/Data",sep=""))
  load(paste(path,"/Results/NeutraliseStatus.RData",sep=""))
  data.exists<-
    !data.files%in%neutralise.status$file.name[
      (neutralise.status$type=="data")&
        (neutralise.status$check==TRUE)]

  if(sum(data.exists)>0) {
    for(data in data.files[data.exists]) {
      issues<-c()
      filename<-paste(path,"/Data/",data,sep="")
      header<-Check_Data_Description(filename)
      if(!header) {
        issues<-c(issues,
                  "Header of data generator R file does not have a correct header")
      }
      data.name<-strsplit(data,".R")[[1]]
      if(data.name%in%neutralise.status$name[neutralise.status$type=="data"]) {
        issues<-c(issues,
                  "data generator name already exists")
      }
      #if(exists("settings")) {
      #  remove("settings")
      #}

      #setting.issues<-Test_New_Setting1(path,
      #                                  data.name)
      #issues<-c(issues,setting.issues)


      source(paste(path,"/Data/",data,sep=""))
      #pars<-settings[1,which(colnames(settings)!="null")]
      res<-try(Data.Generator(n1=20,n2=20),
               silent=TRUE)
      if(inherits(res, "try-error")) {
        issues<-c(issues,"error on running Data.Generator function")
      }
      else {
        # further checks
        if(length(res)!=2) {
          issues<-c(issues,
                    "Data generator should output data from with two columns")
        }
        if(length(res)==2) {
          if(min(names(res)==c("y","group"))==0) {
            issues<-c(issues,
                      "names of generated data frame are incorrect")
          }
          if(nrow(res)!=40) {
            issues<-c(issues,
                      "number of rows of data frame is not equal to n1+n2")
          }
        }
      }


      if(length(issues)==0) {
        neutralise.status<-neutralise.status%>%
          add_row(file.name=data,
                  name=data.name,
                  type="data",
                  check=TRUE,
                  to.run=FALSE,
                  neutralised=FALSE)
      }
      if(length(issues)>0) {
        neutralise.status<-neutralise.status%>%
          add_row(file.name=data,
                  name=data.name,
                  type="data",
                  check=FALSE,
                  to.run=FALSE,
                  neutralised=FALSE)
        write(issues,
              file=paste(path,"/Issues/issues_",data.name,".txt",sep=""))
      }
      save(neutralise.status,
           file=paste(path,"/Results/NeutraliseStatus.RData",sep=""))
      check.data.issues[[cnt]]<-issues
      check.data.names<-c(check.data.names,data.name)
      check.data.result<-c(check.data.result,
                           ifelse(length(issues)==0,TRUE,FALSE))
      cnt<-cnt+1
    }
  }
  if(sum(data.exists)==0) {
    check.data.result<-0
  }

  return(list(check.data.issues=check.data.issues,
              check.data.names=check.data.names,
              check.data.result=check.data.result))
}
