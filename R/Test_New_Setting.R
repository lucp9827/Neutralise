#'
#' @export
Test_New_Setting<-function(path) {
  check.settings.result<-c()
  check.settings.issues<-list()
  check.settings.names<-c()

  load(paste(path,"/Results/NeutraliseStatus.RData",sep=""))
  setting.files<-dir(path=paste(path,"/Settings",sep=""))
  setting.exists<-
    (grepl("_settings.R",setting.files))&
    (!grepl("_settings.RData",setting.files))
  #new.setting%in%neutralise.status$file.name

  cnt<-1
  if(sum(setting.exists)>0) {
    for(setting in setting.files[setting.exists]) {
      issues<-c()
      # check existence data generator
      data.name<-strsplit(setting,"_")[[1]][1]
      data.exists<-data.name%in%neutralise.status$name[
        (neutralise.status$type=="data")&
          (neutralise.status$check)]
      if(data.exists) {
        issues<-Test_New_Setting1(path,data.name)
      }
      else {
        issues<-c(issues,
                  "Data generator for settings does not exist")
      }

      if(length(issues)==0) {
        source(paste(path,"/Data/",data.name,".R",sep=""))
        setting.name<-strsplit(setting,".R")[[1]]
        load(paste(path,"/Settings/",setting.name,"_new.RData",sep=""))
        pars<-settings[1,which(colnames(settings)!="null")]
        res<-try(Data.Generator(n1=20,n2=20,parameters = as.numeric(pars)), silent=TRUE)
        if(inherits(res, "try-error")) {
          issues<-c(issues,"error on running Data.Generator function")
        }
      }
      if(length(issues)==0) {
        neutralise.status$to.run[
          (neutralise.status$name==data.name)&
            (neutralise.status$type=="data")]<-TRUE
        neutralise.status<-neutralise.status%>%
          add_row(file.name=paste(data.name,
                                  "_settings_new.RData", sep=""),
                  name=data.name,
                  type="setting",
                  check=TRUE,
                  to.run=TRUE,
                  neutralised=NA)
      }
      if(length(issues)>0) {
        write(issues,
              file=paste(path,"/Issues/issues_",setting.name,".txt",sep=""))
        neutralise.status<-neutralise.status%>%
          add_row(file.name=paste(data.name,
                                  "_settings_new.RData", sep=""),
                  name=data.name,
                  type="setting",
                  check=FALSE,
                  to.run=FALSE,
                  neutralised=NA)
      }
      check.settings.issues[[cnt]]<-issues
      check.settings.result<-c(check.settings.result,
                               ifelse(length(issues)==0,TRUE,FALSE))
      cnt<-cnt+1
    }
  }
  if(sum(setting.exists)==0) {
    check.settings.result<-0
  }
  save(neutralise.status,
       file=paste(path,"/Results/NeutraliseStatus.RData",sep=""))

  return(list(check.settings.issues=check.settings.issues,
              check.settings.names=check.settings.names,
              check.settings.result=check.settings.result))
}

