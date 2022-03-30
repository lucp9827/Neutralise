Test_New_Setting1<-function(path,data.name) {
  check.settings.result<-c()
  check.settings.issues<-list()
  check.settings.names<-c()

  #cnt<-1
  #new.setting<-dir(path=paste(path,"/Settings",sep=""))
  setting.file.name<-paste(data.name,"_settings.R",sep="")
  #setting.exists<-setting.file.name%in%new.setting
  #
  #if(!setting.exists) {
  #  issues<-c(issues,
  #            "No settings specified for data generator")
  #}

  issues<-c()
  #setting.name<-strsplit(setting,".R")[[1]]

  # if(exists("settings")) {
  #   remove("settings")
  # }

  filename<-paste(path,"/Settings/",
                  setting.file.name,sep="")
  #source(filename)
  load(filename)

  if(!exists("settings")) {
    issues<-c(issues,"no settings are specified (or at least not with the proper name)")
  }
  if(exists("settings")) {
    if(!is.data.frame(settings)) {
      issues<-c(issues,"settings is not given as a data frame")
    }
    if(is.data.frame(settings)) {
      if(!("null"%in%names(settings))) {
        issues<-c(issues,"The null column is missing in settings")
      }
      else {
        if(min(settings$null%in%c(0,1))==0) {
          issues<-c(issues,"The null column of settings must only contain 0s and 1s")
        }
      }
    }


    if(length(issues)==0) {
      save(settings,
           file=paste(path,"/Settings/",
                      data.name,
                      "_settings_new.RData",sep=""))
      file.remove(filename)
    }
    #if(length(issues)>0) {
    #  write(issues,
    #        file=paste(path,"/Issues/issues_",setting.name,".txt",sep=""))
    #}
  }
  return(issues)
}
