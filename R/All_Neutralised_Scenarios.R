#' @export
All_Neutralised_Scenarios<-function(path,data, type='power') {
  dir<-dir(paste("settings/",data,"_settings",sep=""))
  file<-paste("settings/",data,
              "_settings.RData",
              sep="")
  load(file)

  if (type=='power'){
    settings<-settings[settings$null==0,]
    scenarios<-subset(settings,select=-null)
    id=c(1:nrow(scenarios))
    scenarios<-cbind(id,scenarios)
  }else{
    settings<-settings[settings$null!=0,]
    scenarios<-subset(settings,select=-null)
    id=c(1:nrow(scenarios))
    scenarios<-cbind(id,scenarios)
  }
  return(scenarios)
}
