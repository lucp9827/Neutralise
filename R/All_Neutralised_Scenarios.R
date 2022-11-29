#' @export
All_Neutralised_Scenarios<-function(path,data, type='power') {
  dir<-paste("Settings/",data,"_settings",sep="")
  file<-paste(path,"/Settings/",data,
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
