#'
#' @export
Run_All_New_Data<-function(path,N=N) {
  load(paste(path,"/Results/NeutraliseStatus.RData",sep=""))
  new.data<-neutralise.status$file.name[
    (neutralise.status$type=="data")&
      (neutralise.status$to.run)]
  all.methods<-neutralise.status$file.name[
    (neutralise.status$type=="method")&
      (neutralise.status$neutralised)]

  for(method in all.methods) {
    method.name<-strsplit(method,".R")[[1]][1]
    filename.method<-paste(path,"/Methods/",method,sep="")
    source(filename.method)

    for(data in new.data) {
      filename.data<-paste(path,"/Data/",data,sep="")
      data.name<-strsplit(data,".R")[[1]][1]
      settings.filename<-paste(path,"/Settings/",data.name,"_settings_new.RData",sep="")#####
      load(settings.filename)
      source(filename.data)

      Run_Single_Method(path,method.name,data.name,
                        settings,filename.method,filename.data,
                        mode="all",N=N)
    }
  }
  for(data in new.data) {

    data.name<-strsplit(data,".R")[[1]][1]

    settings.filename<-paste(path,"/Settings/",data.name,"_settings_new.RData",sep="")
    load(settings.filename)

    neutralise.status$neutralised[neutralise.status$file.name==data]<-TRUE
    neutralise.status$to.run[neutralise.status$file.name==data]<-FALSE
    neutralise.status$neutralised[
      (neutralise.status$name==data.name)&
        (neutralise.status$type=="setting")]<-TRUE

    #settings.filename<-paste(path,"/Settings/",data.name,"_settings_new.RData",sep="")
    #settings.filename<-paste(path,"/Settings/",data.name,"_settings.RData",sep="")
    first.setting<-!file.exists(
      paste(path,"/Settings/",data.name,"_settings.RData",sep=""))
    if(first.setting) {
      file.rename(from=settings.filename,
                  to=paste(path,"/Settings/",data.name,"_settings.RData",sep=""))
    }
    else {
      settings0<-settings
      load(paste(path,"/Settings/",data.name,"_settings.RData",sep=""))
      settings<-rbind(settings,settings0)
      save(settings,
           file=paste(path,"/Settings/",data.name,"_settings.RData",sep=""))
      #file.remove(settings.filename)
    }
  }
  save(neutralise.status,
       file=paste(path,"/Results/NeutraliseStatus.RData",sep=""))
}
