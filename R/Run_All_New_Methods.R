#'
#' @export
Run_All_New_Methods<-function(path,N=N,B=B) {
  load(paste(path,"/Results/NeutraliseStatus.RData",sep=""))
  new.methods<-neutralise.status$file.name[
    (neutralise.status$type=="method")&
      (neutralise.status$to.run)]
  all.data<-neutralise.status$file.name[
    (neutralise.status$type=="data")&
      (neutralise.status$neutralised)]

  for(method in new.methods) {
    method.name<-strsplit(method,".R")[[1]][1]
    filename.method<-paste(path,"/Methods/",method,sep="")
    source(filename.method)

    for(data in all.data) {
      filename.data<-paste(path,"/Data/",data,sep="")
      data.name<-strsplit(data,".R")[[1]][1]
      source(filename.data)
      load(paste(path,"/Settings/",
                 data.name,"_settings.RData",sep=""))
      Run_Single_Method(path,method.name,data.name,
                        settings,filename.method,filename.data,
                        mode="all",
                        N=N,B=B)
    }

    neutralise.status$neutralised[
      neutralise.status$file.name==method]<-TRUE
    neutralise.status$to.run[
      neutralise.status$file.name==method]<-FALSE

    save(neutralise.status,
         file=paste(path,"/Results/NeutraliseStatus.RData",sep=""))

  }
}
