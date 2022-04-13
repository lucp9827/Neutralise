#' @export
All_Neutralised_Scenarios<-function(path,method,data) {
  dir<-dir(paste(path,"/Results/SimRes_",method,"_",data,sep=""))
  file<-paste(path,"/Results/SimRes_",method,"_",data,
              "/",dir[grepl(".RData",dir)&grepl(method,dir)],
              sep="")
  load(file)
  results<-results[results$null==0,]
  scenarios<-unique(
    results%>%select(!c(method,distribution,seed,null,N,power.01,power.05,power.10)))
  
  return(scenarios)
}