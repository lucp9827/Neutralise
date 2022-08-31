#' @export
All_Neutralised_Scenarios<-function(path,method,data,type) {
  dir<-dir(paste(path,"/Results/SimRes_",method,"_",data,sep=""))
  file<-paste(path,"/Results/SimRes_",method,"_",data,
              "/",dir[grepl(".RData",dir)&grepl(method,dir)],
              sep="")
  load(file)

  if (type=='power'){
  results<-results[results$null==0,]
  scenarios<-unique(
    results%>%select(!c(method,distribution,seed,null,N,power0.01,power0.05,power0.10,l_CI,u_CI)))
  }else{
    results<-results[results$null==1,]
    scenarios<-unique(
      results%>%select(!c(method,distribution,seed,null,N,power0.01,power0.05,power0.10,l_CI,u_CI)))
  }
  return(scenarios)
}
