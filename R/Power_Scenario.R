#' @export
Power_Scenario<-function(path,method,data,par.x,par.fix,alpha=0.05,add.to.plot=NULL,
                         col="black",linetype="solid") {
  
  dir<-dir(paste(path,"/Results/SimRes_",method,"_",data,sep=""))
  file<-paste(path,"/Results/SimRes_",method,"_",data,
              "/",dir[grepl(".RData",dir)&grepl(method,dir)],
              sep="")
  load(file)
  settings.fix<-results%>%select(names(par.fix))
  results.fix<-results[apply(settings.fix,1,
                             function(x) {
                               all(x==unlist(par.fix))
                             }),]
  
  if(is.null(add.to.plot)) {
    p<-ggplot(results.fix,aes(x={{par.x}},y=power.05))+
      geom_line(linetype={{linetype}}, colour={{col}})+
      geom_point(colour={{col}})+
      ylim(0,1)+
      ylab("power")
  }
  else {
    p<-add.to.plot+
      geom_line(data=results.fix,aes(x={{par.x}},y=power.05),
                linetype={{linetype}}, colour={{col}})+
      geom_point(data=results.fix,aes(x={{par.x}},y=power.05),
                 colour={{col}})
  }
  p
  
  invisible(p)
}