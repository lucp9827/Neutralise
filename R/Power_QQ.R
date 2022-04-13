#' @export
Power_QQ<-function(path,method1,method2,alpha=0.05,
                   par.fix=NULL,
                   data=NULL,
                   col="black", add.to.plot=NULL) {
  finished<-read.csv(paste(path,"/Results/Finished.txt",sep=""),
                     header=T)
  data1<-finished$data[finished$method==method1]
  data2<-finished$data[finished$method==method2]
  data.i<-intersect(data1,data2)
  
  results1<-list()
  results2<-list()
  
  win2<-0
  cnt.scenarios<-0
  cnt<-1
  pwr1<-c()
  pwr2<-c()
  for(d in data.i) {
    dir1<-dir(paste(path,"/Results/SimRes_",method1,"_",d,sep=""))
    dir2<-dir(paste(path,"/Results/SimRes_",method2,"_",d,sep=""))
    file1<-paste(path,"/Results/SimRes_",method1,"_",d,
                 "/",dir1[grepl(".RData",dir1)&grepl(method1,dir1)],
                 sep="")
    file2<-paste(path,"/Results/SimRes_",method2,"_",d,
                 "/",dir2[grepl(".RData",dir2)&grepl(method2,dir2)],
                 sep="")
    
    load(file1)
    if(!is.null(data)) {
      results<-results[results$distribution==data,]
    }
    settings.fix<-results%>%select(names(par.fix))
    
    if(!is.null(par.fix)) {
      results<-results[apply(settings.fix,1,
                             function(x) {
                               all(x==unlist(par.fix))
                             }),]
    }
    results1[[cnt]]<-results
    pwr1.tmp<-results$power.05[results$null==0]
    
    load(file2)
    if(!is.null(data)) {
      results<-results[results$distribution==data,]
    }
    settings.fix<-results%>%select(names(par.fix))
    if(!is.null(par.fix)) {
      results<-results[apply(settings.fix,1,
                             function(x) {
                               all(x==unlist(par.fix))
                             }),]
    }
    results2[[cnt]]<-results
    pwr2.tmp<-results$power.05[results$null==0]
    
    cnt<-cnt+1
    
    win2<-win2+sum(pwr1.tmp<pwr2.tmp)
    cnt.scenarios<-cnt.scenarios+length(pwr1.tmp)
    
    pwr1<-c(pwr1,pwr1.tmp)
    pwr2<-c(pwr2,pwr2.tmp)
  }
  powers<-data.frame(pwr1=pwr1,pwr2=pwr2)
  
  cat(paste(method2," wins over ",method1, " in ", round(100*win2/cnt.scenarios,1),
            "% of the ", cnt.scenarios, " scenarios",sep=""))
  
  if(is.null(add.to.plot)) {
    p<-ggplot(powers,aes(x=pwr1,y=pwr2))+
      geom_point(colour={{col}})+
      ylim(0,1)+xlim(0,1)+
      geom_abline()+
      xlab(paste("power of ",deparse(substitute(method1)),sep=""))+
      ylab(paste("power of ",deparse(substitute(method2)),sep=""))
  }
  else {
    p<-add.to.plot+
      geom_point(data=powers,aes(x=pwr1,y=pwr2),
                 colour={{col}})
  }
  p
  
  invisible(list(win.pct=win2/cnt.scenarios,
                 power=powers,
                 results1=results1,
                 results2=results2,
                 graph=p))
  
}