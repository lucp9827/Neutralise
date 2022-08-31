#' @export
#' 
Power_curve<-function(path,method,data,alpha=0.05,
par.fix=NULL,
col="black",CI=FALSE) { 
  
  dir1<-dir(paste(path,"/Results/SimRes_",method,"_",data,sep=""))
  file1<-paste(path,"/Results/SimRes_",method,"_",data,
               "/",dir1[grepl(".RData",dir1)&grepl(method,dir1)],
               sep="")
  
  load(file1)
  
  if(!is.null(par.fix)) {
    settings.fix<-results%>%dplyr::select(names(par.fix))
    results<-results[apply(settings.fix,1,
                                 function(x) {
                                   all(x==unlist(par.fix))
                                 }),]}
    
    results$n=results$n1 + results$n2
    results$n=as.factor(results$n)
    
    results= filter_significance(results,alpha)
    
    if (CI){
      graph=ggplot(results,aes(x=delta,y=power, group=n))+
        geom_line(aes(col=n))+
        geom_point(aes(col=n))+
        geom_errorbar(aes(ymin = l_CI, ymax = u_CI,col=n), width = 0.2)+
        ylim(0:1)+
        facet_wrap(~method,ncol=3)+ theme(axis.text.x = element_text(size = 15),
                                          axis.text.y = element_text(size = 15),
                                          axis.title = element_text(size = 18),
                                          strip.text.x = element_text(size = 15),
                                          legend.key.size = unit(1.5, 'cm'),
                                          legend.title = element_text(size=15),legend.text = element_text(size=15))+
        labs(colour='Sample size (total)')}
    else{  graph=ggplot(results,aes(x=delta,y=power, group=n))+
      geom_line(aes(col=n))+
      geom_point(aes(col=n))+
      ylim(0:1)+
      facet_wrap(~method,ncol=3)+ theme(axis.text.x = element_text(size = 15),
                                        axis.text.y = element_text(size = 15),
                                        axis.title = element_text(size = 18),
                                        strip.text.x = element_text(size = 15),
                                        legend.key.size = unit(1.5, 'cm'),
                                        legend.title = element_text(size=15),legend.text = element_text(size=15))+
      labs(colour='Sample size (total)')
    
    }
    
    return(list(graph))
  
}
  