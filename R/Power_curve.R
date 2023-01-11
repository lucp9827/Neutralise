#' @export
Power_curve<-function(path,methods=NULL,alpha=0.05,
                          par.fix=NULL,
                          data=NULL,
                          col="black",CI=FALSE,N=10000) {


  load(paste0(path,'\\Results_typeI_perdatagen.RData'))
  results = results_datagen_type1

  load(paste0(path,'\\Results_power_perdatagen.RData'))
  results_list = results_datagen

  results_list = filter_type1(path,results,results_power=results_list,alpha,N=N)$filter_list

  if (is.null(methods)){
    group=TRUE
  }else{
    group=FALSE
  }

  # Read finished file
  if (is.null(data)){
    data.i<-names(results_list)
  }else{
    data.i=data
  }


  if (group){
    load(paste0(path,file = "\\Results\\NeutraliseStatus.RData"))
    methods=neutralise.status[neutralise.status$type=='method','name']
  }

  end=data.frame()

  for(d in data.i) {


    #results1<-list()
    results1<-data.frame()
    win2<-0
    cnt.scenarios<-0
    pwr1<-data.frame()
    data.gen<-c()

    for (m in methods){

      # specify folder in results file
      results_dm = results_list[[d]][results_list[[d]]$method==m,]


      if(!is.null(par.fix)) {
        settings.fix<-results_dm%>%dplyr::select(names(par.fix))
        results_dm<-results_dm[apply(settings.fix,1,
                                     function(x) {
                                       all(x==unlist(par.fix))
                                     }),]
      }

      results1<-rbind(results1,results_dm)

    }


    end_tmp=data.frame(method=results1$method,
                       data.gen=results1$distribution,n=results1$n,delta=results1$delta,power=results1$power,l_CI=results1$l_CI,u_CI=results1$u_CI)

    end=rbind(end,end_tmp)

  }

  end$n=as.factor(end$n)
  end=remove_missing(end)
  if (CI){
    graph=ggplot(end,aes(x=delta,y=power, group=n))+
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
  else{  graph=ggplot(end,aes(x=delta,y=power, group=n))+
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
