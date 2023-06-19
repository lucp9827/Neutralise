#' Power_curve creates a curve plot of the power in function of delta
#'
#' @param path is the path to NeutraliseFiles
#' @param methods is a character string that specifies a method (default =NULL, when =NULL a power curve is made for all methods seperatly)
#' @param alpha is the significance level (default = 0.05)
#' @param N is the amount of simulations (default = 10000)
#' @param data is a character string that defines a data generation method (default = NULL, its necessary to specify this as the power-cruve will be unclear)
#' @param par.fix is a data frame that specifies a specific setting parameter (default =NULL)
#' @param CI is a logical variable, when =TRUE the CI for the observations wil be plotted
#' @return Power curve
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


  end = list()
  datgen = c(names(results_list))
  for (d in datgen){

    data_power_dist = results_list[[d]]

    if (d=='Normal2Var'){
      data_type1_dist = results_datagen_type1[['Normal']]
    }else{
      data_type1_dist = results_datagen_type1[[d]]}
    data_type1_dist = filter_significance(data_type1_dist,alpha)

    uplim = optimise(function(p){(p-sqrt(p*(1-p)/N)*qnorm(alpha/2, mean = 0, sd = 1, lower.tail = FALSE)-alpha)^2}, interval=c(0,1))$minimum

    data_type1_dist$control= data_type1_dist$power<=uplim
    data_type1_dist = data_type1_dist[data_type1_dist$control==TRUE,]

    ind=c()
    colnr_1 = grep('delta',colnames(data_power_dist))+1
    colnr_2 = grep('power',colnames(data_power_dist))-1
    colnr_n = grep('cnt',colnames(data_power_dist))+1
    colnr_m = grep('method', colnames(data_type1_dist))
    if(colnr_2-colnr_1>2){
      ind=c(colnr_1,colnr_1+1,colnr_n)
    }else{ind = c(colnr_1,colnr_n)}
    data_type1_dist = remove_missing(data_type1_dist)
    data_power_dist = remove_missing(data_power_dist)
    data_power_dist_orig = data_power_dist


    for (i in (1:nrow(unique(data_power_dist[,ind])))){

      if(d=="Normal2Var"){
        df1= data.frame(unique(data_power_dist[,c(ind[1],ind[2])])[i,])
      }else{
        df1= data.frame(unique(data_power_dist[,ind])[i,])}
      settings.fix<-data_power_dist%>%dplyr::select(names(df1))
      data_power_dist_1<-data_power_dist[apply(settings.fix,1,
                                               function(x) {
                                                 all(x==unlist(df1))
                                               }),]

      methodstt = unique(data_power_dist_1$method)
      for (m in methodstt){
        if(m=='Gastwirth'){next}

        data_tt = data_power_dist_1[data_power_dist_1$method==m,]

        if(colnr_1==colnr_2){
          extra_0 = data.frame(unique(data_tt[,colnr_1]))
          rownames(extra_0) = rownames(data_tt)[1]
        }else{
          if(colnr_2-colnr_1>2){
            extra_0 = data.frame(unique(data_tt[,c(colnr_1,colnr_1+1,colnr_1+2)]))
          }else{
            extra_0 = data.frame(unique(data_tt[,c(colnr_1,colnr_2)]))
          }

        }

        t1 = data.frame(rn=rownames(data_tt))

        t2= c(rownames(extra_0))
        data_extra= data_tt[match(t2, t1$rn),]



        data_extra$delta=0

        if ((colnr_2-colnr_1)<=2){
          data_typeI_meth = data_type1_dist[data_type1_dist$method==m,]
          power_add = data_typeI_meth[data_typeI_meth[,c(colnr_1)] == data_extra[1,colnr_1],]
          if(d=="Normal2Var"){power_add= power_add[power_add[,colnr_n-1]==data_extra$n[1],]}else{
            power_add= power_add[power_add[,colnr_n]==data_extra$n[1],]}
        }else{

          data_typeI_meth = data_type1_dist[data_type1_dist$method==m,]
          power_add = data_typeI_meth[data_typeI_meth[,c(colnr_1)] == data_extra[1,colnr_1],]
          power_add = power_add[power_add[,c(colnr_1+1)] == data_extra[1,colnr_1+1],]
          power_add= power_add[power_add[,colnr_n]==data_extra$n[1],]
        }


        data_extra$power = power_add$power
        data_extra$l_CI = power_add$l_CI
        data_extra$u_CI = power_add$u_CI
        data_power_dist_orig = rbind(data_power_dist_orig, data_extra)
      }

    }
    end[[d]]= data_power_dist_orig
  }


  results_list = end
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
    methods=unlist(c(methods))
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
      geom_line(aes(col=n),size=1.3)+
      geom_point(aes(col=n))+
      geom_errorbar(aes(ymin = l_CI, ymax = u_CI,col=n), width = 0.3,size=1.3)+
      ylim(0:1)+
      xlab("Difference in means")+
      ylab("Power")+
      facet_wrap(~method,ncol=3)+ theme(axis.text.x = element_text(size = 15),
                                        axis.text.y = element_text(size = 15),
                                        axis.title = element_text(size = 18),
                                        strip.text.x = element_text(size = 15),
                                        legend.key.size = unit(1.5, 'cm'),
                                        legend.title = element_text(size=15),legend.text = element_text(size=15))+
      labs(colour='Sample size (total)')}
  else{  graph=ggplot(end,aes(x=delta,y=power, group=n))+
    geom_line(aes(col=n),size=1.3)+
    geom_point(aes(col=n))+
    ylim(0:1)+
    xlab("Difference in means")+
    ylab("Power")+
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
