#' Boxplot_TypeI creates a boxplot of the type I error results per method
#'
#' @param path to the NeutraliseFiles
#' @param method is a specified method (character string) or is NULL (default). When =NULL, a boxplot of all methods is made.
#' @param alpha is the significance level, default=0.05
#' @param panel is a paramter to specifiy the boxplot based on 'n'=samplesize or 'distribution'= data generation method
#' @param N is the amount of simulations the results come from (default = 10 000)
#' @param ylim are the limits of the y-axis of the boxplot
#' @return a list with 3 objects: 1) Graph = ggplot2 boxplot 2) text object that defines the results 3) a dataframe with the results the graph is based on
#' @export
Boxplot_TypeI<-function(path,method=NULL,alpha=0.05,tol=0.02,panel="",ylim=c(0,0.13),N=10000) {

  load(paste0(path,'\\Results_typeI_perdatagen.RData'))

  results = results_datagen_type1


  data = names(results)
  lowlim = optimise(function(p){(p+sqrt(p*(1-p)/N)*qnorm(alpha/2, mean = 0, sd = 1, lower.tail = FALSE)-alpha)^2}, interval=c(0,1))$minimum
  uplim = optimise(function(p){(p-sqrt(p*(1-p)/N)*qnorm(alpha/2, mean = 0, sd = 1, lower.tail = FALSE)-alpha)^2}, interval=c(0,1))$minimum

  if (length(method)==1){
    group=TRUE
  }else{
    group=FALSE
  }

  if (group){

    cnt.scenarios<-0
    cnt<-1
    pwr<-c()
    distr<-c()
    n<-c()
    results1<-data.frame()

    for(d in data) {

      if (d=='Normal2Var'){
        d='Normal'
      }

      results_method = results[[d]][results[[d]]$method==method,]

      results_method = filter_significance(results_method,alpha)

      results_method$n = results_method$n1+results_method$n2

      n<-c(n,results_method$n)

      cnt<-cnt+1

      cnt.scenarios<-cnt.scenarios+nrow(results_method)

      results1 = rbind(results1,results_method[,c(1:6,(ncol(results_method)-13):ncol(results_method))])

      # cat(paste(d,results_method[results_method$distribution==d,'id']," with a total sample size of ",results_method[results_method$distribution==d,'n'],":\n",method," has a type I error rate of ",
      #           round(results_method[results_method$distribution==d,'power'],2)," at the nominal ",
      #           alpha," level.\n",sep=""))
    }

    #boxplot(pwr,
    #        ylab=paste("Type I error rate of ",
    #                   deparse(substitute(method)),sep=""))


    txt =c(paste("On average (over all scenarios) :",method," has a type I error rate not larger than ",
                 alpha," + ",tol," in ",
                 round(100*mean(results1[,'power']<alpha+tol,na.rm = TRUE),1),
                 "% of the scenarios.\n",sep=""))


    if(panel=="") {
      p0<-ggplot(results1,aes(x="",y=power))
    }
    if(panel=="distribution") {
      p0<-ggplot(results1,aes(x=distribution,y=power))
    }
    if(panel=="n") {
      results1$n=factor(results1$n)
      p0<-ggplot(results1,aes(x=n,y=power))
    }
    graph=p0+
      geom_boxplot()+
      geom_jitter(alpha=0.6,width = 0.2,size=3, aes(colour=results1$n))+
      lims(y=ylim)+
      ylab(paste("Type I error rate"))+
                 #,{{method}},sep=""))+
      geom_hline(yintercept=alpha, linetype="dotted", colour="red")+
      geom_hline(yintercept= lowlim,linetype = 'dotted')+
      geom_hline(yintercept= uplim, linetype = 'dotted')+
      xlab("")+ theme(axis.text.x = element_text(size = 18, angle = 90),
                      axis.text.y = element_text(size = 15),
                      axis.title = element_text(size = 18),
                      strip.text.x = element_text(size = 15),
                      legend.key.size = unit(1.5, 'cm'),
                      legend.title = element_text(size=15),legend.text = element_text(size=15))+
      facet_wrap(~method,ncol=1)+
      labs(colour='Sample size (total)')
  }else{


    txt=c()
    results1<-data.frame()
    cnt.scenarios<-0
    cnt<-1
    n<-c()


    for(d in data) {

      if (d=='Normal2Var'){
        d='Normal'
      }

      results_data= results[[d]]
      method=unique(results_data$method)

      for (m in method){

        results_method = results_data[results_data$method==m,]

        results_method = filter_significance(results_method,alpha)

        results_method$n = results_method$n1+results_method$n2

        n<-c(n,results_method$n)

        cnt<-cnt+1

        cnt.scenarios<-cnt.scenarios+nrow(results_method)

        results1 = rbind(results1,results_method[,c(1:6,(ncol(results_method)-13):ncol(results_method))])

        # cat(paste(d,results_method[results_method$distribution==d,'id']," with a total sample size of ",results_method[results_method$distribution==d,'n'],":\n",method," has a type I error rate of ",
        #           round(results_method[results_method$distribution==d,'power'],2)," at the nominal ",
        #           alpha," level.\n",sep=""))

      }

      #boxplot(pwr,
      #        ylab=paste("Type I error rate of ",
      #                   deparse(substitute(method)),sep=""))

    }

    for (m in method){
      txt =c(txt,paste("On average (over all scenarios) :",m," has a type I error rate not larger than ",
                       alpha," + ",tol," in ",
                       round(100*mean(results1[results1$method==m,'power']<alpha+tol,na.rm = TRUE),1),
                       "% of the scenarios.\n",sep=""))
    }
    if(panel=="") {
      p0<-ggplot(results1,aes(x="",y=power))
    }
    if(panel=="distribution") {
      p0<-ggplot(results1,aes(x=distribution,y=power))
    }
    if(panel=="n") {
      results1$n=factor(results1$n)
      p0<-ggplot(results1,aes(x=n,y=power))
    }
    graph=p0+
      geom_boxplot()+
      geom_jitter(alpha=0.6,width = 0.2,size=3, aes(colour=results1$n))+
      lims(y=ylim)+
      ylab("Type I error rate")+
      geom_hline(yintercept=alpha, linetype="dotted", colour="red")+
      geom_hline(yintercept= lowlim,linetype = "dotted")+
      geom_hline(yintercept= uplim, linetype = "dotted") +
      xlab("")+
      facet_wrap(~method,ncol=3)+ theme(axis.text.x = element_text(size = 15, angle = 90),
                                        axis.text.y = element_text(size = 12),
                                        axis.title = element_text(size = 15),
                                        strip.text.x = element_text(size = 12),
                                        legend.key.size = unit(1.5, 'cm'),
                                        legend.title = element_text(size=12),legend.text = element_text(size=12))+
      labs(colour='Sample size (total)')

  }

  return(list(graph=graph,text=txt,data=results1))
}
