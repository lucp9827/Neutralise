#' Power_QQ creates a plot that compare the power results of 2 specified methods
#'
#' @param path is the path to NeutraliseFiles
#' @param method1 is a character string that specifies a method
#' @param method2 is a character string that specifies a second method
#' @param alpha is the significance level (default = 0.05)
#' @param N is the amount of simulations (default = 10000)
#' @param group is a logical variable that indicates to color the observations based on data generation method
#' @param data is a character string that defines a data generation method (default = NULL)
#' @param par.fix is a data frame that specifies a specific setting parameter (default =NULL)
#' @return a list object with 6 elements. 1) amount of times method1 has a higher power than method2 2) results1 - results data frame of method 1 3) results2 - results data frame of method 2 4) total - data frame of all the results for the graph 5) graph 6) text string that summarizes the results
#' @export
Power_QQ<-function(path,method1,method2,alpha=0.05,
                   par.fix=NULL,
                   data=NULL,
                   col="black",group=FALSE,N=10000) {
  # data = data generation tool

  load(paste0(path,'\\Results_typeI_perdatagen.RData'))
  results = results_datagen_type1

  load(paste0(path,'\\Results_power_perdatagen.RData'))
  results_list = results_datagen

  results_list = filter_type1(path,results,results_power=results_list,alpha,N=N)$filter_list

  # Read finished file
  finished<-read.csv(paste0(path,"\\Results/Finished.txt"),sep=",",header=T)

  # Save data generation methods ran per specified method

  if (is.null(data)){

    data1<-finished$data[finished$method==method1]
    data2<-finished$data[finished$method==method2]
    data.i<-intersect(data1,data2)

  }else {

    data.i=data
  }




  results1<-list()
  results2<-list()
  total_tmp=data.frame()
  win2<-0
  cnt.scenarios<-0
  cnt<-1
  text_group=c()

  for(d in data.i) {

    results1_tmp = results_list[[d]][results_list[[d]]$method==method1,]
    results2_tmp = results_list[[d]][results_list[[d]]$method==method2,]


    if(!is.null(data)) {
      results1_tmp<-results1_tmp[results1_tmp$distribution==d,]
      results2_tmp<-results2_tmp[results2_tmp$distribution==d,]
    }

    if(!is.null(par.fix))  {
      settings.fix<-results1_tmp%>%dplyr::select(names(par.fix))
      results1_tmp<-results1_tmp[apply(settings.fix,1,
                                       function(x) {
                                         all(x==unlist(par.fix))
                                       }),]

      settings.fix<-results2_tmp%>%dplyr::select(names(par.fix))
      results2_tmp<-results2_tmp[apply(settings.fix,1,
                                       function(x) {
                                         all(x==unlist(par.fix))
                                       }),]
    }

    results1[[d]]<-results1_tmp
    results2[[d]]<-results2_tmp

    cnt<-cnt+1

    results1_tmp$scenario = paste(results1_tmp$distribution,results1_tmp$id)
    results2_tmp$scenario = paste(results2_tmp$distribution,results2_tmp$id)

    results1_tmp = results1_tmp[,c(1:7,which(colnames(results1_tmp)=='power'):length(colnames(results1_tmp)))]
    results2_tmp = results2_tmp[,c(1:7,which(colnames(results2_tmp)=='power'):length(colnames(results2_tmp)))]

    if (!length(results1_tmp$scenario)==length(results2_tmp$scenario)){
      rownames(results1_tmp$scenario)=NULL
      rownames(results2_tmp$scenario)=NULL
      tt1 = merge(results1_tmp,results2_tmp,by=c('scenario','n'))
    }else{
      colnames(results1_tmp) = paste0(colnames(results1_tmp),".x")
      colnames(results2_tmp) = paste0(colnames(results2_tmp),".y")
      names(results2_tmp)[names(results2_tmp)=='n.y'] <- 'n'
      results1_tmp= subset(results1_tmp,select=-n.x)
      names(results2_tmp)[names(results2_tmp)=='scenario.y'] <- 'scenario'
      results1_tmp= subset(results1_tmp,select=-scenario.x)

      tt1=cbind(results1_tmp,results2_tmp)
    }


    tt2 = remove_missing(tt1)

    win2<-win2+sum(tt2[,'power.x'] < tt2[,'power.y'])
    cnt.scenarios<-cnt.scenarios+length(tt2[,'power.x'] < tt2[,'power.y'])

    if (group==TRUE){
      win_tmp = sum(tt2[,'power.x'] < tt2[,'power.y'])
      text_group=c(text_group,paste(d,": ",method2," wins over ",method1, " in ", round(100*win_tmp/length(tt2[,'power.x'] < tt2[,'power.y']),1),
                                    "% of the ",length(tt2[,'power.x'] < tt2[,'power.y']), " scenarios\b",sep=""))
    }

    total_tmp = rbind(total_tmp,tt2)
  }


  txt=(paste(method2," wins over ",method1, " in ", round(100*win2/cnt.scenarios,1),
             "% of the ", cnt.scenarios, " scenarios\n",sep=""))

  if (!group){
    if (dim(total_tmp)[1]==0){
      p<-print("No scenarios where both methods control the Type I error")
    }else{
    p<-ggplot(total_tmp,aes(x=power.x,y=power.y))+
      geom_point(colour=col,size=4)+
      ylim(0,1)+xlim(0,1)+
      geom_abline()+
      xlab(paste("power of",method1,sep=" "))+
      ylab(paste("power of",method2,sep=" "))+
      theme(axis.text.x = element_text(size = 15),
                                                     axis.text.y = element_text(size = 15),
                                                     axis.title = element_text(size = 20),
                                                     legend.key.size = unit(1, 'cm'),
                                                     legend.title = element_text(size=15),legend.text = element_text(size=15))
  }}else{
    if (dim(total_tmp)[1]==0){
      p<-print("No scenarios where both methods control the Type I error")
    }else{
    p<- ggplot(total_tmp,aes(x=power.x,y=power.y))+
      geom_point(aes(colour=factor(distribution.x)),size=4)+
      ylim(0,1)+xlim(0,1)+
      geom_abline()+
      xlab(paste("Power of",method1,sep=" "))+
      ylab(paste("Power of",method2 ,sep=" "))+
      labs(colour='Data generation method')+ theme(axis.text.x = element_text(size = 15),
                                                   axis.text.y = element_text(size = 15),
                                                   strip.text = element_text(size=15),
                                                 axis.title = element_text(size = 18),
                                                   legend.key.size = unit(1, 'cm'),
                                                   legend.title = element_text(size=15),legend.text = element_text(size=15))
  }}

  if (group){
    txt=text_group
  }


  (list(win.pct=win2/cnt.scenarios,
        results1=results1,
        results2=results2,
        total=total_tmp,
        graph=p,
        text=txt))

}
