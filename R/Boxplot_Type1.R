#' @export
Boxplot_TypeI<-function(path,method,alpha=0.05,tol=0.02,panel="",ylim=c(0,0.13)) {

  finished<-read.csv(paste(path,"/Results/Finished.txt",sep=""),
                     header=T)
  data<-finished$data[finished$method==method]

  lowlim = optimise(function(p){(p+sqrt(p*(1-p)/1000)*qnorm(alpha/2, mean = 0, sd = 1, lower.tail = FALSE)-alpha)^2}, interval=c(0,1))$minimum
  uplim = optimise(function(p){(p-sqrt(p*(1-p)/1000)*qnorm(alpha/2, mean = 0, sd = 1, lower.tail = FALSE)-alpha)^2}, interval=c(0,1))$minimum

  cnt.scenarios<-0
  cnt<-1
  pwr<-c()
  distr<-c()
  n<-c()
  results1<-list()
  for(d in data) {


    dir1<-dir(paste(path,"/Results/SimRes_",method,"_",d,sep=""))
    file1<-paste(path,"/Results/SimRes_",method,"_",d,
                 "/",dir1[grepl(".RData",dir1)&grepl(method,dir1)],
                 sep="")

    load(file1)
    results1[[d]]<-results

    #results = filter_type1(d,results,alpha,path)

    x= colnames(results)
    colnr = grep(alpha,x)


    pwr<-c(pwr,results[results$null==1,colnr])
    distr<-c(distr,results$distribution[results$null==1])
    n<-c(n,results$n1[results$null==1]+results$n2[results$null==1])

    cnt<-cnt+1
    cnt.scenarios<-cnt.scenarios+length(pwr)

    db_tmp<-data.frame(pwr=pwr,distribution=distr,n=n)

    # cat(paste(d," with a total sample size of ",db_tmp[db_tmp$distribution==d,'n'],":\n",method," has on average a type I error rate of ",
    #           round(100*db_tmp[db_tmp$distribution==d,'pwr']/cnt.scenarios,2),"% at the nominal ",
    #           alpha," level.\n",sep=""))
  }

  #boxplot(pwr,
  #        ylab=paste("Type I error rate of ",
  #                   deparse(substitute(method)),sep=""))


  cat(paste("On average (over all scenarios) :",method," has a type I error rate not larger than ",
            alpha," + ",tol," in ",
            round(100*mean(pwr<alpha+tol),1),
            "% of the scenarios.\n",sep=""))

  db<-data.frame(pwr=pwr,distribution=distr,n=n)
  invisible(list(results=results1)) #power=db,

  if(panel=="") {
    p0<-ggplot(db,aes(x="",y=pwr))
  }
  if(panel=="distribution") {
    p0<-ggplot(db,aes(x=distribution,y=pwr))
  }
  if(panel=="n") {
    p0<-ggplot(db,aes(x=factor(n),y=pwr))
  }
  graph=p0+
    geom_boxplot()+
    geom_jitter(alpha=0.6,width = 0.2, aes(colour=n))+
    lims(y=ylim)+
    ylab(paste("Type I error rate of ",
               {{method}},sep=""))+
    geom_hline(yintercept=alpha, linetype="dotted", colour="red")+
    geom_hline(yintercept= lowlim,linetype = "dotted")+
    geom_hline(yintercept= uplim, linetype = "dotted") +
    xlab("")+theme(axis.text.x = element_text(size = 18, angle = 90),
                   axis.text.y = element_text(size = 15),
                   axis.title = element_text(size = 18),
                   strip.text.x = element_text(size = 15),
                   legend.key.size = unit(1, 'cm'),
                   legend.title = element_text(size=15),legend.text = element_text(size=15))


}
