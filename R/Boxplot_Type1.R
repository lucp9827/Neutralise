#' @export
Boxplot_TypeI<-function(path,method,alpha=0.05,tol=0.02,panel="",ylim=c(0,0.07)) {

  finished<-read.csv(paste(path,"/Results/Finished.txt",sep=""),
                     header=T)
  data<-finished$data[finished$method==method]

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
    results1[[cnt]]<-results

    x= colnames(results)
    colnr = grep(alpha,x)


    pwr<-c(pwr,results[results$null==1,colnr])
    distr<-c(distr,results$distribution[results$null==1])
    n<-c(n,results$n1[results$null==1]+results$n2[results$null==1])

    cnt<-cnt+1
    cnt.scenarios<-cnt.scenarios+length(pwr)

    db_tmp<-data.frame(pwr=pwr,distribution=distr,n=n)

    cat(paste(d,":\n",method," has on average a type I error rate of ",
              round(100*db_tmp[db$distribution_tmp==d,'pwr']/cnt.scenarios,2),"% at the nominal ",
              alpha," level.\n",sep=""))
  }

  #boxplot(pwr,
  #        ylab=paste("Type I error rate of ",
  #                   deparse(substitute(method)),sep=""))


  cat(paste("On average (over all scenarios) :",method," has a type I error rate not larger than ",
            alpha," + ",tol," in ",
            round(100*mean(pwr<alpha+tol),1),
            "% of the scenarios.\n",sep=""))

  db<-data.frame(pwr=pwr,distribution=distr,n=n)
  invisible(list(power=db,
                 results=results1))

  if(panel=="") {
    p0<-ggplot(db,aes(x="",y=pwr))
  }
  if(panel=="distribution") {
    p0<-ggplot(db,aes(x=distribution,y=pwr))
  }
  if(panel=="n") {
    p0<-ggplot(db,aes(x=n,y=pwr))
  }
  p0+
    geom_boxplot()+
    geom_jitter(alpha=0.6,width = 0.2, aes(colour=n))+
    lims(y=ylim)+
    ylab(paste("Type I error rate of ",
               {{method}},sep=""))+
    geom_hline(yintercept=alpha, linetype="dotted", colour="red")+
    xlab("")
}
