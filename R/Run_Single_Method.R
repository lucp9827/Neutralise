#'
#' @export
Run_Single_Method<-function(path,method.name,data.name,
                            settings,method.filename,data.filename,
                            N,B,
                            mode="all",reproduce=TRUE) {

  sample.sizes<-matrix(ncol=2,c(
    10,10,
    20,20,
    100,100,
    10,100
  ), byrow=T)

  n.settings<-nrow(settings)
  n.parameters<-ncol(settings)
  results<-matrix(ncol=18+n.parameters,
                  nrow=n.settings*nrow(sample.sizes))
  results<-as.data.frame(results)
  names(results)<-c(c("method","distribution","seed","N","n1","n2"),
                    names(settings),"power0.01","l_CI","u_CI", "power0.05","l_CI","u_CI","power0.10","l_CI","u_CI","ct_0.10","ct_0.05","ct_0.01")

  if (reproduce==TRUE){
    load(paste0(path,"\\Results\\SimRes_",method.name,"_",data.name,".RData"))
  }
  cnt<-1
  for(nn in 1:nrow(sample.sizes)) {
    for(pp in 1:n.settings) {

      if (!(reproduce==TRUE)){
      seed<-round((as.numeric(Sys.time()))/100000*exp(runif(1,min=0.00001,max=10)),0)}
      else{
        seed=results$seed[cnt]
      }
      set.seed(seed)
      if (!is.null(B)){
        pwr<-Power_perm(n1=sample.sizes[nn,1],
                   n2=sample.sizes[nn,2],
                   parameters = as.numeric(settings[pp,-n.parameters]),
                   N=N,B=B)
      }else{ pwr<-Power(n1=sample.sizes[nn,1],
                 n2=sample.sizes[nn,2],
                 parameters = as.numeric(settings[pp,-n.parameters]),
                 N=N)}

      results[cnt,]<-c(method.name,data.name,seed,N,
                       sample.sizes[nn,1],sample.sizes[nn,2],
                       settings[pp,],pwr)
      cnt<-cnt+1
    }
  }

  #if(mode=="all") {
  #new.dir<-paste(path,"/Results/SimRes_",method.name,"_",data.name,sep="")
  #}
  #if(mode=="single") {
  #new.dir<-paste(path,"/Results/Local/SimRes_",method.name,"_",data.name,sep="")
  #}
  subdir<-ifelse(mode=="single","Local/","")
  subdir<-ifelse(reproduce==TRUE,"Repo/","")
  new.dir<-paste(path,"/Results/",subdir,"SimRes_",method.name,"_",data.name,sep="")

  if(file.exists(new.dir)) {
    l<-length(dir(paste(path,"/Results/",subdir,sep="")))
    new.dir<-paste(new.dir,"_",l+1,sep="")
  }
  dir.create(new.dir)

  filename<-paste(new.dir,"/",method.name,"_",data.name,sep="")
  filename_res<-paste(new.dir,"/",method.name,"_",data.name,sep="")

  save(results,
       file=paste(filename_res,".RData",sep=""))
  write.table(results,
              row.names = FALSE, col.names = TRUE,
              append = FALSE, dec=".", sep=",",
              file=paste(filename,".txt",sep=""))

  sink(paste(new.dir,"/SessionInfo.txt",sep=""))
  print(sessionInfo())
  sink()

  if(mode=="all") {
    # copy R functions to Results directory
    file.copy(from=method.filename,
              to=paste(new.dir,"/",method.name,".R",sep=""))
    file.copy(from=data.filename,
              to=paste(new.dir,"/",data.name,".R",sep=""))
    save(settings,
         file=paste(new.dir,"/settings.RData",sep=""))

    finished<-read.table(file=paste(path,"/Results/Finished.txt",sep=""),
                         header=TRUE, sep=",")
    newline<-data.frame(method=method.name,
                        data=data.name,
                        date=date())
    finished<-rbind(finished,newline)
    write.table(finished,
                file=paste(path,"/Results/Finished.txt",sep=""),
                sep=",",col.names = TRUE, row.names = FALSE)
  }
  if(mode=="single") {
    return(results)
  }
}
