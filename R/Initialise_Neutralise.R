Initialise_Neutralise<-function(path) {
  neutralise.status<-data.frame(
    file.name="foo",
    name="foo",
    type="foo",  # "method", "data" or "setting"
    check=FALSE,
    to.run=FALSE,
    neutralised=FALSE
  )
  
  save(neutralise.status,
       file=paste(path,"/Results/NeutraliseStatus.RData",sep=""))
  
  finished<-data.frame(
    method="NULL",
    data="NULL",
    date=date()
  )
  write.table(finished,
              file=paste(path,"/Results/Finished.txt",sep=""),
              sep=",",col.names = TRUE, row.names = FALSE)
}