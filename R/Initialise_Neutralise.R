#' Initialise_Neutralise function to initilize/create the *Finished.txt* and *neutralise_status.Rfile*
#'
#' @param path is the directory path to NeutraliseFiles
#' @return Two files in the Results directory: 1) *Finished.txt* which contains information on which evaluations are done 2) *neutralise_status.Rfile*, which contains information on the status of the different files. The 'check' variable defines if the file has passed the Neutralise format check, the 'to.run' variable identifies if a files still needs to be evaluated, and the 'neutralised' variable defines which files has been evaluated successfully.
#' @export
Initialise_Neutralise<-function(path,reproduce=FALSE) {
  neutralise.status<-data.frame(
    file.name="foo",
    name="foo",
    type="foo",  # "method", "data" or "setting"
    check=FALSE,
    to.run=FALSE,
    neutralised=FALSE
  )
if (reproduce==FALSE){
  save(neutralise.status,
       file=paste(path,"/Results/NeutraliseStatus.RData",sep=""))

  finished<-data.frame(
    method="NULL",
    data="NULL",
    date=date()
  )
  write.table(finished,
              file=paste(path,"/Results/Finished.txt",sep=""),
              sep=",",col.names = TRUE, row.names = FALSE)}
  else{
    save(neutralise.status,
         file=paste(path,"/NeutraliseStatus.RData",sep=""))

    finished<-data.frame(
      method="NULL",
      data="NULL",
      date=date()
    )
    write.table(finished,
                file=paste(path,"/Finished.txt",sep=""),
                sep=",",col.names = TRUE, row.names = FALSE)

  }

}
