#' @export
All_Neutralised<-function(path,
                          type="method") {
  load(paste(path,"/Results/NeutraliseStatus.RData",sep=""))
  names<-neutralise.status$name[
    (neutralise.status$type==type)&
      (neutralise.status$neutralised)]
  return(names)
}
