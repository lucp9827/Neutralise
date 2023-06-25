#'
#' @export
## Function to summarize methods
sum_methods = function(){

  method.files<-dir(path=paste("methods",sep=""))
  load(paste("Results/neutralisestatus.RData",sep=""))
  method.exists<-method.files%in%neutralise.status$file.name[
    (neutralise.status$type=="method")&
      (neutralise.status$check==TRUE)]

  text=data.frame()

  for (i in (1:length(method.files))){

    filename_temp=method.files[i]

    filename<-paste("methods/",filename_temp,sep="")

    con=file(filename,"r")
    tmp<-readLines(con,n=-1)
    close(con)

    name_id = which(tmp=="# NAME")
    name_txt = tmp[name_id+1]
    name_txt = gsub('#','',name_txt)


    hep_id = which(tmp=="# HYPOTHESIS")
    hep_txt = tmp[hep_id+1]
    hep_txt = gsub('#','',hep_txt)


    des_id = which(tmp=="# DESCRIPTION")
    des_txt = tmp[des_id+1]
    des_txt = gsub('#','',des_txt)

    ref_id = which(tmp=="# REFERENCES")
    ref_txt = tmp[ref_id+1]
    ref_txt = gsub('#','',ref_txt)

    abbriv_txt=gsub('.R','',filename_temp)

    text_tmp= data.frame(Abbriviation=abbriv_txt,Name=name_txt,Hypotheses=hep_txt,Description=des_txt,References=ref_txt)

    text=rbind(text,text_tmp)
  }

  return(text)
}
