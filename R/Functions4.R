Neutralise<-function(path,
                     Test=NULL, 
                     Data.Generator=NULL,
                     settings=NULL, B=10000, N=1000) {
  
  if(is.null(Test)&is.null(Data.Generator)&is.null(settings)) {
    # run code that checks directories to see what need to be run: Run_All
    Run_All(path)
  }
  
  if((!is.null(Test))&(!is.null(Data.Generator))&(!is.null(settings))) {
    assign("Test",Test, envir = .GlobalEnv)
    assign("Data.Generator",Data.Generator, envir = .GlobalEnv)
    assign("settings",settings,envir = .GlobalEnv)
    
    res<-Run_Single_Method(path,
                           method.name = deparse(substitute(Test)),
                           data.name = deparse(substitute(Data.Generator)),
                           settings = settings,
                           N=N,
                           mode="single")
  }
}


Initialise_Neutralise<-function(path) {
  local.file.name<-paste(path,"/NeutraliseFiles.zip",sep="")
  download.file(url = "https://github.com/othas/neutralise/blob/main/NeutraliseFiles.zip",
                destfile = local.file.name)
  
  path2<-paste(path,"/NeutraliseFiles",sep="")
  
  #zip::unzip(zipfile = local.file.name,
  #      exdir = path2)
  
  return(path2)
}


Run_All<-function(path) {
  test.method<-Test_New_Method(path)
  if(max(test.method$check.method.result)==1) {
    # run new tests on all the data
    Run_All_New_Methods(path)
  }
  else {
    # call function to report the issues
  }
  
  test.data<-Test_New_Data(path)
  if(max(test.data$check.data.result)==1) {
    # run all tests on new data
    Run_All_New_Data(path)
  }
  else {
    # call function to report the issues
  }
  
  test.settings<-Test_New_Setting(path)
  if(max(test.settings$check.settings.result)==1) {
    # run all tests on new data
    Run_All_New_Data(path)
  }
  else {
    # call function to report the issues
  }
  
}





Test_New_Setting<-function(path) {
  check.settings.result<-c()
  check.settings.issues<-list()
  check.settings.names<-c()
  
  load(paste(path,"/Results/NeutraliseStatus.RData",sep=""))
  setting.files<-dir(path=paste(path,"/Settings",sep=""))
  setting.exists<-
    (grepl("_settings.R",setting.files))&
    (!grepl("_settings.RData",setting.files))
  #new.setting%in%neutralise.status$file.name
  
  cnt<-1
  if(sum(setting.exists)>0) {
    for(setting in setting.files[setting.exists]) {
      issues<-c()
      # check existence data generator
      data.name<-strsplit(setting,"_")[[1]][1]
      data.exists<-data.name%in%neutralise.status$name[
        (neutralise.status$type=="data")&
          (neutralise.status$check)]
      if(data.exists) {
        issues<-Test_New_Setting1(path,data.name)
      }
      else {
        issues<-c(issues,
                  "Data generator for settings does not exist")
      }
      
      if(length(issues)==0) {
        source(paste(path,"/Data/",data.name,".R",sep=""))
        setting.name<-strsplit(setting,".R")[[1]]
        load(paste(path,"/Settings/",setting.name,"_new.RData",sep=""))
        pars<-settings[1,which(colnames(settings)!="null")]
        res<-try(Data.Generator(n1=20,n2=20,parameters = as.numeric(pars)), silent=TRUE)
        if(inherits(res, "try-error")) {
          issues<-c(issues,"error on running Data.Generator function")
        }
      }
      if(length(issues)==0) {
        neutralise.status$to.run[
          (neutralise.status$name==data.name)&
            (neutralise.status$type=="data")]<-TRUE
        neutralise.status<-neutralise.status%>%
          add_row(file.name=paste(data.name,
                                  "_settings_new.RData", sep=""),
                  name=data.name,
                  type="setting",
                  check=TRUE,
                  to.run=TRUE,
                  neutralised=NA)
      }
      if(length(issues)>0) {
        write(issues,
              file=paste(path,"/Issues/issues_",setting.name,".txt",sep=""))
        neutralise.status<-neutralise.status%>%
          add_row(file.name=paste(data.name,
                                  "_settings_new.RData", sep=""),
                  name=data.name,
                  type="setting",
                  check=FALSE,
                  to.run=FALSE,
                  neutralised=NA)
      }
      check.settings.issues[[cnt]]<-issues
      check.settings.result<-c(check.settings.result,
                               ifelse(length(issues)==0,TRUE,FALSE))
      cnt<-cnt+1
    }
  }
  if(sum(setting.exists)==0) {
    check.settings.result<-0
  }
  save(neutralise.status,
       file=paste(path,"/Results/NeutraliseStatus.RData",sep=""))
  
  return(list(check.settings.issues=check.settings.issues,
              check.settings.names=check.settings.names,
              check.settings.result=check.settings.result))
}



Test_New_Setting1<-function(path,data.name) {
  check.settings.result<-c()
  check.settings.issues<-list()
  check.settings.names<-c()
  
  #cnt<-1
  #new.setting<-dir(path=paste(path,"/Settings",sep=""))
  setting.file.name<-paste(data.name,"_settings.R",sep="")
  #setting.exists<-setting.file.name%in%new.setting
  #
  #if(!setting.exists) {
  #  issues<-c(issues,
  #            "No settings specified for data generator")
  #}
  
    issues<-c()
    #setting.name<-strsplit(setting,".R")[[1]]
    
    if(exists("settings")) {
      remove("settings")
    }
    
    filename<-paste(path,"/Settings/",
                    setting.file.name,sep="")
    source(filename)
    
    if(!exists("settings")) {
      issues<-c(issues,"no settings are specified (or at least not with the proper name)")
    }
    if(exists("settings")) {
      if(!is.data.frame(settings)) {
        issues<-c(issues,"settings is not given as a data frame")
      }
      if(is.data.frame(settings)) {
        if(!("null"%in%names(settings))) {
          issues<-c(issues,"The null column is missing in settings")
        }
        else {
          if(min(settings$null%in%c(0,1))==0) {
            issues<-c(issues,"The null column of settings must only contain 0s and 1s")
          }
        }
      }
    
    
    if(length(issues)==0) {
      save(settings,
           file=paste(path,"/Settings/",
                      data.name,
                      "_settings_new.RData",sep=""))
      file.remove(filename)
    }
    #if(length(issues)>0) {
    #  write(issues,
    #        file=paste(path,"/Issues/issues_",setting.name,".txt",sep=""))
    #}
  }
  return(issues)
}



Test_New_Data<-function(path) {
  check.data.result<-c()
  check.data.issues<-list()
  check.data.names<-c()
  
  cnt<-1
  data.files<-dir(path=paste(path,"/Data",sep=""))
  load(paste(path,"/Results/NeutraliseStatus.RData",sep=""))
  data.exists<-
    !data.files%in%neutralise.status$file.name[
      (neutralise.status$type=="data")&
        (neutralise.status$check==TRUE)]
  
  if(sum(data.exists)>0) {
    for(data in data.files[data.exists]) {
      issues<-c()
      filename<-paste(path,"/Data/",data,sep="")
      header<-Check_Data_Description(filename)
      if(!header) {
        issues<-c(issues,
                  "Header of data generator R file does not have a correct header")
      }
      data.name<-strsplit(data,".R")[[1]]
      if(data.name%in%neutralise.status$name[neutralise.status$type=="data"]) {
        issues<-c(issues,
                  "data generator name already exists")
      }
      #if(exists("settings")) {
      #  remove("settings")
      #}
      
      #setting.issues<-Test_New_Setting1(path,
      #                                  data.name)
      #issues<-c(issues,setting.issues)
      
      
        source(paste(path,"/Data/",data,sep=""))
        #pars<-settings[1,which(colnames(settings)!="null")]
        res<-try(Data.Generator(n1=20,n2=20), 
                 silent=TRUE)
        if(inherits(res, "try-error")) {
          issues<-c(issues,"error on running Data.Generator function")
        }
        else {
          # further checks
          if(length(res)!=2) {
            issues<-c(issues,
                      "Data generator should output data from with two columns")
          }
          if(length(res)==2) {
            if(min(names(res)==c("y","group"))==0) {
              issues<-c(issues,
                        "names of generated data frame are incorrect")
            }
            if(nrow(res)!=40) {
              issues<-c(issues,
                        "number of rows of data frame is not equal to n1+n2")
            }
          }
        }
      
      
      if(length(issues)==0) {
        neutralise.status<-neutralise.status%>%
          add_row(file.name=data,
                  name=data.name,
                  type="data",
                  check=TRUE,
                  to.run=FALSE,
                  neutralised=FALSE)
      }
      if(length(issues)>0) {
        neutralise.status<-neutralise.status%>%
          add_row(file.name=data,
                  name=data.name,
                  type="data",
                  check=FALSE,
                  to.run=FALSE,
                  neutralised=FALSE)
        write(issues,
              file=paste(path,"/Issues/issues_",data.name,".txt",sep=""))
      }
      save(neutralise.status,
           file=paste(path,"/Results/NeutraliseStatus.RData",sep=""))
      check.data.issues[[cnt]]<-issues
      check.data.names<-c(check.data.names,data.name)
      check.data.result<-c(check.data.result,
                           ifelse(length(issues)==0,TRUE,FALSE))
      cnt<-cnt+1
    }
  }
  if(sum(data.exists)==0) {
    check.data.result<-0
  }
  
  return(list(check.data.issues=check.data.issues,
              check.data.names=check.data.names,
              check.data.result=check.data.result))
}



Test_New_Method<-function(path) {
  db.test<-data.frame(y=rnorm(20),
                      group=rep(c(1,2),c(10,10)))
  
  check.method.result<-c()
  check.method.issues<-list()
  check.method.names<-c()
  cnt<-1
  method.files<-dir(path=paste(path,"/Methods",sep=""))
  load(paste(path,"/Results/NeutraliseStatus.RData",sep=""))
  method.exists<-
    !method.files%in%neutralise.status$file.name[
      (neutralise.status$type=="method")&
        (neutralise.status$check==TRUE)]
  
  
  if(sum(method.exists)>0) {
    for(method in method.files[method.exists]) {
      issues<-c()
      filename<-paste(path,"/Methods/",method,sep="")
      header<-Check_Method_Description(filename)
      if(!header) {
        issues<-c(issues,
                  "Header of method R file does not have a correct header")
      }
      method.name<-strsplit(method,".R")[[1]]
      if(method.name%in%neutralise.status$name) {
        issues<-c(issues,
                  "methods name already exists")
      }
      source(filename)
      res<-try(Test(db.test), silent=TRUE)
      if(inherits(res, "try-error")) {
        issues<-c(issues,"error on running Test function")
      }
      else {
        if(max((names(res)!=c("stat","p.value")))==1) {
          issues<-c(issues,"names of returned object are wrong")
        }
      }
      if(length(issues)==0) {
        neutralise.status<-neutralise.status%>%
          add_row(file.name=method,
                  name=method.name,
                  type="method",
                  check=TRUE,
                  to.run=TRUE,
                  neutralised=FALSE)
      }
      if(length(issues)>0) {
        neutralise.status<-neutralise.status%>%
          add_row(file.name=method,
                  name=method.name,
                  type="method",
                  check=FALSE,
                  to.run=FALSE,
                  neutralised=FALSE)
        write(issues,
              file=paste(path,"/Issues/issues_",method.name,".txt",sep=""))
      }
      save(neutralise.status,
           file=paste(path,"/Results/NeutraliseStatus.RData",sep=""))
      
      check.method.issues[[cnt]]<-issues
      check.method.names<-c(check.method.names,method.name)
      check.method.result<-c(check.method.result,
                             ifelse(length(issues)==0,TRUE,FALSE))
      cnt<-cnt+1
    }
  }
  if(sum(method.exists)==0) {
    check.method.result<-0
  }
  return(list(check.method.issues=check.method.issues,
              check.method.names=check.method.names,
              check.method.result=check.method.result))
}




Run_All_New_Methods<-function(path) {
  load(paste(path,"/Results/NeutraliseStatus.RData",sep=""))
  new.methods<-neutralise.status$file.name[
    (neutralise.status$type=="method")&
      (neutralise.status$to.run)]
  all.data<-neutralise.status$file.name[
    (neutralise.status$type=="data")&
      (neutralise.status$neutralised)]
  
  for(method in new.methods) {
    method.name<-strsplit(method,".R")[[1]][1]
    filename.method<-paste(path,"/Methods/",method,sep="")
    source(filename.method)
    
    for(data in all.data) {
      filename.data<-paste(path,"/Data/",data,sep="")
      data.name<-strsplit(data,".R")[[1]][1]
      source(filename.data)
      load(paste(path,"/Settings/",
                 data.name,"_settings.RData",sep=""))
      Run_Single_Method(path,method.name,data.name,
                        settings,filename.method,filename.data,
                        mode="all")
    }
    
    neutralise.status$neutralised[
      neutralise.status$file.name==method]<-TRUE
    neutralise.status$to.run[
      neutralise.status$file.name==method]<-FALSE
    
    save(neutralise.status,
         file=paste(path,"/Results/NeutraliseStatus.RData",sep=""))
    
  }
}


Run_All_New_Data<-function(path) {
  load(paste(path,"/Results/NeutraliseStatus.RData",sep=""))
  new.data<-neutralise.status$file.name[
    (neutralise.status$type=="data")&
      (neutralise.status$to.run)]
  all.methods<-neutralise.status$file.name[
    (neutralise.status$type=="method")&
      (neutralise.status$neutralised)]
  
  for(method in all.methods) {
    method.name<-strsplit(method,".R")[[1]][1]
    filename.method<-paste(path,"/Methods/",method,sep="")
    source(filename.method)
    
    for(data in new.data) {
      filename.data<-paste(path,"/Data/",data,sep="")
      data.name<-strsplit(data,".R")[[1]][1]
      settings.filename<-paste(path,"/Settings/",data.name,"_settings_new.RData",sep="")
      load(settings.filename)
      source(filename.data)
      
      Run_Single_Method(path,method.name,data.name,
                        settings,filename.method,filename.data,
                        mode="all")
    }
  }
  for(data in new.data) {
    data.name<-strsplit(data,".R")[[1]][1]
    neutralise.status$neutralised[neutralise.status$file.name==data]<-TRUE
    neutralise.status$to.run[neutralise.status$file.name==data]<-FALSE
    neutralise.status$neutralised[
      (neutralise.status$name==data.name)&
        (neutralise.status$type=="setting")]<-TRUE
    
    settings.filename<-paste(path,"/Settings/",data.name,"_settings_new.RData",sep="")
    first.setting<-!file.exists(
      paste(path,"/Settings/",data.name,"_settings.RData",sep=""))
    if(first.setting) {
      file.rename(from=settings.filename,
                  to=paste(path,"/Settings/",data.name,"_settings.RData",sep=""))
    }
    else {
      settings0<-settings
      load(paste(path,"/Settings/",data.name,"_settings.RData",sep=""))
      settings<-rbind(settings,settings0)
      save(settings,
           file=paste(path,"/Settings/",data.name,"_settings.RData",sep=""))
      file.remove(settings.filename)
    }
  }
  save(neutralise.status,
       file=paste(path,"/Results/NeutraliseStatus.RData",sep=""))
}



Run_Single_Method<-function(path,method.name,data.name,
                            settings,method.filename,data.filename,
                            N=1000,
                            mode="all") {
  n.settings<-nrow(settings)
  n.parameters<-ncol(settings)
  results<-matrix(ncol=9+n.parameters,
                  nrow=n.settings*nrow(sample.sizes))
  results<-as.data.frame(results)
  names(results)<-c(c("method","distribution","seed","N","n1","n2"),
                    names(settings),"power.01", "power.05","power.10")
  
  cnt<-1
  for(nn in 1:nrow(sample.sizes)) {
    for(pp in 1:n.settings) {
      seed<-round((as.numeric(Sys.time()))/100000*exp(runif(1,min=0.00001,max=10)),0)
      set.seed(seed)
      pwr<-Power(n1=sample.sizes[nn,1],
                 n2=sample.sizes[nn,2],
                 parameters = as.numeric(settings[pp,-n.parameters]),
                 N=N)
      
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
  new.dir<-paste(path,"/Results/",subdir,"SimRes_",method.name,"_",data.name,sep="")
  
  if(file.exists(new.dir)) {
    l<-length(dir(paste(path,"/Results/",subdir,sep="")))
    new.dir<-paste(new.dir,"_",l+1,sep="")
  }
  dir.create(new.dir)
  
  filename<-paste(new.dir,"/",method.name,"_",data.name,"_",seed,sep="")
  
  save(results,
       file=paste(filename,".RData",sep=""))
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


Power<-function(n1,n2,parameters,N=100) {
  p.values<-numeric(N)
  for(i in 1:N) {
    db<-Data.Generator(n1=n1,
                       n2=n2,
                       parameters = parameters)
    res<-Test(db)
    p.values[i]<-res$p.value
  }
  return(c(mean(p.values<0.01),
           mean(p.values<0.05),
           mean(p.values<0.10)))
}






Check_Method_Description<-function(filename) {
  check<-TRUE
  con=file(filename,"r")
  tmp<-readLines(con,n=-1)
  close(con)
  if(max(tmp=="# NAME")!=1) {
    check<-FALSE
  }
  if(max(tmp=="# DESCRIPTION")!=1) {
    check<-FALSE
  }
  if(max(tmp=="# REFERENCES")!=1) {
    check<-FALSE
  }
  return(check)
}

Check_Data_Description<-function(filename) {
  check<-TRUE
  con=file(filename,"r")
  tmp<-readLines(con,n=-1)
  close(con)
  if(max(tmp=="# DESCRIPTION")!=1) {
    check<-FALSE
  }
  return(check)
}


#Merge_Settings<-function(path) {
#}


############
