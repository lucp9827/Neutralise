#' @export
summarize_results_type1 = function(path){
  library(moments)
load(file = paste0(path,"\\Results\\NeutraliseStatus.RData"))

names_methods=(All_Neutralised(path))

names_data_scenarios = All_Neutralised(path,type='data')
rm(names_data_scenarios)

data.gen=names_data_scenarios

## Summarize power results per datageneration method & scenario settings  (list per datagen)

results_datagen_type1=list()

for (d in data.gen){

  if (d=="Normal2Var"){
    d="Normal"
  }

methods= names_methods

result_method_datagen = data.frame()

source(paste0(path,"\\Data/",d,".R"))

for (m in methods){


  scenarios = All_Neutralised_Scenarios(path,d,type='type1')
  # Load results file
  dir1<-dir(paste0(path,"\\Results/SimRes_",m,"_",d,sep=""))


  file1<-paste0(path,"\\Results/SimRes_",m,"_",d,
               "/",dir1[grepl(".RData",dir1)&grepl(m,dir1)],
               sep="")

  load(file1)

  # clean up results file for shiny-app plots
alpha=0.05
colnr= grep(as.numeric(alpha),colnames(results))[1]
colnames(results)[colnr+1]='l_CI0.05'
colnames(results)[colnr+2]='u_CI0.05'

alpha=0.01
colnr= grep(as.numeric(alpha),colnames(results))[1]
colnames(results)[colnr+1]='l_CI0.01'
colnames(results)[colnr+2]='u_CI0.01'

alpha=0.10
colnr= grep(as.numeric(alpha),colnames(results))[1]
colnames(results)[colnr+1]='l_CI0.10'
colnames(results)[colnr+2]='u_CI0.10'

results$n = results$n1+results$n2

results = results[results$null==1,]
results = subset(results,select=-null)

# add id for unique scenarios (not including sample size)
for (i in (1:nrow(scenarios))){
tmp =scenarios[i,-1]

# calculate moments per data gen scenario
set.seed(9827)
data = Data.Generator(n1=10000,n2=10000,parameters=as.numeric(tmp))
mom1_1 = mean(data[data$group==1,'y'])
mom1_2 = mean(data[data$group==2,'y'])

mom2_1 = var(data[data$group==1,'y'])
mom2_2 = var(data[data$group==2,'y'])

mom3_1 = skewness(data[data$group==1,'y'])
mom3_2 = skewness(data[data$group==2,'y'])

mom4_1 = kurtosis(data[data$group==1,'y'])
mom4_2 = kurtosis(data[data$group==2,'y'])

tmp=data.frame(tmp,distribution=d)

colnames(tmp)=c(colnames(scenarios)[-1],'distribution')
sub_data_tmp=semi_join(results,tmp)
sub_data_tmp$id=scenarios[i,"id"]

sub_data_tmp$mom1_1 = mom1_1
sub_data_tmp$mom1_2 = mom1_2

sub_data_tmp$mom2_1 = mom2_1
sub_data_tmp$mom2_2 = mom2_2

sub_data_tmp$mom3_1 = mom3_1
sub_data_tmp$mom3_2 = mom3_2

sub_data_tmp$mom4_1 = mom4_1
sub_data_tmp$mom4_2 = mom4_2

result_method_datagen = rbind(result_method_datagen ,sub_data_tmp)
}
}
results_datagen_type1[[d]]=result_method_datagen
}

save(results_datagen_type1,file=paste0(path,'\\Results_typeI_perdatagen.RData'))


## Summarize power results per datageneration method & without scenario settings in 1 dataframe

results_datagen_df_type1=data.frame()

for (d in data.gen){

  if (d=="Normal2Var"){
    d="Normal"
  }


  methods= names_methods

  source(paste0(path,"\\Data/",d,".R"))

  result_method_datagen_tmp =data.frame()

  for (m in methods){

    scenarios = All_Neutralised_Scenarios(path,d,type='type1')
    result_method_datagen = data.frame()

    # Load results file
    dir1<-dir(paste0(path,"\\Results/SimRes_",m,"_",d,sep=""))


    file1<-paste0(path,"\\Results/SimRes_",m,"_",d,
                 "/",dir1[grepl(".RData",dir1)&grepl(m,dir1)],
                 sep="")

    load(file1)



    # clean up results file for shiny-app plots
    alpha=0.05
    colnr= grep(as.numeric(alpha),colnames(results))[1]
    colnames(results)[colnr+1]='l_CI0.05'
    colnames(results)[colnr+2]='u_CI0.05'

    alpha=0.01
    colnr= grep(as.numeric(alpha),colnames(results))[1]
    colnames(results)[colnr+1]='l_CI0.01'
    colnames(results)[colnr+2]='u_CI0.01'

    alpha=0.10
    colnr= grep(as.numeric(alpha),colnames(results))[1]
    colnames(results)[colnr+1]='l_CI0.10'
    colnames(results)[colnr+2]='u_CI0.10'

    results = subset(results,select=-null)
    results$n = results$n1+results$n2

    # add id for unique scenarios (not including sample size)
    for (i in (1:nrow(scenarios))){
      tmp =scenarios[i,-1]

      set.seed(9827)
      data = Data.Generator(n1=10000,n2=10000,parameters=as.numeric(tmp))
      mom1_1 = mean(data[data$group==1,'y'])
      mom1_2 = mean(data[data$group==2,'y'])

      mom2_1 = var(data[data$group==1,'y'])
      mom2_2 = var(data[data$group==2,'y'])

      mom3_1 = skewness(data[data$group==1,'y'])
      mom3_2 = skewness(data[data$group==2,'y'])

      mom4_1 = kurtosis(data[data$group==1,'y'])
      mom4_2 = kurtosis(data[data$group==2,'y'])


      tmp=data.frame(tmp,distribution=d)
      colnames(tmp)=c(colnames(scenarios)[-1],'distribution')
      sub_data_tmp=semi_join(results,tmp)
      sub_data_tmp$id=scenarios[i,"id"]

      sub_data_tmp$mom1_1 = mom1_1
      sub_data_tmp$mom1_2 = mom1_2

      sub_data_tmp$mom2_1 = mom2_1
      sub_data_tmp$mom2_2 = mom2_2

      sub_data_tmp$mom3_1 = mom3_1
      sub_data_tmp$mom3_2 = mom3_2

      sub_data_tmp$mom4_1 = mom4_1
      sub_data_tmp$mom4_2 = mom4_2


      result_method_datagen = rbind(result_method_datagen ,sub_data_tmp)
    }

    del_col=names(scenarios)[-(1:2)]
    result_method_datagen=result_method_datagen[,!names(result_method_datagen)%in%del_col]
    result_method_datagen_tmp=rbind(result_method_datagen_tmp,result_method_datagen)


    }
  results_datagen_df_type1=rbind(results_datagen_df_type1,result_method_datagen_tmp)
}

save(results_datagen_df_type1,file=paste0(path,'\\Results_type1_perdatagen_df.RData'))

}

