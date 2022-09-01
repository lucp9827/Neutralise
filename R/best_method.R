best_method = function(results_df,name_methods=NULL,name_extra=NULL,alpha=0.01,n=20){
  results=results_df
  # results=subset(results,select=-mom1_1)
  # results=subset(results,select=-mom2_1)
  # results=subset(results,select=-mom3_1)
  # results=subset(results,select=-mom4_1)
  
  
  if (is.null(name_methods)){
    name_methods = unique(results$method)
  }
  
  if (!is.null(name_extra)){
    if (name_extra %in% name_methods){
      name_methods = name_methods[-which(name_methods==name_extra)]
    }else{
      name_methods=name_methods
    }
  }
  
  results = results[results$method%in%c(name_methods),]
  data = unique(results$distribution)
  
  
  # Per scenario
  tmp_end=data.frame()
  tmp_all = data.frame()
  for (d in (data)){
    
    results_data = results[results$distribution==d,]
    ind=length(unique(results_data$id))
    
    for (i in (1:ind)){
      #tmp = results_data[results_data$id==i,] 
      tmp = results_data[results_data$id==i&results_data$n==n,]
      tmp_scenario_n = tmp[which(tmp$power==max(tmp$power,na.rm=TRUE)),]
      tmp_scenario_nn = tmp_scenario_n 
      
      if (nrow(tmp_scenario_nn)>1){
        tmp_scenario_n1 = tmp_scenario_nn[1,]
        tmp_scenario_n1$method[1] = paste(tmp_scenario_nn$method,collapse='-')
        tmp_scenario_n1$seed[1] = paste(tmp_scenario_nn$seed,collapse='-')
        tmp_scenario_n1$l_CI[1] = paste(tmp_scenario_nn$l_CI,collapse='-')
        tmp_scenario_n1$u_CI[1] = paste(tmp_scenario_nn$u_CI,collapse='-')
        tmp_scenario_nn = tmp_scenario_n1
      }
      
      
      tmp_end = rbind(tmp_end,tmp_scenario_n)
      tmp_all = rbind(tmp_all,tmp_scenario_nn)
    }
    
    
  }
  tmp_end_tmp = tmp_end
  # Per data generation op basis van meest voorkomend hoogste power
  tmp_end=add_count(tmp_end, method,distribution,name="count")
  test1=data.frame()
  for (d in data){
    tmp_end_dis= tmp_end[tmp_end$distribution==d,]
    if (nrow(tmp_end_dis)==0){
      tmp_end_dis[1,]=NA
      tmp_end_dis[,'distribution']=d
      tmp_end_dis$nscenarios = 0
      test=tmp_end_dis
    }else{
      
      test=tmp_end_dis[which(tmp_end_dis$count==max(tmp_end_dis$count)),]
      n_scenarios=length(unique(tmp_end_dis$id))
      test$nscenarios=n_scenarios
      
    }
    
    
    test1=rbind(test1,test)
  }
  
  end = unique(test1[,c('method','distribution','count',"nscenarios")])
  
  
  return(list(end=end,all= tmp_end_tmp, all_one=tmp_all))
}
