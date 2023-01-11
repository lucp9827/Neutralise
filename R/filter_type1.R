#' @export
filter_type1 = function(path,results,results_power,alpha,N){
  # which scenarios to filter out?
  filter_data_list = list()
  filter_data_df = data.frame()

  data = names(results_power)
  lowlim = optimise(function(p){(p+sqrt(p*(1-p)/N)*qnorm(alpha/2, mean = 0, sd = 1, lower.tail = FALSE)-alpha)^2}, interval=c(0,1))$minimum
  uplim = optimise(function(p){(p-sqrt(p*(1-p)/N)*qnorm(alpha/2, mean = 0, sd = 1, lower.tail = FALSE)-alpha)^2}, interval=c(0,1))$minimum


  for (d in data){

    if (d!='Normal2Var'){
      results_id = results[[d]]
    }else{
      results_id = results[['Normal']]
    }

    results_tmp=filter_significance(results_id,alpha)

    results_tmp$control= results_tmp$power>=lowlim&results_tmp$power<=uplim
    results_tmp[is.na(results_tmp$control),'control']<-FALSE

    filter_data = results_tmp[results_tmp$control==FALSE,]


    filter_data = subset(filter_data, select=-N)
    filter_data = subset(filter_data, select=-seed)
    filter_data = subset(filter_data, select=-delta)
    filter_data = subset(filter_data, select=-control)
    filter_data = subset(filter_data, select=-id)
    filter_data = subset(filter_data, select=-power)
    filter_data = subset(filter_data, select=-l_CI)
    filter_data = subset(filter_data, select=-u_CI)
    filter_data = subset(filter_data, select=-mom1_1)
    filter_data = subset(filter_data, select=-mom1_2)
    filter_data = subset(filter_data, select=-mom2_1)
    filter_data = subset(filter_data, select=-mom2_2)
    filter_data = subset(filter_data, select=-mom3_1)
    filter_data = subset(filter_data, select=-mom3_2)
    filter_data = subset(filter_data, select=-mom4_1)
    filter_data = subset(filter_data, select=-mom4_2)
    filter_data = subset(filter_data, select=-cnt)

    methods_test = unique(filter_data$method )
    itt=which(colnames(filter_data)=='n2')+1
    settng_cols= filter_data[,c(1,itt:(length(filter_data)))]

    if(d=='Normal2Var'){
      settng_cols=data.frame(method=settng_cols$method,sd1=settng_cols$sd,sd2=settng_cols$sd,n=settng_cols$n)
    }

    Results_power = results_power[[d]]
    Results_power = filter_significance(Results_power,alpha)
    Results_power$control=TRUE

    for (m in methods_test){
      for (i in colnames(settng_cols)[-c(1,length(settng_cols))]){

        val =  unique(settng_cols[settng_cols$method==m,c(i,'n')])

        for (j in (1:nrow(val))){


          n=val[j,'n']

          keep =  Results_power[Results_power$method==m&Results_power$n==n,'control']

          for (l in (1:length(keep))){

            Results_power[Results_power$method==m&Results_power$n==n,'control'][l]= ifelse((( Results_power[Results_power$method==m&Results_power$n==n,c(i)][l]==val[j,1])& Results_power[Results_power$method==m&Results_power$n==n,'control'][l]==TRUE),FALSE,keep[l])

          }


        }
      }
    }




    end_power_data = Results_power[Results_power$control==TRUE,]

    filter_data_list[[d]] = end_power_data

    # convert in a data frame
    if (d=='Normal2Var'){
      scenarios = All_Neutralised_Scenarios(path,d,type="power")
    }else{
      scenarios = All_Neutralised_Scenarios(path,d,type="type1")
    }
    scenario_filter = scenarios[,-c(1:2)]
    scenario_filter = data.frame(scenario_filter)
    names(scenario_filter ) <- names(scenarios)[-c(1:2)]

    end_power_data_tmp = end_power_data[,!names(end_power_data)%in% names(scenario_filter )]
    filter_data_df = rbind(filter_data_df,end_power_data_tmp )

  }

  return(list(filter_list=filter_data_list, filter_df=filter_data_df))
}
