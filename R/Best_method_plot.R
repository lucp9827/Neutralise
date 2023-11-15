#' @export
Best_method_plot = function(path,name_extra,n=20,alpha=0.05,name_methods=NULL,N=10000,include_legend=TRUE){


  load(paste0(path,'\\Results_typeI_perdatagen.RData'))
  results = results_datagen_type1

  load(paste0(path,'\\Results_power_perdatagen.RData'))
  results_list = results_datagen

  results_df = filter_type1(path,results,results_power=results_list,alpha,N=N)$filter_df

  results_1_method = results_df[results_df$method==name_extra &results_df$n==n,]


  df = best_method(results_df,name_methods,name_extra=name_extra ,alpha,n)$all_one

  df$scenario = paste(df$distribution,df$id)
  results_1_method$scenario = paste(results_1_method$distribution,results_1_method$id)

  tt = merge(results_1_method,df,by='scenario')
  tt1=remove_missing(tt)

  txt=paste(name_extra,'has the largest power in',sum(tt1$power.x>=tt1$power.y,na.rm=TRUE),'of the ',length(tt1$power.x),'scenarios, which is ',round(sum(tt1$power.x>=tt1$power.y,na.rm=TRUE)/length(tt1$power.x),digits=4)*100,"% of the scenarios.")

  hh=tt1[(tt1$power.x<tt1$power.y),]
  hh$difference = hh$power.y-hh$power.x
  median(hh$difference)

  txt2 = paste("The median of the power differences for scenarios where ",name_extra," has smaller power than the best test is ",round(median(hh$difference,na.rm=TRUE),digits = 4))



  colors <- c("Cauchy" = "red", "Normal" = "purple", "Exp" = "yellow","Logistic"="blue","ghEqual"="green","ghEqualK"="darkgreen","GLDLS"="maroon","Normal2Var"="magenta")



  p <- ggplot(tt1,aes(x=power.y,y=power.x))+
    geom_point(aes(colour=factor(distribution.x)),size=4)+
    scale_color_manual(values = colors)+
    ylim(0,1)+xlim(0,1)+
    geom_abline()+
    # xlab(paste("Power of Best method"))+
    xlab(paste("power of ",'the best method',sep=""))+
    ylab(paste("Power of ",name_extra ,sep=""))+
    labs(colour='Data generation method')+theme(axis.text.x = element_text(size = 15),
                                                 axis.text.y = element_text(size = 15),
                                                 axis.title = element_text(size = 18),
                                                 strip.text=element_text(size=15),
                                                 legend.key.size = unit(1, 'cm'),
                                                 legend.title = element_text(size=15),legend.text = element_text(size=15))



  if (!include_legend) {
    p = p+ theme(legend.position = "none")

  }


  return(list(graph=p,data=tt1,text=txt,text2=txt2))
}

