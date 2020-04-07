library(ggplot2)
library(reshape2)
library(ggpubr)

#####################################
#
#               Figure 3- C and D
#
#####################################

input_data_folder<-"PATH_TO_DIRECTORY" # Path to the input folder
output_data_folder<-"PATH_TO_DIRECTORY" # path to the output folder

color_yv=c("steelblue4", "green3", "goldenrod")

Crop_name=c("Potatoes", "SW", "WW", "Corn", "Alfalfa","Pasture", "Grape", "Cherries", "Apple", "Pear")
crop_types= c("1827", "210", "218", "204", "701","1101", "2001", "1403", "1401", "1409") #1

list_of_gcm=c("CanESM2_rcp45","CanESM2_rcp85","GFDL-ESM2G_45", "GFDL-ESM2G_85", "HadGEM2-CC365_45","HadGEM2-CC365_85", "HadGEM2-ES365_45", "HadGEM2-ES365_85", "inmcm4_45", "inmcm4_85")


proration_future= read.table(paste(input_data_folder, "timeseries_proration.txt", sep = ""), header = T)
proration_hist=read.table(paste(input_data_folder, "timeseries_proration_hist.txt", sep = ""), header = T)


plot_list_3 = list()
plot_list_4 = list()

library(expss)

output<-data.frame(matrix(ncol = 9,nrow = 21))

names(output)=c("Period","GCM","RCP","Avg_1", "Avg_2", "Stdev_1","Stdev_2", "FutureCrops", "HistoricalCrops")

output[,1]=c("1980-2010", rep("2030-2060",10), rep("2060-2090",10))
output[,2]=c("Historical", rep(c(rep("CanESM2",2), rep("GFDL-ESM2G",2), rep("HadGEM2-CC365",2), rep("HadGEM2-ES365",2), rep("inmcm4",2)),2))
output[,3]=c("Historical", rep(c("RCP_4.5", "RCP_8.5"),10))



for (i_crop in 1:length(crop_types)){
  
    
  improved_yield_ts_df=data.frame(matrix(nrow = 60, ncol = 12))
  names(improved_yield_ts_df)=c("year",list_of_gcm, "historical")
  improved_yield_ts_df[1:60,1]=seq(2030, 2089)
  
  baseline_yield_ts_df=data.frame(matrix(nrow = 60, ncol = 12))
  names(baseline_yield_ts_df)=c("year",list_of_gcm, "historical")
  baseline_yield_ts_df[1:60,1]=seq(2030, 2089) 
  
  for (i_year in 1:60){
    
    decade=10*floor((i_year+2029)/10)
    yield_file_improved=read.table(paste(input_data_folder, "CRCs/improved_", crop_types[i_crop], "_", decade, ".txt", sep=""))
    yield_file_baseline=read.table(paste(input_data_folder, "CRCs/", crop_types[i_crop], "_", decade, ".txt", sep=""))
    
    
    for (i_gcm in 1:10){
      
      proration=proration_future[i_year+12,i_gcm+1]
      p_round=5*floor(proration*100/5) 
      
      improved_yield_ts_df[i_year,1+i_gcm]=yield_file_improved[1+p_round/5,1]
      baseline_yield_ts_df[i_year,1+i_gcm]=yield_file_baseline[1+p_round/5,1]
      
      
    }  # i_gcm
    
  } # i_year
  
  
  
  ########### historical
  for (i_year in 1:30){
    
    decade=10*floor((i_year+1979)/10)
    yield_file_improved=read.table(paste(input_data_folder, "CRCs/improved_", crop_types[i_crop], "_", decade, ".txt", sep=""))
    yield_file_baseline=read.table(paste(input_data_folder, "CRCs/", crop_types[i_crop], "_", decade, ".txt", sep=""))
    
    
    proration=proration_hist[i_year,2]
    p_round=5*floor(proration*100/5) 
    
    improved_yield_ts_df[i_year,12]=yield_file_improved[1+p_round/5,1]
    baseline_yield_ts_df[i_year,12]=yield_file_baseline[1+p_round/5,1]
    
    
  } # i_year
  
 
  

  # average yield
  output[1,5]=mean(improved_yield_ts_df[1:30,12])
  output[2:11,5]=colMeans(improved_yield_ts_df[1:30,2:11])
  output[12:21,5]=colMeans(improved_yield_ts_df[31:60,2:11])
  
  # standard deviation
  output[1,7]=sd(improved_yield_ts_df[1:30,12])
  output[2:11,7]=apply(improved_yield_ts_df[1:30,2:11],2,sd)
  output[12:21,7]=apply(improved_yield_ts_df[31:60,2:11],2,sd)
  

    
      # average yield
      output[1,4]=mean(baseline_yield_ts_df[1:30,12])
      output[2:11,4]=colMeans(baseline_yield_ts_df[1:30,2:11])
      output[12:21,4]=colMeans(baseline_yield_ts_df[31:60,2:11])
      
      # standard deviation
      output[1,6]=sd(baseline_yield_ts_df[1:30,12])
      output[2:11,6]=apply(baseline_yield_ts_df[1:30,2:11],2,sd)
      output[12:21,6]=apply(baseline_yield_ts_df[31:60,2:11],2,sd)
     
      #--------- criteria for bad years (70% of maximum status quo yield)
      
      criteria_baseline=rep(max(baseline_yield_ts_df[1:30,12])*0.7, 21) # status quo yield
      criteria_improved=rep(max(baseline_yield_ts_df[1:30,12])*0.7, 21) # status quo yield
      
      #############----------------------- BAD YEARS -----------
      
      #Historical
      output[1,8]=length(baseline_yield_ts_df[which(baseline_yield_ts_df[1:30,12]<criteria_baseline[1]),12])/30 # no change in  crop type
      output[1,9]=length(improved_yield_ts_df[which(improved_yield_ts_df[1:30,12]<criteria_improved[1]),12])/30 # improved crop types
      
      # Future
      for (i_scr in 1:10){
      #Extreme years- Status Quo Crops
      
      output[i_scr+1,9]=length(improved_yield_ts_df[which(improved_yield_ts_df[1:30,i_scr+1]<criteria_improved[i_scr+1]),12])/30
      output[i_scr+11,9]=length(improved_yield_ts_df[which(improved_yield_ts_df[31:60,i_scr+1]<criteria_improved[i_scr+11]),12])/30
      
      #Extreme years- Improved Crops
      
      output[i_scr+1,8]=length(baseline_yield_ts_df[which(baseline_yield_ts_df[1:30,i_scr+1]<criteria_baseline[i_scr+1]),12])/30
      output[i_scr+11,8]=length(baseline_yield_ts_df[which(baseline_yield_ts_df[31:60,i_scr+1]<criteria_baseline[i_scr+11]),12])/30
      

      } # i_scr
  
  
#####---------------------------------------------------- FIGURES ------------------------------------------  

      
      melt_out=melt(data =output[,c(3, 8,9)],id.vars = "RCP" )
      
      #------------------------ Figure 3-C
      
      melt_out$variable <- factor(melt_out$variable,levels = c("HistoricalCrops", "FutureCrops", "Sun"))
      
      P_3_C=  ggplot(melt_out, aes(x=variable, y=value, fill=RCP)) + 
        geom_boxplot() + 
        scale_fill_manual(values=c(color_yv)) +
        labs(y = "Probability of Bad Years")+  theme(legend.title = element_blank())+ theme(axis.title.x = element_blank()) +
        ggtitle(paste("c. Probability of bad years")) +theme(legend.position = "none")+
        scale_x_discrete(labels = c("HistoricalCrops"= "Status Quo Crop Types", "FutureCrops"= "Improved Crop Types"))
      
      ggsave(paste(output_data_folder, "Figure_3_C/", crop_types[i_crop], ".png", sep = ""), plot = P_3_C)
      
      plot_list_3[[i_crop]]=P_3_C
      
      #------------------------ Figure 3-D
  
      out_3_1=melt(data = output[1:11,], id.vars = "RCP", measure.vars = c("Avg_1", "Avg_2"))
      out_3_2=melt(data = output[1:11,], id.vars = "RCP", measure.vars = c("Stdev_1", "Stdev_2"))
      out_3_3=cbind(out_3_1, out_3_2[,3])
      names(out_3_3)=c("RCP", "variable", "average", "stdv")
      out_3_3$variable=as.character(out_3_3$variable)
      out_3_3[1:11,2]=as.character(rep("No_Improvement", 11))
      out_3_3[12:22,2]=rep("Improved_Yield", 11)
      

      # remove historical
      out_3_3=out_3_3[-c(1, 12),]
      
      P3_D=ggplot(out_3_3) + 
        geom_point(size=3, aes(y=stdv, x=average, colour=variable, shape=RCP)) + 
        stat_ellipse(type = "norm", linetype = 2, level = 0.9,  show.legend = T, size=1.1,  aes(y=stdv, x=average, colour=variable)) +
        scale_colour_manual(values=c("black", "maroon2"))+
        labs(y = "Standard Deviation (tonnes/ha)", x= "Average Yield (tonnes/ha)")+
        ggtitle("Yield ")+ ggtitle(paste("d. Average vs standard deviation")) + theme(legend.position = "none")
      
      plot_list_4[[i_crop]] = P3_D
      
      ggsave(paste(output_data_folder, "Figure_3_D/", crop_types[i_crop], ".png", sep = ""), plot = P3_D)

  
  } #i_crop
