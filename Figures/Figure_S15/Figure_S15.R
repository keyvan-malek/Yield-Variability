
library(ggplot2)
library(reshape2)
library(ggpubr)

#####################################
#
#               Figure S15
#
#####################################

input_data_folder<-"PATH_TO_DIRECTORY" # Path to the input folder
output_data_folder<-"PATH_TO_DIRECTORY" # path to the output folder

color_yv=c("steelblue4", "green3", "goldenrod")

Crop_name=c("Potatoes", "SW", "WW", "Corn", "Alfalfa","Pasture", "Grape", "Cherries", "Apple", "Pear")
crop_types= c("1827", "210", "218", "204", "701","1101", "2001", "1403", "1401", "1409") #1
Crop_name_title=c("a. Potatoes", "b. S-Wheat", "c. W-Wheat", "d. Corn", "e. Alfalfa","f. Pasture", "g. Grape", "h. Cherries", "i. Apple", "j. Pear")


list_of_gcm=c("CanESM2_rcp45","CanESM2_rcp85","GFDL-ESM2G_45", "GFDL-ESM2G_85", "HadGEM2-CC365_45","HadGEM2-CC365_85", "HadGEM2-ES365_45", "HadGEM2-ES365_85", "inmcm4_45", "inmcm4_85")


proration_future= read.table(paste(input_data_folder, "timeseries_proration.txt", sep = ""), header = T)
proration_hist=read.table(paste(input_data_folder, "timeseries_proration_hist.txt", sep = ""), header = T)

plot_list_S15 = list()

output<-data.frame(matrix(ncol = 9,nrow = 21))

names(output)=c("Period","GCM","RCP","Avg_1", "Avg_2", "Stdev_1","Stdev_2", "FutureCrops", "HistoricalCrops")

output[,1]=c("1980-2010", rep("2030-2060",10), rep("2060-2090",10))
output[,2]=c("Historical", rep(c(rep("CanESM2",2), rep("GFDL-ESM2G",2), rep("HadGEM2-CC365",2), rep("HadGEM2-ES365",2), rep("inmcm4",2)),2))
output[,3]=c("Historical", rep(c("RCP_4.5", "RCP_8.5"),10))


for (i_crop in 1:length(crop_types)){
  
  df_scale<-data.frame()

for (i_coef in 1:10){
  
  scaling_factor<-i_coef * 0.05 +1
  
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
    
    yield_file_improved=yield_file_improved/1.1*scaling_factor 
    
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
    baseline_yield_ts_df[i_year,1+i_gcm]=yield_file_baseline[1+p_round/5,1]
    
    
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
  
  
  out_3_1=melt(data = output[1:11,], id.vars = "RCP", measure.vars = c("Avg_1", "Avg_2"))
  out_3_2=melt(data = output[1:11,], id.vars = "RCP", measure.vars = c("Stdev_1", "Stdev_2"))
  out_3_3=cbind(out_3_1, out_3_2[,3])
  names(out_3_3)=c("RCP", "variable", "average", "stdv")
  out_3_3$variable=as.character(out_3_3$variable)
  out_3_3[1:11,2]=as.character(rep("No_Improvement", 11))
  out_3_3[12:22,2]=rep("Improved_Yield", 11)
  scf_column<-c(rep("SF_1.00",11 ), rep(paste("SF_", scaling_factor, sep = ""),11 ) )
  out_3_3<-cbind(out_3_3, scf_column)
  
  out_3_3=out_3_3[-c(1, 12),]


  df_scale<-rbind(df_scale,out_3_3)
    
} #-------------------------------- i_coef
  

  P_S15=ggplot(df_scale, aes(x=average, y=stdv, fill=scf_column)) + geom_boxplot()  +  # geom_smooth(span = 0.8)
    scale_fill_viridis_d(option = "C") + theme_bw() +labs(y = "Standard Deviation (tons/hec)", x= "Average Yield (tons/hec)")+
    ggtitle(paste(Crop_name[i_crop], "- Average vs standard deviation")) + theme(legend.title = element_blank(), legend.position = "" )


  plot_list_S15[[i_crop]] = P_S15
} #--------------------------------  i_crop

combined_p10=ggarrange(plotlist = plot_list_S15[1:10], ncol = 2, nrow = 5,
                      common.legend =T, legend = "bottom") 

ggsave(plot = combined_p10, filename = paste(output_data_folder,"SA_combined.png", sep=""), width = 10, height =12 )
