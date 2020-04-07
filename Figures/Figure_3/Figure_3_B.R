
library(ggplot2)
library(reshape2)
library(ggpubr)

#####################################
#
#               Figure 3-B
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

new_max=c(15, 5, 9, 12, 45, 45, 6, 6, 40, 13)
new_min=c(4,2,3,3, 5, 5, 2, 0, 2, 2)

plot_list_2 = list()

for (i_crop in 1:length(crop_types)){
  
  
  actual_yield_ts_df=data.frame(matrix(nrow = 60, ncol = 12))
  
  names(actual_yield_ts_df)=c("year",list_of_gcm, "historical")
  
  actual_yield_ts_df[1:60,1]=seq(2030, 2089)
  
  
  for (i_year in 1:60){
    
    decade=10*floor((i_year+2029)/10)
    
    yield_file=read.table(paste(input_data_folder, "CRCs/improved_", crop_types[i_crop], "_", decade, ".txt", sep=""))
    
    for (i_gcm in 1:10){
      
      proration=proration_future[i_year+12,i_gcm+1]
      p_round=5*floor(proration*100/5) 
      
      if(!proration==0 & !proration==1){
        actual_yield_ts_df[i_year,1+i_gcm]=yield_file[1+p_round/5,1] 
      } else{
        actual_yield_ts_df[i_year,1+i_gcm]=yield_file[1+p_round/5,1]
      }
      

      
    }  # i_gcm
    
  } # i_year
  
  
  
  ########### historical
  for (i_year in 1:30){
    
    decade=10*floor((i_year+1979)/10)
    yield_file=read.table(paste(input_data_folder, "CRCs/improved_", crop_types[i_crop], "_", decade, ".txt", sep=""))
    

    proration=proration_hist[i_year,2]
    p_round=5*floor(proration*100/5) 
    
    actual_yield_ts_df[i_year,12]=yield_file[1+p_round/5,1]
    
  } # i_year
  
  combined_rcp4.5_imp<-data.frame(matrix(ncol = 3,nrow = 630))
  names(combined_rcp4.5_imp)=c("RCP", "Time", "Yield")
  combined_rcp4.5_imp[1:30,3]=actual_yield_ts_df[1:30,12]
  
  for (i in 1:10){
    combined_rcp4.5_imp[((i-1)*60+31):((i-1)*60+90),3]=actual_yield_ts_df[1:60,(i+1)] 
    #print(paste((i*30+1):(i*30+30)))
  }
  
  rcps=c(rep("Historical", 30), rep(c(rep("RCP_4.5", 60), rep("RCP_8.5", 60)), 5))
  decade=c(rep("1980-2010", 30), rep(c(rep("2030-2060", 30), rep("2060-2090", 30)), 10))
  combined_rcp4.5_imp[,1]=rcps
  combined_rcp4.5_imp[,2]=decade
  
  
  P3_B=ggplot(combined_rcp4.5_imp, aes(x=RCP, y=Yield, fill=RCP)) + 
    geom_violin() + scale_fill_manual(values=c(color_yv)) +
    labs(y =  "Yield (tonnes/ha)")+
    geom_boxplot(width=.07, fill="white") + 
    facet_wrap(~Time, scale="free_x") +  theme(legend.title = element_blank()) +
    scale_y_continuous(breaks = seq(new_min[i_crop], new_max[i_crop], by = (new_max[i_crop]-new_min[i_crop])/5), limits = c(new_min[i_crop],new_max[i_crop])) + 
    theme(legend.position = "none") + 
    ggtitle(paste("b. Crop yield under improved crops"))+ theme(axis.title.x = element_blank()) 
  
  ggsave(paste(output_data_folder, "Figure_3_B/",  crop_types[i_crop], ".jpg", sep = ""), plot = P3_B)
  
  plot_list_2[[i_crop]] = P3_B
  
} #i_crop




