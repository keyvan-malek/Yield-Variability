
library(ggplot2)
library(reshape2)
library(ggpubr)

#####################################
#
#               Figure S5
#
#####################################

input_data_folder<-"PATH_TO_DIRECTORY" # Path to the input folder
output_data_folder<-"PATH_TO_DIRECTORY" # path to the output folder

color_yv=c("steelblue4", "green3", "goldenrod")

Crop_name=c("Potatoes", "SW", "WW", "Corn", "Alfalfa","Pasture", "Grape", "Cherries", "Apple", "Pear")
crop_types= c("1827", "210", "218", "204", "701","1101", "2001", "1403", "1401", "1409") 

list_of_gcm=c("CanESM2_rcp45","CanESM2_rcp85","GFDL-ESM2G_45", "GFDL-ESM2G_85", "HadGEM2-CC365_45","HadGEM2-CC365_85", "HadGEM2-ES365_45", "HadGEM2-ES365_85", "inmcm4_45", "inmcm4_85")


proration_future= read.table(paste(input_data_folder, "timeseries_proration.txt", sep = ""), header = T)
proration_hist=read.table(paste(input_data_folder, "timeseries_proration_hist.txt", sep = ""), header = T)

max_melt=data.frame(matrix(ncol = 12, nrow = 660))
min_melt=data.frame(matrix(ncol = 12, nrow = 660))

names(max_melt)=c("Period", "RCP",Crop_name)
names(min_melt)=c("Period", "RCP",Crop_name)

# proration vs yield loss for different decades, color-coded scatter plot
for (i_crop in 1:length(crop_types)){
  
  gap_yield_ts_def=data.frame(matrix(nrow = 60, ncol = 13))
  max_yield=data.frame(matrix(nrow = 60, ncol = 13))
  min_yield=data.frame(matrix(nrow = 60, ncol = 13))
  
  names(gap_yield_ts_def)=c("year",rep(c("RCP_4.5", "RCP_8.5"), 5), "period", "Historical")
  names(max_yield)=c("year",rep(c("RCP_4.5", "RCP_8.5"), 5), "period", "Historical")
  names(min_yield)=c("year",rep(c("RCP_4.5", "RCP_8.5"), 5), "period", "Historical")
  
  gap_yield_ts_def[1:60,1]=seq(2030, 2089)
  max_yield[1:60,1]=seq(2030, 2089)
  min_yield[1:60,1]=seq(2030, 2089)
  
  gap_yield_ts_def[,12]=c(rep("2030-2060",30),rep("2060-2090",30))
  max_yield[,12]=c(rep("2030-2060",30),rep("2060-2090",30))
  min_yield[,12]=c(rep("2030-2060",30),rep("2060-2090",30))
  
  
  
  for (i_year in 1:60){
    
    decade=10*floor((i_year+2029)/10)
    yield_file=read.table(paste(input_data_folder, "CRCs/", crop_types[i_crop], "_", decade, ".txt", sep=""))
    
    for (i_gcm in 1:10){
      
      gap_yield_ts_def[i_year,1+i_gcm]=yield_file[21,i_gcm]-yield_file[1,i_gcm]
      max_yield[i_year,1+i_gcm]=yield_file[21,i_gcm]
      min_yield[i_year,1+i_gcm]=yield_file[1,i_gcm]
    }  # i_gcm
    
  } # i_year
  
  
  ########### historical
  for (i_year in 1:30){
    
    decade=10*floor((i_year+1979)/10)
    yield_file=read.table(paste(input_data_folder, "CRCs/", crop_types[i_crop], "_", decade, ".txt", sep=""))
    
    gap_yield_ts_def[i_year,13]=yield_file[21,1]-yield_file[1,1]
    max_yield[i_year,13]=yield_file[21,1]
    min_yield[i_year,13]=yield_file[1,1]
    
  } # i_year
  
  
  gap_melted_crop_gap=melt(gap_yield_ts_def[2:13], id.vars = "period") 
  max_melted_crop_gap=melt(max_yield[2:13], id.vars = "period") 
  min_melted_crop_gap=melt(min_yield[2:13], id.vars = "period") 
  
  RCP_TS=c(rep(c(rep("RCP_4.5",60),rep("RCP_8.5",60)), 5), rep("Historical", 60))
  
  max_melted_crop_gap[,2]=RCP_TS
  min_melted_crop_gap[,2]=RCP_TS
 
  max_melted_crop_gap[601:660,1]=rep("1980-2010", 60)
  min_melted_crop_gap[601:660,1]=rep("1980-2010", 60)
  
  if(i_crop==1){
    max_melt=max_melted_crop_gap
    min_melt=min_melted_crop_gap
  } else{
    max_melt[,2+i_crop]=max_melted_crop_gap[,3]
    min_melt[,2+i_crop]=min_melted_crop_gap[,3]
  }
  
  
} #i_crop

names(max_melt)=c("Period", "RCP",Crop_name)
names(min_melt)=c("Period", "RCP",Crop_name)


max_melt[631:660,]=max_melt[601:630,]
max_melt[601:630,1]="2030-2060"
max_melt[631:660,1]="2060-2090"

min_melt[631:660,]=min_melt[601:630,]
min_melt[601:630,1]="2030-2060"
min_melt[631:660,1]="2060-2090"

max_remelt=melt(max_melt[1:660,],id.vars = 'Period', 
                measure.vars = c('Potatoes', 'SW', 'WW', 'Corn',  'Alfalfa',  'Pasture',    'Grape',  'Apple', 'Cherries', 'Pear'))

min_remelt=melt(min_melt[1:660,],id.vars = 'Period', 
                measure.vars = c('Potatoes', 'SW', 'WW', 'Corn',  'Alfalfa',  'Pasture',    'Grape',  'Apple', 'Cherries', 'Pear'))

remelt_RCP<-rep(max_melt[1:660,2], 10)
crop_groups<-c(rep("Annual-Crops", 660*4), rep("Multiple-cutting-Crops", 660*2), rep("Tree-Fruits", 660*4) ) #, rep(2, 660), rep(1, 660*2))

max_remelt<-cbind(max_remelt, remelt_RCP, crop_groups)
min_remelt<-cbind(min_remelt, remelt_RCP, crop_groups)

# ------------------ Figure S5-A
                 
P_S5A=ggplot(max_remelt, aes(x=variable, y=value, fill=remelt_RCP))+
  geom_boxplot(aes(), position = position_dodge2(preserve = "total"))+ scale_fill_manual(values=color_yv) +
  labs(title="a. Maximum yield under fully-irrigated condition")  + 
   ylab("Yield (Tonnes/Hectare)")+ 
 facet_wrap(~Period+crop_groups ,dir = "h", scales = "free")+ theme(legend.position = "none")+
  theme(axis.title.x = element_blank(),  legend.title =element_blank()) 

ggsave(paste(output_data_folder, "/Final_max_plot.png", sep = ""), P_S5A)  

# ------------------ Figure S5-B

P_S5B=ggplot(min_remelt, aes(x=variable, y=value, fill=remelt_RCP))+
  geom_boxplot(aes())+ scale_fill_manual(values=color_yv) +
  labs(title="b. Minimum yield under no-irrigation condition")  + 
  ylab("Yield (Tonnes/Hectare)")+ 
  # facet_grid(crop_groups +Period ~.,  scales = "free", space = "free" )+ 
  facet_wrap(~Period+crop_groups ,dir = "h", scales = "free")+ theme(legend.position = "none")+
  theme(axis.title.x = element_blank(),  legend.title =element_blank())

ggsave(paste(output_data_folder, "Final_min_plot.png", sep = ""), P_S5B)  

ToSave=ggarrange(P_S5A,P_S5B, nrow = 2, common.legend = T ,legend = "bottom")
ggsave(paste(output_data_folder, "Final_combined_plot.png", sep = ""), ToSave, height = 10,width = 8)  
 