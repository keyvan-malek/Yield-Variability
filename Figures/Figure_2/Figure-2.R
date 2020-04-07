
library(ggplot2)
library(ggpubr)
library(reshape2)

#####################################
#
#               Figure 2
#
#####################################

input_data_folder<-"PATH_TO_DIRECTORY" # Path to the input folder
output_data_folder<-"PATH_TO_DIRECTORY" # path to the output folder

color_yv=c("steelblue4", "green3", "goldenrod")

Crop_name=c("Potatoes", "SW", "WW", "Corn", "Alfalfa","Pasture", "Grape", "Cherries", "Apple", "Pear")
crop_types= c("1827", "210", "218", "204", "701","1101", "2001", "1403", "1401", "1409") #1


hist_proration=read.table(paste(input_data_folder, "timeseries_proration_hist.txt", sep = ""), header = T)
future_proration=read.table(paste(input_data_folder, "timeseries_proration.txt", sep = ""), header = T)

df_sd_all<- data.frame(matrix(ncol = 13, nrow = 21))
names(df_sd_all)=c("N", "GCM", "Period" ,Crop_name)
df_sd_all[,1]=seq(1,21)
df_sd_all[,2]=c("Historical", "CanESM2_rcp45", "CanESM2_rcp85", "GFDL-ESM2G_45", "GFDL-ESM2G_85", "HadGEM2-CC365_45", "HadGEM2-CC365_85", 
                "HadGEM2-ES365_45", "HadGEM2-ES365_85", "inmcm4_45", "inmcm4_85", "CanESM2_rcp45", "CanESM2_rcp85", "GFDL-ESM2G_45", 
                "GFDL-ESM2G_85", "HadGEM2-CC365_45", "HadGEM2-CC365_85", "HadGEM2-ES365_45", "HadGEM2-ES365_85", "inmcm4_45", "inmcm4_85")
df_sd_all[,3]=c("1980-2010", rep("2030-2060",10), rep("2060-2090", 10))

df_mean_all<- data.frame(matrix(ncol = 13, nrow = 21))
names(df_mean_all)=c("N", "GCM", "Period", Crop_name)
df_mean_all[,1]=seq(1,21)
df_mean_all[,2]=c("Historical", "CanESM2_rcp45", "CanESM2_rcp85", "GFDL-ESM2G_45", "GFDL-ESM2G_85", "HadGEM2-CC365_45", "HadGEM2-CC365_85", 
                "HadGEM2-ES365_45", "HadGEM2-ES365_85", "inmcm4_45", "inmcm4_85", "CanESM2_rcp45", "CanESM2_rcp85", "GFDL-ESM2G_45", 
                "GFDL-ESM2G_85", "HadGEM2-CC365_45", "HadGEM2-CC365_85", "HadGEM2-ES365_45", "HadGEM2-ES365_85", "inmcm4_45", "inmcm4_85")
df_mean_all[,3]=c("1980-2010", rep("2030-2060",10), rep("2060-2090", 10))


for (i_crop in 1:length(crop_types)){
  
  
  actual_yield_ts_df=data.frame(matrix(nrow = 60, ncol = 12))
  
  names(actual_yield_ts_df)=c("year",list_of_gcm, "historical")
  
  actual_yield_ts_df[1:60,1]=seq(2030, 2089)
  
  
  for (i_year in 1:60){
    
    decade=10*floor((i_year+2029)/10)
    yield_file=read.table(paste(input_data_folder, "CRCs/", crop_types[i_crop], "_", decade, ".txt", sep=""))
    
    for (i_gcm in 1:10){
      
      proration=future_proration[i_year+12,i_gcm+1]
      p_round=5*floor(proration*100/5) 
      
      actual_yield_ts_df[i_year,1+i_gcm]=yield_file[1+p_round/5,i_gcm]
      
      
    }  # i_gcm
    
  } # i_year
  
  
  
  ########### historical
  for (i_year in 1:30){
    
    decade=10*floor((i_year+1979)/10)
    yield_file=read.table(paste(input_data_folder, "CRCs/", crop_types[i_crop], "_", decade, ".txt", sep=""))
    
    
    proration=hist_proration[i_year,2]
    p_round=5*floor(proration*100/5) 
    
    actual_yield_ts_df[i_year,12]=yield_file[1+p_round/5,1]
    
  } # i_year
  
  combined_rcp4.5<-data.frame(matrix(ncol = 3,nrow = 630))
  names(combined_rcp4.5)=c("RCP", "Time", "Yield")
  combined_rcp4.5[1:30,3]=actual_yield_ts_df[1:30,12]
  
  # Impacts of climate change on average yield and standard deviation of yield which represents inter-annual variability
  
  list_of_gcm=c("CanESM2_rcp45","CanESM2_rcp85","GFDL-ESM2G_45", "GFDL-ESM2G_85", "HadGEM2-CC365_45","HadGEM2-CC365_85", "HadGEM2-ES365_45", "HadGEM2-ES365_85", "inmcm4_45", "inmcm4_85")
  
  mean_2030=colMeans(actual_yield_ts_df[1:30,2:12])
  mean_2060=colMeans(actual_yield_ts_df[31:60,2:11])
  
  min_2030=apply(actual_yield_ts_df[1:30,2:12],2,min)
  min_2060=apply(actual_yield_ts_df[31:60,2:11],2,min)
  
  max_2030=apply(actual_yield_ts_df[1:30,2:12],2,max)
  max_2060=apply(actual_yield_ts_df[31:60,2:11],2,max)
  
  sd_2030=apply(actual_yield_ts_df[1:30,2:12],2,sd)
  sd_2060=apply(actual_yield_ts_df[31:60,2:11],2,sd)
  
  df_sd<-data.frame(matrix(ncol = 3,nrow = 21))
  names(df_sd)=c("GCM", "year", "Stdv")
  
  df_sd[1,3]=sd_2030[11]
  df_sd[2:11,3]=sd_2030[1:10]
  df_sd[12:21,3]=sd_2060[1:10]
  df_sd[,1]=c("Historical", rep(list_of_gcm,2))
  df_sd[,2]=c("1980-2010", rep("2030-2060",10), rep("2060-2090", 10))
  

  df_mean<-data.frame(matrix(ncol = 3,nrow = 21))
  names(df_mean)=c("GCM", "year", "Stdv")
  
  df_mean[1,3]=mean_2030[11]
  df_mean[2:11,3]=mean_2030[1:10]
  df_mean[12:21,3]=mean_2060[1:10]
  df_mean[,1]=c("Historical", rep(list_of_gcm,2))
  df_mean[,2]=c("1980-2010", rep("2030-2060",10), rep("2060-2090", 10))
  
  df_sd_all[,i_crop+3]=df_sd[,3]
  df_mean_all[, i_crop+3]=df_mean[,3]

  
} #i_crop

INPUT_AGG_SD=df_sd_all 
INPUT_AGG_MEAN=df_mean_all 

INPUT_AGG_SD=rbind(INPUT_AGG_SD,INPUT_AGG_SD[1,])
INPUT_AGG_SD[1,3]="2030-2060"
INPUT_AGG_SD[22,3]="2060-2090"

INPUT_AGG_MEAN=rbind(INPUT_AGG_MEAN,INPUT_AGG_MEAN[1,])
INPUT_AGG_MEAN[1,3]="2030-2060"
INPUT_AGG_MEAN[22,3]="2060-2090"

# ------------------------    Figure 2-A

to_gg_mean=melt(INPUT_AGG_MEAN[,3:13], id.vars = "Period", 
                measure.vars =c('Potatoes', 'SW', 'WW', 'Corn',  'Alfalfa',  'Pasture',    'Grape',  'Apple', 'Cherries', 'Pear'))

to_gg_mean=cbind(to_gg_mean,rcp_ts, crop_group)


P_A_mean=ggplot(to_gg_mean, aes(variable, y=value,  fill=rcp_ts)) +  
  geom_boxplot()+ 
  scale_fill_manual(values=c(color_yv))+ scale_color_manual(values=c(color_yv)) +
  labs(title="a. Average Yield", x="Crop Type",
       y = "Yield (tonnes/ha)", x="Crop Type") +
  #facet_grid(Period~.)+
  facet_wrap(~Period+crop_group ,dir = "h", scales = "free")+ theme(legend.position = "none")+
  theme(axis.title.x = element_blank(), legend.key.size = unit(0.9, "lines"), legend.title = element_blank())

ggsave(paste(output_data_folder, "Mean_plot.jpg", sep = ""), plot = P_A_mean)

#----------------------   Figure 2-B

rcp_ts=rep(c("historical", rep(c("RCP_4.5", "RCP_8.5"), 10), "historical"), 10)
crop_group=c(rep("Annual-Crops", 4*22), rep("Multiple-cutting-Crops", 2*22), rep("Tree-Fruits",22*4) )


to_gg_sd=melt(INPUT_AGG_SD[,3:13], id.vars = "Period", 
           measure.vars =c('Potatoes', 'SW', 'WW', 'Corn',  'Alfalfa',  'Pasture',    'Grape',  'Apple', 'Cherries', 'Pear') )

to_gg_sd=cbind(to_gg_sd,rcp_ts, crop_group)


P_B_sd=ggplot(to_gg_sd, aes(variable, y=value, fill=rcp_ts)) +  
  geom_boxplot()+ 
  scale_fill_manual(values=c(color_yv))+ scale_color_manual(values=c(color_yv)) +
  labs(title="b. Inter-Annual Variability in Crop Yield ",
       y = "Standard Deviation (tonnes/ha)", x="Crop Type") +
  #facet_grid(year~.)+
  facet_wrap(~Period+crop_group  ,dir = "h", scales = "free")+ theme(legend.position = "none")+
  theme(axis.title.x = element_blank(), legend.key.size = unit(0.9, "lines"), legend.title = element_blank())


ggsave(paste(output_data_folder, "SD_plot.jpg", sep = ""), plot = P_B_sd)



GG1=ggarrange(P_A_mean, P_B_sd, nrow = 2, common.legend = T ,legend = "bottom")

ggsave(paste(output_data_folder, "Figure_2_Combined.jpg"), plot = GG1, width = 9, height = 11 )

