
library(ggplot2)
library(reshape2)
library(ggpubr)

#####################################
#
#               Figure S4
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

plot_list_S4 = list()


for (i_crop in 1:length(crop_types)){
  
  
  actual_yield_ts_df=data.frame(matrix(nrow = 60, ncol = 12))
  max_yield_ts_df=data.frame(matrix(nrow = 60, ncol = 12))
  def_yield_ts_def=data.frame(matrix(nrow = 60, ncol = 12))
  
  names(actual_yield_ts_df)=c("year",list_of_gcm, "historical")
  names(max_yield_ts_df)=c("year",list_of_gcm, "historical")
  names(def_yield_ts_def)=c("year",list_of_gcm, "historical")
  
  
  
  actual_yield_ts_df[1:60,1]=seq(2030, 2089)
  max_yield_ts_df[1:60,1]=seq(2030, 2089)
  def_yield_ts_def[1:60,1]=seq(2030, 2089)
  
  for (i_year in 1:60){
    
    decade=10*floor((i_year+2029)/10)
    yield_file=read.table(paste(input_data_folder, "CRCs/", crop_types[i_crop], "_", decade, ".txt", sep=""))
    
    for (i_gcm in 1:10){
      
      proration=proration_future[i_year+12,i_gcm+1]
      p_round=5*floor(proration*100/5) 
      
      actual_yield_ts_df[i_year,1+i_gcm]=yield_file[1+p_round/5,i_gcm]
      max_yield_ts_df[i_year,1+i_gcm]=yield_file[21,i_gcm]
      def_yield_ts_def[i_year,1+i_gcm]=yield_file[21,i_gcm]-yield_file[1+p_round/5,i_gcm]
      
    }  # i_gcm
    
  } # i_year
  
  
  
  ########### historical
  for (i_year in 1:30){
    
    decade=10*floor((i_year+1979)/10)
    yield_file=read.table(paste(input_data_folder, "CRCs/", crop_types[i_crop], "_", decade, ".txt", sep=""))
    
    
    proration=proration_hist[i_year,2]
    p_round=5*floor(proration*100/5) 
    
    actual_yield_ts_df[i_year,12]=yield_file[1+p_round/5,1]
    max_yield_ts_df[i_year,12]=yield_file[21,1]
    def_yield_ts_def[i_year,12]=yield_file[21,1]-yield_file[1+p_round/5,1]
    
    
  } # i_year

  ######### generating plots
  first_year=43 #13 #
  last_year=72 #42 #
  year_sim="2060-2090" #"2030-2060"
  
  
  #------------ data to ggplot
   
  df_to_mlt=proration_future[13:72,]
  names(df_to_mlt)=c("period", rep(c("rcp_4.5", "rcp_8.5"),5))
  df_to_mlt[,1]=c(rep("2030-2060", 30), rep("2060-2090", 30)) 
  
  mlt_2x<-melt(df_to_mlt[,1:11], id.vars ="period")
  mlt_2x[,2]=rep(c(rep("RCP_4.5", 60), rep("RCP_8.5", 60)), 5)

  y_df_to_mlt<-def_yield_ts_def
  names(y_df_to_mlt)=c("period", rep(c("rcp_4.5", "rcp_8.5"),5))
  y_df_to_mlt[,1]=c(rep("2030-2060", 30), rep("2060-2090", 30)) 
  
  mlt_2y<-melt(y_df_to_mlt[,1:11], id.vars ="period")
  mlt_2y[,2]=rep(c(rep("RCP_4.5", 60), rep("RCP_8.5", 60)), 5)  

  mlt_2=cbind(mlt_2x,mlt_2y[,3])
  names(mlt_2)<-c("Period", "RCP", "Unmet_Demand" , "Yield")
  mlt_2<-rbind(mlt_2, mlt_2[1:30,])
  mlt_2[601:630,3]=proration_hist[1:30,2]
  mlt_2[601:630,4]=def_yield_ts_def[1:30,12]
  mlt_2[601:630,1]=c(rep("1980-2010", 30))
  mlt_2[601:630,2]=c(rep("Historical", 30))
  mlt_2[,3]=1-mlt_2[,3]
  
 P_S4<- ggplot(mlt_2, aes(x = Unmet_Demand, y=Yield, colour=RCP)) +  geom_point()  + 
  scale_color_manual(values=c(color_yv)) +
   theme(legend.title = element_blank())+ theme(axis.title.x = element_blank()) + theme(axis.title.y = element_blank()) +
   theme(legend.position = "bottom") + 
   scale_size_manual(values=c(1.8,1,1)) + 
   annotate(geom = "text",x = 0.1, y = 0.9*max(mlt_2$Yield), label=paste(Crop_name_title[i_crop]) ) +
   scale_x_continuous(breaks=seq(0,1,by = 0.2), limits = c(0,1)) 
  

  plot_list_S4[[i_crop]]=P_S4
  
  
} #i_crop

plt_2all<-ggarrange(plotlist = plot_list_S4[1:10] , nrow = 5, ncol=2, common.legend = T, legend = "bottom")


ggsave(paste(output_data_folder, "combined_S4.png", sep = ""), plt_2all, width = 6.75, height = 8)
