
library(scatterplot3d) 
library(ggplot2)
library(ggpubr)
library(reshape2)

#################################################
#
#  Figure 1-A and 1-B
#
##############################################

input_data_folder<-"PATH_TO_DIRECTORY" # Path to the input folder
output_data_folder<-"PATH_TO_DIRECTORY" # path to the output folder

color_yv=c("steelblue4", "green3", "goldenrod")


input_file=read.table(paste(input_data_folder, "inputs_for_3d_plot.txt", sep=""), header = T)


#----------------- Figure 1-A

colors <- color_yv
colors <- colors[as.numeric(input_file$RCP)]
sizes=c(1.5,1,1)
sizes <- sizes[as.numeric(input_file$RCP)]

png(filename = paste(output_data_folder, "Figure_1-A.png", sep=""), width = 600, height = 600)

scatterplot3d(x=input_file$Tavg, y=input_file$PREC, z=100*(1-input_file$Proration), color=colors, cex.symbols = sizes, 
              angle = 65,axis = T,box = T, #type = "h",lty.hplot = 2,
              col.grid = T, pch = 19, bg = T, 
              main="Unmet demand vs Temperature and Precipitation",
              xlab = "Temperature (C)",
              ylab = "Precipitation (mm)",
              zlab = "Unmet Demand")


dev.off()

#----------------- Figure 1-B


P1_B<-ggplot(input_file, aes(Tavg, 100*(1-Proration))) +# stat_ecdf(geom = "step") +
  #geom_line(aes(y= Max_flow, colour = "Max_flow"),colour="darkolivegreen3",size=2)+
  geom_boxplot(aes( fill = RCP),fatten = 1)+theme_bw()+
  labs(title="b. Temperature vs Unmet Demand",
       y = "Unmet Demand (%)", x="Tavg (C)")+ scale_fill_manual(values=c(color_yv)) #

ggsave(paste(output_data_folder, "Figure_1-B.PNG"), plot = P1_B)


#--------------- Figure 1-C



#################################################
#
#  Figure 1-C and 1-D
#
##############################################


hist_proration=read.table(paste(input_data_folder, "timeseries_proration_hist.txt", sep = ""), header = T)
future_proration=read.table(paste(input_data_folder, "timeseries_proration.txt", sep = ""), header = T)


hist_norm=rnorm(30, mean = mean(hist_proration[,2]), sd = sd(hist_proration[,2]))


#------------------------ Figure 1-C

mean_2030=apply(future_proration[13:42,2:11],1,mean,na.rm=TRUE)
max_2030=apply(future_proration[13:42,2:11],1,max)
min_2030=apply(future_proration[13:42,2:11],1,min,na.rm=TRUE)
#labs(title="2030-2060 Cumulative Density Function", y = "F(Proration)", x="Proration Ratio")# + theme_classic()

df_2030_2060 <- data.frame(
  Historical = sort(hist_proration[1:30,2]),
  avg = sort(mean_2030),
  avg_4.5 = sort(mean_2030),
  avg_8.5 = sort(mean_2030),
  
  scr_1 = sort(future_proration[13:42,2]),
  scr_2 = sort(future_proration[13:42,4]),
  scr_3 = sort(future_proration[13:42,6]),
  scr_4 = sort(future_proration[13:42,8]),
  scr_5 = sort(future_proration[13:42,10]),
  
  scr_6 = sort(future_proration[13:42,3]),
  scr_7 = sort(future_proration[13:42,5]),
  scr_8 = sort(future_proration[13:42,7]),
  scr_9 = sort(future_proration[13:42,9]),
  scr_10 = sort(future_proration[13:42,11]),

  g = seq(0,1, length.out = 30)
  )
df_2030_2060$avg=apply(df_2030_2060[1:30,5:14],1,mean)
df_2030_2060$max=apply(df_2030_2060[1:30,5:14],1,max)
df_2030_2060$min=apply(df_2030_2060[1:30,5:14],1,min)

df_2030_2060$avg_4.5=apply(df_2030_2060[1:30,5:9],1,mean)
df_2030_2060$max_4.5=apply(df_2030_2060[1:30,5:9],1,max)
df_2030_2060$min_4.5=apply(df_2030_2060[1:30,5:9],1,min)

df_2030_2060$avg_8.5=apply(df_2030_2060[1:30,10:14],1,mean)
df_2030_2060$max_8.5=apply(df_2030_2060[1:30,10:14],1,max)
df_2030_2060$min_8.5=apply(df_2030_2060[1:30,10:14],1,min)

P1_C=ggplot(df_2030_2060, aes(g))+ coord_flip() +
 geom_ribbon(aes(ymin=min, ymax=max), fill="lightcyan3", alpha=0.5) +
  geom_line(aes(y = avg, colour = "GCM Average"), size=1) + 
  geom_line(aes(y = avg_4.5, colour = "RCP_4.5"), size=1.7, linetype = "dashed") +
  geom_line(aes(y = avg_8.5, colour = "RCP_8.5"), size=1.7, linetype = "dashed")+
  geom_line(aes(y = Historical, colour = "Historical"), size=2 ) +
  labs(title="c. 2030-2060 Cumulative Density Function") + xlab("P(Unmet Demand)")+ ylab("Unmet Demand (%)") +
  scale_y_continuous(breaks = seq(0, 1, by =0.2), label =c("100","80", "60", "40", "20", "0"))+
  scale_x_continuous(breaks = seq(0, 1, by =0.2))+
  scale_color_manual(values=c("black", color_yv)) +theme_bw()

ggsave(paste(output_data_folder, "Figure_1-C.PNG", sep = ""), plot = P1_C)


#----------------- Figure 1-D

mean_2060=apply(future_proration[43:72,2:11],1,mean,na.rm=TRUE)
max_2060=apply(future_proration[43:72,2:11],1,max)
min_2060=apply(future_proration[43:72,2:11],1,min,na.rm=TRUE)


df_2060_2090 <- data.frame(
  Historical = sort(hist_proration[1:30,2]),
  avg = sort(mean_2060),
  avg_4.5 = sort(mean_2060),
  avg_8.5 = sort(mean_2060),
  
  scr_1 = sort(future_proration[43:72,2]),
  scr_2 = sort(future_proration[43:72,4]),
  scr_3 = sort(future_proration[43:72,6]),
  scr_4 = sort(future_proration[43:72,8]),
  scr_5 = sort(future_proration[43:72,10]),
  
  scr_6 = sort(future_proration[43:72,3]),
  scr_7 = sort(future_proration[43:72,5]),
  scr_8 = sort(future_proration[43:72,7]),
  scr_9 = sort(future_proration[43:72,9]),
  scr_10 = sort(future_proration[43:72,11]),
  
  max=sort(max_2060),
  min=sort(min_2060),  
  max_4.5=sort(max_2060),
  min_4.5=sort(min_2060),
  max_8.5=sort(max_2060),
  min_8.5=sort(min_2060),
  g = seq(0,1, length.out = 30)
)
df_2060_2090$avg=apply(df_2060_2090[1:30,5:14],1,mean)
df_2060_2090$max=apply(df_2060_2090[1:30,5:14],1,max)
df_2060_2090$min=apply(df_2060_2090[1:30,5:14],1,min)

df_2060_2090$avg_4.5=apply(df_2060_2090[1:30,5:9],1,mean)
df_2060_2090$max_4.5=apply(df_2060_2090[1:30,5:9],1,max)
df_2060_2090$min_4.5=apply(df_2060_2090[1:30,5:9],1,min)

df_2060_2090$avg_8.5=apply(df_2060_2090[1:30,10:14],1,mean)
df_2060_2090$max_8.5=apply(df_2060_2090[1:30,10:14],1,max)
df_2060_2090$min_8.5=apply(df_2060_2090[1:30,10:14],1,min)

P1_D=ggplot(df_2060_2090, aes(g))+ coord_flip() +
  geom_ribbon(aes(ymin=min, ymax=max), fill="lightcyan3", alpha=0.5)+
  geom_line(aes(y = avg, colour = "GCM Average"), size=1) +
  geom_line(aes(y = avg_4.5, colour = "RCP_4.5"), size=1.2, linetype = "dashed") +
  geom_line(aes(y = avg_8.5, colour = "RCP_8.5"), size=1.2, linetype = "dashed")+
  geom_line(aes(y = Historical, colour = "Historical"), size=2 )+
  labs(title="d. 2060-2090 Cumulative Density Function") + xlab("P(Unmet Demand)")+ ylab("Unmet Demand (%)") +
  scale_x_continuous(breaks = seq(0, 1, by =0.2), label =c("0","0.2", "0.4", "0.6", "0.8", "1")) +
  scale_y_continuous(breaks = seq(0, 1, by =0.2), label =c("100","80", "60", "40", "20", "0"))+
  scale_color_manual(values=c("black", color_yv)) +theme_bw() 

ggsave(paste(output_data_folder, "Figure_1-D.PNG", sep = ""), plot = P1_D)

