###################################################
######
######   THIS CODE AGGREGATES CRCS
######  
####################################################

#decades=c(2020, 2030,2040,2050, 2060,2070,2080)

soil_file=read.table("PATH_TO_DIRECTORY/soil_file_climate.txt", header=T) 

list_of_gcm=c("CanESM2","GFDL-ESM2G", "HadGEM2-CC365", "HadGEM2-ES365","inmcm4")

proration_list=seq(0,1,0.05) 
cropparam=read.table("PATH_TO_DIRECTORY/cropparam_waterrightAdded.txt", header=T) #  to change

crop_types= c("1401", "218", "204", "701", "2001", "2002", "1403", "1101", "210", "1409", "1827")

# Apples, WinWheat, Corn, Alfalfa, GrapeJuice, Grapes, CherryOrchard, Mint,SpgWheat, Pear,Potatoes

list_of_rcps=c("rcp45","rcp85")

vic_output_directory= ("PATH_TO_DIRECTORY/crop_cell_files/") #  to change

for (i_crop in 1:length(crop_types)){
  
  cell_list=which(cropparam[,2]==crop_types[i_crop])
  
  for (i_decade in 1:length(decades)){
    
    aggregate_over_crop_type=0
    low_prec_aggregate_over_crop_type=0
    high_prec_aggregate_over_crop_type=0
    n_low_cells=0
    n_high_cells=0
    
    for (i_cell in 1:length(cell_list)){
      
      
      i_row=cell_list[i_cell]
      while(cropparam[i_row,1]==0){
        cell_idx=cropparam[i_row,1]
        i_row=i_row-1
      }
      cell_idx=cropparam[i_row,1] 
      number_crop=cropparam[i_row,2]
      
      crop_rank=cell_list[i_cell]-i_row 
      
      yield_column= 7+crop_rank
      
      if(cropparam[cell_list[i_cell],7]>0){  
        
        
        to_be_written=data.frame(matrix(nrow=length(proration_list), ncol=length(list_of_rcps)*length(list_of_gcm) ))
        
        
        
        file_name=paste( crop_types[i_crop], "_", cell_idx,"_", decades[i_decade], ".txt", sep="")
        if(file.exists(paste(vic_output_directory,  file_name, sep="" ))){
          
          VIC_output= read.table(paste(vic_output_directory,  file_name, sep="" ))
          
          aggregate_over_crop_type=aggregate_over_crop_type+VIC_output
          
          #############################
          # for different climates
          #############################
          if(soil_file[which(soil_file[,2]==cell_idx),6]<234){
            low_prec_aggregate_over_crop_type=low_prec_aggregate_over_crop_type+VIC_output
            n_low_cells=n_low_cells+1
          }
          
          if(soil_file[which(soil_file[,2]==cell_idx),6]>234){
            high_prec_aggregate_over_crop_type=high_prec_aggregate_over_crop_type+VIC_output
            n_high_cells=n_high_cells+1
          }
          
          
        } #if file exists  
        
        write.table(aggregate_over_crop_type/length(cell_list), file=paste("PATH_TO_DIRECTORY",  crop_types[i_crop], "_", 
                                                                           decades[i_decade], ".txt" ,sep=""), col.names = F, row.names = F) #to change
        
        write.table(low_prec_aggregate_over_crop_type/n_low_cells, file=paste("PATH_TO_DIRECTORY",  crop_types[i_crop], "_", 
                                                                              decades[i_decade], ".txt" ,sep=""), col.names = F, row.names = F) #to change
        
        write.table(high_prec_aggregate_over_crop_type/n_high_cells, file=paste("PATH_TO_DIRECTORY",  crop_types[i_crop], "_", 
                                                                                decades[i_decade], ".txt" ,sep=""), col.names = F, row.names = F) #to change
      } #if irrigated
    } # i_cell
    
  } # i_decade
}# i_crop
