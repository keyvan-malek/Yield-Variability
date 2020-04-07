
########################################################################################
######   This code reads VIC-CropSyst outputs and generate crop response curves for
######    all GCMs, RCPs, gridcells and crop types
########################################################################################

decades=c(2020, 2030,2040,2050, 2060,2070,2080)

soil_file=read.table("PATH_TO_FILE/soil_file_climate.txt", header=T) 

list_of_gcm=c("CanESM2","GFDL-ESM2G", "HadGEM2-CC365", "HadGEM2-ES365","inmcm4")

proration_list=seq(0,1,0.05) 

cropparam=read.table("PATH_TO_FILE/cropparam_waterrightAdded.txt", header=T) 

crop_types= c("1401", "218", "204", "701", "2001", "2002", "1403", "1101", "210", "1409", "1827")
 
list_of_rcps=c("rcp45","rcp85")

vic_output_directory= ("PATH_TO_DITRCTORY")

for (i_crop in 1:length(crop_types)){
 
   cell_list=which(cropparam[,2]==crop_types[i_crop])
  
  for (i_cell in 1:length(cell_list)){
    
    
    i_row=cell_list[i_cell]
    while(cropparam[i_row,1]==0){
      cell_idx=cropparam[i_row,1]
      i_row=i_row-1
    }
    cell_idx=cropparam[i_row,1] 
    number_crop=cropparam[i_row,2]
    cell_name=paste("fluxes_crop_",soil_file[which(soil_file[,2]==cell_idx),3] ,"_", soil_file[which(soil_file[,2]==cell_idx),4], sep="")
    crop_rank=cell_list[i_cell]-i_row 

    yield_column= 7+crop_rank
    
    if(cropparam[cell_list[i_cell],7]>0){  
    for (i_decade in 1:length(decades)){
      
      to_be_written=data.frame(matrix(nrow=length(proration_list), ncol=length(list_of_rcps)*length(list_of_gcm) ))
      
    for (i_rcp in 1:length(list_of_rcps)){
     for (i_gcm in 1: length(list_of_gcm)){
       for (i_proration in 1:length(proration_list)){
         
         VIC_output= read.table(paste(vic_output_directory, "/", list_of_gcm[i_gcm],"_", list_of_rcps[i_rcp], "/", proration_list[i_proration]
                                      , "/", cell_name , sep="" ))
         

      
         decade_rows=which(VIC_output[,1]==decades[i_decade])
         start_row=decade_rows[1]
        specific_ts=VIC_output[start_row:(start_row+365*10),yield_column]
        mean_for_cell_crop=sum(specific_ts)/10
        to_be_written[i_proration,i_gcm+length(list_of_gcm)*(i_rcp-1)]=mean_for_cell_crop
     # plot(specific_ts)
 
      
    }# i_proration
  } # i_gcm
} # i_rcp
      
      print(paste ( crop_types[i_crop], "--------", cell_idx, "----------", decades[i_decade] ))
      
      write.table(to_be_written, file=paste("PATH_TO_FILE/crop_cell_files/",  crop_types[i_crop], "_", 
                                            cell_idx,"_", decades[i_decade], ".txt" ,sep=""), col.names = F, row.names = F) # to change
      
} # i_decade
    } #if irrigated
} # i_cell
}# i_crop