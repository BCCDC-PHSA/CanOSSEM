######################################################################
# Purpose: CanOSSEM raster cell num assignment for MERRA extract
# Author: namanpaul
# Last modified by: namanpaul
# R Version: 4.3.0
# Date: 2024-12-29
#####################################################################

#loading the packages
library(tidyverse)
library(foreach)


#call the base raster script that creates base raster and converts it to 4326
#instead loading the the CanOSSEM 4326 dataframe

#set working directory to the source directory
#setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

load('../CanOSSEM_4326.RData')

xy_base_raster_4326 <- sapply(CanOSSEM_base_raster_4326_dataframe[, 2:3], as.numeric)

# specifying input  -------------------------------------------------------

#MERRA i/p path
MERRA_input_path <- "path/MERRA_bound/"
MERRA_output_path <- "path/MERRA/"
#year_val <- 2016



# function_definition -----------------------------------------------------


rCN_assign_MERRA <- function(MERRA_input_path, MERRA_output_path,year_val){
  
  #create the date range in julian days
  date_range <- format(seq(as.Date(paste(year_val,"-01-01", sep = "")),
                           as.Date(paste(year_val,"-12-31", sep = "")),
                           by=1), "%Y-%m-%d")
  
  foreach(i = seq_along(date_range)) %do% {
    
    #print the date
    print(date_range[i])
    
    #load the MERRA df
    load(paste0(MERRA_input_path,"MERRA_data_bound_",date_range[i],".RData"))
    
    #daily mean calculation alongwith max values for TLML and QLML
    daily_mean_MERRA <- MERRA_bound %>%
      select(-UTC_time, -CST_time) %>% 
      group_by(longitude, latitude, ACQ_DATE) %>%
      mutate(max_TLML = max(TLML, na.rm = T),
             max_QLML = max(QLML, na.rm = T),
             min_TLML = min(TLML, na.rm = T),
             min_QLML = min(QLML, na.rm = T)) %>% 
      ungroup() %>% 
      select(longitude, latitude, ACQ_DATE, max_TLML:min_QLML, everything())
    
    # max cols
    keep_max <- daily_mean_MERRA %>% 
      select(longitude, latitude, ACQ_DATE, max_TLML:min_QLML) %>% 
      distinct()
      
    # calculate the daily mean
    daily_mean_MERRA <- daily_mean_MERRA %>% 
      group_by(longitude, latitude, ACQ_DATE) %>%
      summarise_at(vars(PBLH:V2M), mean, na.rm = TRUE) %>% 
      ungroup() %>% 
      mutate(MERRA_row_num = 1:nrow(.)) %>% 
      left_join(., keep_max,
                by = c('longitude','latitude','ACQ_DATE'))
    
    
    #xy of MERRA
    xy_MERRA <- sapply(daily_mean_MERRA[, 1:2], as.numeric)
    
    
    #calling the nn2 function
    nn2output <- RANN::nn2(xy_MERRA,
                           query = xy_base_raster_4326,
                           k =1,
                           treetype = "kd",
                           searchtype = "priority")
    
    #converting the indices into a df
    nn2outputdf <- as.data.frame(nn2output[["nn.idx"]])
    
    #finding the closest rCN to MERRA row
    nn2outputdf_kd_priority <- nn2outputdf %>% 
      mutate(dists = nn2output[["nn.dists"]]) %>%
      rename(MERRA_row_num = V1) %>% 
      mutate(CanOSSEM_rCN = 1:nrow(nn2outputdf))
    
    #putting into a df
    nn2_lonlat <- left_join(nn2outputdf_kd_priority,
                            CanOSSEM_base_raster_4326_dataframe,
                            by = "CanOSSEM_rCN") 
    
    
    #nn2 MERRA
    nn2_MERRA <- left_join(nn2_lonlat,
                         daily_mean_MERRA,
                         by = "MERRA_row_num") %>% 
      dplyr::select(-MERRA_row_num)
    
    
    #MERRA lon/lat
    lon1 <- as.numeric(nn2_MERRA$longitude.y)
    lat1 <- as.numeric(nn2_MERRA$latitude.y)
    
    #base raster lon/lat
    lon2 <- as.numeric(nn2_MERRA$longitude.x)
    lat2 <- as.numeric(nn2_MERRA$latitude.x)
    
    #defining the haversine distance function
    haversine_distance <- function(lon1,lat1,lon2,lat2){
      phi1 = lat1 * pi/180
      phi2 = lat2 * pi/180
      delta_phi = (lat2-lat1) * pi/180
      delta_lambda = (lon2-lon1) * pi/180
      R = 6371e3
      x = delta_lambda*cos((phi1+phi2)/2)
      y = delta_phi
      distance = R*sqrt(x^2 + y^2)
      return(distance/1000)
    }
    
    merra_distance <- as.data.frame(haversine_distance(lon1, lat1, lon2, lat2))
    
    merra_distance_bincols <- bind_cols(nn2_MERRA, merra_distance)
    
    merra_distance_new <- merra_distance_bincols %>% 
      rename(`Distance (km)` = `haversine_distance(lon1, lat1, lon2, lat2)`) %>% 
     dplyr::select(-dists)

    #dropping all the values where distance >50
    MERRA_new_distance <- merra_distance_new %>%
      filter(`Distance (km)`<=50) %>% 
      add_count(CanOSSEM_rCN, name = "count_cells", .drop = F) 
      
    
    #fill up the values for other CanOSSEM_rCN
    MERRA_filled_up_base <- MERRA_new_distance %>% 
      dplyr::select(-count_cells, -longitude.x, -latitude.x, -longitude.y, -latitude.y) %>% 
      tidyr::complete(CanOSSEM_rCN = seq(1:970215)) %>% 
      dplyr::mutate(ACQ_DATE = date_range[i]) %>% 
      padr::fill_by_value(value = NA) 
    
    #save the merra data
    save(MERRA_filled_up_base, 
         file = paste0(MERRA_output_path,'MERRA_data_rCN_assigned_', date_range[i], '.RData'),
         compress = T)
    
    gc()
    
  }
}



# function calling --------------------------------------------------------
for(year_val in seq(2010,2023)){
  rCN_assign_MERRA(MERRA_input_path, MERRA_output_path, year_val)
}