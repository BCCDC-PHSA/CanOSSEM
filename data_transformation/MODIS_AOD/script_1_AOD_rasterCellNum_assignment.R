#####################################################################
# Purpose:  CanOSSEM raster cell num assignment for AOD extract  
# Author: namanpaul
# Last modified by: namanpaul
# R Version: 4.3.0
# Date: 2024-12-30
#####################################################################

# load the packages
library(tidyverse)
library(RANN)
library(foreach)

#call the base raster script that creates base raster and converts it to 4326
#instead loading the the CanOSSEM 4326 dataframe

#set working directory to the source directory
#setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# load the CanOSSEM base raster grid data
load("../CanOSSEM_4326.RData")

xy_base_raster_4326 <- sapply(CanOSSEM_base_raster_4326_dataframe[, 2:3], as.numeric)


# get i/p and o/p path
MOD_input_path <- "path/AOD_extracted/MOD/2023"
MYD_input_path <- "path/AOD_extracted/MYD/2023"
AOD_output_path <- "path/AOD_rCN_assigned/2023/"


# list the files for the year
MOD_files <- list.files(path = paste0(MOD_input_path),
                        pattern ="*.RData", full.names=TRUE, recursive=TRUE)

MYD_files <- list.files(path = paste0(MYD_input_path),
                        pattern ="*.RData", full.names=TRUE, recursive=TRUE)

# all files
MOD_MYD_files <- c(MOD_files, MYD_files)

# test if all days are available

# Generate the complete list of days in yyyy_jjj format (max possible val, 366)
all_julian_days <- sprintf("%03d", 1:366)

# Extract the Julian day (jjj) from file names
existing_julian_days <- sub("^.*_([0-9]{3})_.*$", "\\1", MOD_MYD_files)

# Identify missing Julian days
missing_julian_days <- setdiff(all_julian_days, existing_julian_days)


# go through one day at a time
year_val <- 2023

# create the date range
date_range <- seq(as.Date(paste(year_val,"-01-01", sep = "")),
                  as.Date(paste(year_val,"-01-02", sep = "")),
                  by=1)

# set the format to %Y%m%d
date_range <- format(date_range, "%Y_%j")

# loop thru
foreach(i = seq_along(date_range)) %do% {
  
  # print
  print(date_range[i])
  
  #list the files
  MOD_filelist_forday <- list.files(path = MOD_input_path,
                                    pattern = paste("*",date_range[i],sep="_"),
                                    full.names = T, recursive = T)
  
  MYD_filelist_forday <- list.files(path = MYD_input_path,
                                    pattern = paste("*",date_range[i],sep="_"),
                                    full.names = T, recursive = T)
  
  MOD_MYD_filelist_forday <- c(MOD_filelist_forday, MYD_filelist_forday)
  
  # now bind the files for the day
  for(k in seq_along(MOD_MYD_filelist_forday)) {
    
    load(MOD_MYD_filelist_forday[k])
    
    assign(paste("dfs", k, sep="_"), raster_df_clean)
    
    #list of dataframes
    list_df <- lapply(ls(pattern="dfs+"), function(x) get(x))
    
    # AOD bound of the list of dfs
    AOD_bound <- dplyr::bind_rows(list_df) %>% 
      na.omit() %>% 
      select(-time_CST) %>% 
      distinct() %>% 
      rename(AOD_lon = longitude,
             AOD_lat = latitude) %>% 
      mutate(AOD_row_num = 1:nrow(.))
    
    
  }
  
  
  # remove the files
  rm(list=ls(pattern="dfs+"))
  
  
  # xy of the AOD
  xy_AOD <- sapply(AOD_bound[, 1:2], as.numeric)
  
  #RANN nn2()
  #trying nn2 function
  nn2output <- RANN::nn2(xy_AOD,
                         query = xy_base_raster_4326,
                         k =1,
                         treetype = "kd",
                         searchtype = "priority")
  
  #converting the rastercell indices into a dataframe
  nn2outputdf <- as.data.frame(nn2output[["nn.idx"]])
  
  #renaming the var, and adding a row count  
  nn2outputdf_kd_priority <- nn2outputdf %>% 
    mutate(dists = nn2output[["nn.dists"]]) %>%
    rename(AOD_row_num = V1) %>% 
    mutate(CanOSSEM_rCN = 1:nrow(nn2outputdf))
  
  #creating a lon-lat df
  nn2_lonlat <- left_join(nn2outputdf_kd_priority,
                          CanOSSEM_base_raster_4326_dataframe,
                          by = "CanOSSEM_rCN") #%>% 
  #dplyr::select(-lon_3347, -lat_3347)
  
  #doing the joining specifically for AOD, alongwith the AOD lon-lat vals
  nn2_AOD <- left_join(nn2_lonlat,
                       AOD_bound,
                       by = "AOD_row_num")
  
  #removing the AOD rownum since it won't be required beyond
  nn2_AOD <- nn2_AOD %>% 
    dplyr::select(-AOD_row_num)
  
  
  #AOD lon/lat
  lon1 <- as.numeric(nn2_AOD$AOD_lon)
  lat1 <- as.numeric(nn2_AOD$AOD_lat)
  
  #base raster lon/lat
  lon2 <- as.numeric(nn2_AOD$longitude)
  lat2 <- as.numeric(nn2_AOD$latitude)
  
  
  #define the function to calculate haversine distance
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
  
  aod_distance <- as.data.frame(haversine_distance(lon1, lat1, lon2, lat2))
  
  aod_distance_new <- bind_cols(nn2_AOD, aod_distance)
  
  aod_distance_new <- aod_distance_new %>% 
    dplyr::rename(`Distance (km)` = `haversine_distance(lon1, lat1, lon2, lat2)`) %>% 
    dplyr::select(-dists)
  
  
  # dropping all the values where distance >50
  AOD_new_distance <- aod_distance_new %>%
    filter(`Distance (km)`<=50) %>% 
    add_count(CanOSSEM_rCN, name = "count_cells", .drop = F)
  
  # summarising for each cell
  averaged_AOD_data <- AOD_new_distance %>% 
    dplyr::select(-satellite_name) %>%
    group_by(CanOSSEM_rCN) %>%
    summarise_all(mean, na.rm=T)
  
  #fill up the values for other CanOSSEM_rCN
  AOD_filled_up_base <- averaged_AOD_data %>% 
    dplyr::select(-count_cells, -longitude, -latitude, -AOD_lon, -AOD_lat) %>% 
    tidyr::complete(CanOSSEM_rCN = seq(1:970215)) %>%
    dplyr::rename(ACQ_DATE = acq_date) %>% 
    padr::fill_by_value(value = NA)
  
  
  # acq date
  ACQ_DATE <- max(AOD_filled_up_base$ACQ_DATE, na.rm = T)
  
  save(AOD_filled_up_base,
       file = paste0(AOD_output_path,'AOD_daily_data_', ACQ_DATE,'.RData'))
  
}