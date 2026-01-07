######################################################################
# Purpose: CanOSSEM raster cell num assignment for NAPS/PM2.5 measurements
# Author: namanpaul
# Last modified by: namanpaul
# R Version: 4.3.0
# Date: 2024-12-29
#####################################################################

#loading the packages
library(tidyverse)
library(foreach)



# input -------------------------------------------------------------------

call the base raster script that creates base raster and converts it to 4326
#instead loading the the CanOSSEM 4326 dataframe

#set working directory to the source directory
#setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
#call the base raster script and convert it to 4326
load('../CanOSSEM_4326.RData')

xy_base_raster_4326 <- sapply(CanOSSEM_base_raster_4326_dataframe[, 2:3], as.numeric)

#NAPS o/p path
NAPS_output_path <- "NAPS_output_path"

#NAPS input path
load("Canada_NAPS_PM25_2010_2023.RData")


#keep distinct stations only
Distinct_stations <- Canada_NAPS_PM25_2010_2023 %>% 
  filter(Latitude >= 40) %>% 
  distinct(Longitude, Latitude) %>% 
  ungroup() %>% 
  select(SiteID) %>% 
  distinct()


#filtering for only the distinct stations
Canada_NAPS_PM25_2010_2023 <- Canada_NAPS_PM25_2010_2023 %>% 
  filter(SiteID %in% Distinct_stations$SiteID)


NAPS_stations <- Canada_NAPS_PM25_2010_2023 %>% 
  ungroup() %>% 
  dplyr::select(-ValidDate, -daily_mean_pm25) %>% 
  distinct() 

NAPS_stations <- NAPS_stations %>%
  mutate(NAPS_row_num = 1:nrow(NAPS_stations))
  


# NAPS_stations -----------------------------------------------------------

#xy of NAPS
xy_NAPS <- sapply(NAPS_stations[, 3:4], as.numeric)


#converting the spdf into dataframe
df_spatial_NAPS <- as.data.frame(NAPS_stations)


#cleaning up the df
df_spatial_NAPS <- df_spatial_NAPS %>% 
  mutate(NAPS_row_num = 1:nrow(df_spatial_NAPS)) %>% 
  rename(NAPS_lon = Longitude,
         NAPS_lat = Latitude) 


#RANN nn2()

nn2output <- RANN::nn2(xy_NAPS,
                       query = xy_base_raster_4326,
                       k =1,
                       treetype = "kd",
                       searchtype = "priority")

#converting the rastercell indices into a dataframe
nn2outputdf <- as.data.frame(nn2output[["nn.idx"]])

#renaming the var, and adding a row count  
nn2outputdf_kd_priority <- nn2outputdf %>% 
  mutate(dists = nn2output[["nn.dists"]]) %>%
  rename(NAPS_row_num = V1) %>% 
  mutate(CanOSSEM_rCN = 1:nrow(nn2outputdf))

#creating a lon-lat df
nn2_lonlat <- left_join(nn2outputdf_kd_priority,
                        CanOSSEM_base_raster_4326_dataframe,
                        by = "CanOSSEM_rCN") 

#doing the joining specifically for NAPS, alongwith the NAPS lon-lat vals
nn2_NAPS <- left_join(nn2_lonlat,
                      df_spatial_NAPS,
                      by = "NAPS_row_num")


#removing the NAPS rownum since it won't be required beyond
nn2_NAPS <- nn2_NAPS %>% 
  dplyr::select(-NAPS_row_num)


#NAPS lon/lat
lon1 <- as.numeric(nn2_NAPS$NAPS_lon)
lat1 <- as.numeric(nn2_NAPS$NAPS_lat)

#base raster lon/lat
lon2 <- as.numeric(nn2_NAPS$longitude)
lat2 <- as.numeric(nn2_NAPS$latitude)


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

NAPS_distance <- as.data.frame(haversine_distance(lon1, lat1, lon2, lat2))

NAPS_distance_new <- bind_cols(nn2_NAPS,NAPS_distance)

NAPS_distance_new <- NAPS_distance_new %>% 
  dplyr::rename(`Distance (km)` = `haversine_distance(lon1, lat1, lon2, lat2)`) %>% 
  dplyr::select(-dists)


#dropping all the values where distance >50
NAPS_distance_new <- NAPS_distance_new %>%
  filter(`Distance (km)`<=50) 


#count how many times a rCN is repeated for a site
CanOSSEM_rCN_repeats <- NAPS_distance_new %>% 
  group_by(CanOSSEM_rCN) %>% 
  count(SiteID)


#reducing the cols
NAPS_ID_rCN <- NAPS_distance_new %>% 
  dplyr::select(CanOSSEM_rCN, SiteID, `Distance (km)`)

#merge it with the NAPS_stations_data_cleaned
NAPS_ID_rCN_assigned <- inner_join(Canada_NAPS_PM25_2010_2023,
                                   NAPS_ID_rCN,
                                   by = "SiteID")

#select relevant vars
NAPS_ID_rCN_assigned <- NAPS_ID_rCN_assigned %>% 
  na.omit()


save(NAPS_ID_rCN_assigned,
     file = 'path/NAPS_ID_rCN_assigned_2010_2023.RData')


