#####################################################################
# Purpose: MODIS-FIRMS FRP data extraction
# Author: namanpaul
# Last modified by: namanpaul
# R Version: # use sessionInfo()
# Date: # use Sys.Date()
#####################################################################

# load the packages
library(tidyverse)
library(terra)
library(sf)
library(foreach)

#set working directory to the source directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# set FRP output path
FRP_output_RData_path <- ('FRP_output_path')

# load the CanOSSEM base raster
base_raster <- rast('../../spatial_data/base_raster.grd')


#no. of cells in base raster
num_cells_base_raster <- as.numeric(ncell(base_raster))

#create a df
base_raster_df = as.data.frame(seq(1,num_cells_base_raster,1))
colnames(base_raster_df) = "rasterCellNum"


# creating a larger raster

# save the extent of the base_raster
base_raster_extent <- ext(base_raster)

# Create a 100 km buffer around the base_raster
buffer_distance <- 100000 # in meters

big_raster_extent <- extend(base_raster_extent, buffer_distance)  # Width in meters

big_raster <- extend(base_raster, big_raster_extent)

#no. of cells in base raster
num_cells_big_raster <- as.numeric(ncell(big_raster))


#create a df for the base raster
big_raster_df = as.data.frame(seq(1,ncell(big_raster),1))

colnames(big_raster_df) = "rasterCellNum"




# project the rasters and FRP input
base_raster <- project(base_raster,
                       'EPSG:3347')

big_raster <- project(big_raster,
                      'EPSG:3347')


# read in the shp file
FRP_input <- st_read('G:/INPUT_data/unprocessed_raw_input_data/FRP_input/2023/fire_archive_M-C61_491603.shp')

# transform the coordinate system
FRP_input <- st_transform(FRP_input,
                          "EPSG:3347") 

# extract to big raster
big_raster_with_FRP <- extract(big_raster,
                               FRP_input, xy =T, cells=T)


# keep relevant vars
FRP_input <- FRP_input %>% 
   mutate(ID = 1:nrow(.)) %>% 
   select(ID, LATITUDE, LONGITUDE, FRP, ACQ_DATE, ACQ_TIME) %>% 
   st_drop_geometry()

# join it to the big_raster_with_FRP
big_raster_with_FRP <- left_join(FRP_input,
                                 big_raster_with_FRP,
                                 by = 'ID')


# specify CST, rasterCellNum
big_raster_with_FRP <- big_raster_with_FRP %>% 
   mutate(UTC_time = ymd_hm(str_c(ACQ_DATE, ACQ_TIME, sep = " ")),
          CST_time = UTC_time - hours(6),
          rasterCellNum = cell) %>%
   filter(!is.na(rasterCellNum))  


# FRP df
FRP_df <- big_raster_with_FRP %>% 
   select(rasterCellNum, FRP, ACQ_DATE) %>% 
   dplyr::group_by(rasterCellNum, ACQ_DATE) %>% 
   mutate(FRP_daily_sum = sum(FRP, na.rm = T)) %>% 
   ungroup() %>% 
   select(-FRP) %>% 
   distinct()


# create the date-range as per the current file's year
date_range <- format(seq(as.Date("2023-01-01", sep = ""),
                         as.Date("2023-12-31", sep = ""),
                         by=1), "%Y-%m-%d")





# for loop for going over all the days
foreach(k = date_range) %do% {
   
   # create a smaller df to operate on
   singledate <- FRP_df %>% 
      dplyr::filter(ACQ_DATE == k)
   
   # match with big raster df
   match <- big_raster_df %>%
      left_join(singledate, by = 'rasterCellNum') %>% 
      mutate(ACQ_DATE = k,
             FRP_daily_sum = ifelse(is.na(FRP_daily_sum), 0, FRP_daily_sum)) 
   
   # set values to the big raster
   big_raster <- setValues(big_raster,
                           match$FRP_daily_sum)
   
   # calculate focal values 
   focal_values <- terra::focal(big_raster, w=matrix(1,41,41),
                                fun=sum, na.policy="all", na.rm=TRUE, expand=FALSE, fillvalue=NA)
   # extract focal values to the base_raster extent
   FRP_filled_up_base  <- terra::extract(focal_values,
                                         ext(base_raster),
                                         cells = T)
   # rename the vars
   FRP_filled_up_base <- FRP_filled_up_base %>% 
      mutate(rasterCellNum = as.numeric(row.names(.))) %>% 
      select(rasterCellNum, FRP_daily_sum = focal_sum, -cell) %>% 
      ungroup()
   
   # save the FRP single day df
   save(FRP_filled_up_base, 
        file = paste0(FRP_output_RData_path,'/','FRP_rCN_assigned_', k, '.RData'))
   
   
   
}


