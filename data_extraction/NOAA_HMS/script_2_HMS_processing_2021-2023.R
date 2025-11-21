#####################################################################
# Purpose: Extract HMS data (2021-2023)
# Author: namanpaul
# Last modified by: namanpaul
# R Version: 4.3.2
#####################################################################


# loading the packages
library(sf)
library(terra)
library(tidyverse)
library(cleangeo)
library(foreign)


#set working directory to the source directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# load the CanOSSEM base raster
base_raster <- load('../../spatial_data/CanOSSEM_polygon_sf.RData')

# create a crs object for the source 4326, these lon-lat values work
crs_4326 <- crs("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

#crs_3347 <- crs("+proj=lcc +lat_1=49 +lat_2=77 +lat_0=63.390675 +lon_0=-91.86666666666666 +x_0=6200000 +y_0=3000000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")


# i/p paths for HMS smoke
HMS_input_path <- 'input_path/NOAA_HMS/'


# o/p path
HMS_RData_output_path <- 'output_path/NOAA_HMS/'


# define the HMS processing function
process_HMS <- function(HMS_input_path, year_val){
  
  # create the date range
  date_range <- seq(as.Date(paste(year_val,"-01-01", sep = "")),
                    as.Date(paste(year_val,"-12-31", sep = "")),
                    by=1)
  
  # set the format to %Y%m%d
  date_range <- format(date_range, "%Y%m%d")
  
  # list the folders in the directory
  
  
  
  #filter out missing days
  #date_range <- date_range1[!date_range1 %in% dates_HMS]
  
  #looping over the seq_along FRP input files
  for (i in seq_along(date_range)) {
    
    # check if the dbf file exists
    if(file.exists(paste0(HMS_input_path, year_val,'/',
                         date_range[i],'.dbf')))
    {
      
      # read in the associated dbf (years 2010-2020)
      read_DBF <- foreign::read.dbf(file = paste0(HMS_input_path, year_val,'/',
                                                  date_range[i],".dbf"))
      
      
      #checking if the dbf files are not empty
      if(nrow(read_DBF) > 0 & colSums(is.na(read_DBF))[2] == 0){
        
        # read in the HMS smoke file
        hms_data_input <- st_read(dsn = paste0(HMS_input_path, year_val,'/'),
                                  layer = paste0(date_range[i])) 
        
        
        # clean the input
        hms_data_input <- sf::st_make_valid(hms_data_input)
        
        # set the crs 4326
        hms_data_input <- st_set_crs(hms_data_input, crs_4326)
        
        # convert UTC to CST
        hms_data_input <- hms_data_input %>% 
          dplyr::mutate(end_time_UTC = as.POSIXct(strptime(End, format = "%Y%j %H%M")),
                        end_time_CST = end_time_UTC-hours(6),
                        ACQ_DATE = as_date(end_time_CST)) %>% 
          dplyr::select(-Start,-End,-end_time_UTC)
        
        
        # use st_join to see if a raster cell is being intersected by a smoke plume
        hms_joined_CanOSSEM <- st_join(hms_data_input,
                                       CanOSSEM_polygon_sf,
                                       join = st_intersects) %>% 
          rename(rasterCellNum = CanOSSEM_raster)
        
        
        # now we can keep relevant columns only
        HMS_filled_up_base <- hms_joined_CanOSSEM %>% 
          st_drop_geometry() %>% 
          select(rasterCellNum, ACQ_DATE) %>% 
          mutate(HMS_value = 1) %>% # i.e. assign smoke plume presence
          distinct() %>% 
          tidyr::complete(rasterCellNum = seq(1:nrow(CanOSSEM_polygon_sf))) %>% 
          padr::fill_by_value(value = NA) %>% 
          filter(!is.na(rasterCellNum)) %>% 
          mutate(ACQ_DATE = ymd(date_range[i])) %>% 
          distinct() 
        
        # save the RData df
        save(HMS_filled_up_base, 
             file = paste0(HMS_RData_output_path,'/','HMS_rCN_assigned_', ymd(date_range[i]), '.RData'))
        
      }
    }
    else {
      
      # else we take the CanOSSEM polygon, convert into a dataframe, and assign NAs
      HMS_filled_up_base <- CanOSSEM_polygon_sf %>%
        st_drop_geometry() %>% 
        select(rasterCellNum = CanOSSEM_raster) %>% 
        mutate(HMS_value = NA,
               ACQ_DATE = as.character(ymd(date_range[i]))) %>% 
        ungroup() %>% 
        select(rasterCellNum, ACQ_DATE, HMS_value)
      
      #save the RData df
      save(HMS_filled_up_base, 
           file = paste0(HMS_RData_output_path,'/','HMS_rCN_assigned_', ymd(date_range[i]), '.RData'))
      
      
    }
    
    
  }
  
}


# call the function, with the input path, and year_val

for(year_val in 2023){
  process_HMS(HMS_input_path,year_val)
}


# known error
# Error in scan(text = lst[[length(lst)]], quiet = TRUE) : 
#   scan() expected 'a real', got 'IllegalArgumentException:'
# Calls: .rs.sourceWithProgress ... st_sfc -> CPL_geos_make_valid -> <Anonymous> -> scan
# In addition: Warning message:
#   In CPL_read_ogr(dsn, layer, query, as.character(options), quiet,  :
#                     GDAL Message 1: Non closed ring detected. To avoid accepting it, set the OGR_GEOMETRY_ACCEPT_UNCLOSED_RING configuration option to NO
#                   Error in (function (msg)  : 
#                               IllegalArgumentException: Points of LinearRing do not form a closed linestring
#                             Calls: .rs.sourceWithProgress ... st_make_valid.sfc -> st_sfc -> CPL_geos_make_valid -> <Anonymous>
#                               Execution halted