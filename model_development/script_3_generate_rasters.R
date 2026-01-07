#####################################################################
# Purpose: Generate CanOSSEM retrained rasters
# Author: namanpaul
# Last modified by: namanpaul
# R Version: 4.3.0
# Date: 2025-03-01
#####################################################################

library(tidyverse)
library(terra)
library(sf)
library(foreach)


# **daily rasters** -------------------------------------------------------


# using base rasterdf, since tidyr complete is taking much longer than expected
CanOSSEM_raster <- rast('path/git_national_ossem/nationalOSSEM/spatial_data/CanOSSEM_raster.tif')

# use the polygon for faster rasterization
load('path/git_national_ossem/nationalOSSEM/spatial_data/CanOSSEM_polygon_sf.RData') 

CanOSSEM_polygon_sf <- CanOSSEM_polygon_sf %>% 
   rename(CanOSSEM_rCN = CanOSSEM_raster)


# load the annual estimate set
# load the combined estimates dataset
load('path/MAIN_DATA_REPOSITORY/annual_predictions/Primary_Secondary_combined/CanOSSEM_estimates_2010.RData')

# # separate date and rCNs
# CanOSSEM_estimates <- CanOSSEM_estimates %>%
#    mutate(date_val = ymd(str_sub(rCN_date_identifier, 1,10)),
#           CanOSSEM_rCN = as.numeric(str_sub(rCN_date_identifier, 12)))
#
# # quick check if there are missing rCNs for a particular day: these might need special treatment
# check_base_rCN_count <- CanOSSEM_estimates %>%
#    group_by(date_val) %>%
#    count(nrow(CanOSSEM_estimates))
#
# # check if days are missing?
# check_missing_day <- CanOSSEM_estimates %>%
#    group_by(year(date_val)) %>%
#    count(nrow(CanOSSEM_estimates))
#
# # date range
# year_val <- 2010
#
# date_range <- seq(ymd("2010-01-01"),
#                   ymd("2010-12-31"), by = 1)
#
# #set output paths
# CanOSSEM_output_path <- 'path/CanOSSEM_RASTERS_VERSION_3/daily/2010/'
#
# #foreach loop
# foreach(i = seq_along(date_range)) %do% {
#
#    print(date_range[i])
#
#    #start with a clean dataframe every iteration
#    one_day_data <- CanOSSEM_estimates %>%
#       filter(date_val == date_range[i]) %>%
#       distinct()
#
#    #select relevant vars, renaming to match the var names of the CanOSSEM_predictions_df
#    #complete the seq of cells, and fill the blank ones with NA
#
#    # join with the multipolygon sf
#    updated_sf <- left_join(CanOSSEM_polygon_sf,
#                            one_day_data,
#                            by = 'CanOSSEM_rCN') %>%
#       st_transform(.,3347)
#
#
#    CanOSSEM_daily_raster <- rasterize(updated_sf, CanOSSEM_raster, field = 'predicted_pm25')
#
#    rm(updated_sf)
#
#    #write rasters
#    writeRaster(CanOSSEM_daily_raster,
#                filename = file.path(CanOSSEM_output_path,
#                                     paste0('CanOSSEM_daily_raster_',date_range[i],'.tif')),
#                overwrite=TRUE)
#
#
# }








# visualize CanOSSEM daily raster --------------------------------------------------

# check me for the year 2010
# 2010-07-15
# 2010-08-21
# 2010-06-25
# 11-24
# 06-03

# library(tmap)
# library(leaflet)
#
# CanOSSEM_WFS <- rast('path/CanOSSEM_RASTERS_VERSION_3/daily/2010/CanOSSEM_daily_raster_2010-11-24.tif')
#
# # 18-level colour scale
# viridis_turbo <- viridis::turbo(n=18, direction = 1)
#
# # set breaks, labels
# #input_breaks  <- c(5,30,60,90,120,150,180)
# #input_labels <- c("5-30", "31-60", "61-90","91-120","121-150","151-180+")
#
# # tmap view
# a <- tm_shape(CanOSSEM_WFS)+
#   tm_raster(alpha = 0.6,
#             title = 'CanOSSEM daily mean PM2.5 (2010-11-24)',
#             breaks = c(0,10,20,30,40,50,60,70,80,90,100,200,300,400,500,600,700,800,900),
#             labels = c("0-10", "11-20", "21-30","31-40","41-50","51-60","61-70","71-80","81-90","91-100",
#                        "101-200","201-300","301-400","401-500","501-600","601-700","701-800","801-900"),
#             palette = viridis_turbo)+
#   tm_basemap('OpenStreetMap')
#
#
# map1 <- tmap_leaflet(a)
#
# map1


# monthly -----------------------------------------------------------------
load('path/MAIN_DATA_REPOSITORY/annual_predictions/Primary_Secondary_combined/CanOSSEM_estimates_2010.RData')

# separate date and rCNs
CanOSSEM_estimates <- CanOSSEM_estimates %>% 
   mutate(date_val = ymd(str_sub(rCN_date_identifier, 1,10)),
          CanOSSEM_rCN = as.numeric(str_sub(rCN_date_identifier, 12)))  %>% 
   group_by(CanOSSEM_rCN, year(date_val)) %>%
   mutate(annual_predicted_mean = mean(predicted_pm25, na.rm = T),
          annual_observed_mean = mean(daily_mean_pm25, na.rm = T)) %>%
   ungroup() %>%
   group_by(CanOSSEM_rCN, month(date_val), year(date_val)) %>%
   mutate(monthly_predicted_mean = mean(predicted_pm25, na.rm = T),
          monthly_observed_mean = mean(daily_mean_pm25, na.rm = T)) %>%
   ungroup() 


# date range
year_val <- 2010

month_range <- seq(1,12, by=1)

#set output paths
CanOSSEM_output_path <- 'path/CanOSSEM_RASTERS_VERSION_3/monthly/2010/'

#foreach loop
foreach(j = seq_along(month_range)) %do% {
   
   print(month_range[j])
   
   #start with a clean dataframe every iteration
   one_month_data <- CanOSSEM_estimates %>%
      ungroup() %>% 
      filter(`month(date_val)` == month_range[j]) %>%
      dplyr::select(CanOSSEM_rCN,`month(date_val)` ,monthly_predicted_mean) %>% 
      distinct()
   
   #select relevant vars, renaming to match the var names of the CanOSSEM_predictions_df
   #complete the seq of cells, and fill the blank ones with NA
   
   # join with the multipolygon sf
   updated_sf <- left_join(CanOSSEM_polygon_sf,
                           one_month_data,
                           by = 'CanOSSEM_rCN') %>% 
      st_transform(.,3347)
   
   
   CanOSSEM_monthly_raster <- rasterize(updated_sf, CanOSSEM_raster, field = 'monthly_predicted_mean')
   
   rm(updated_sf)
   
   #write rasters
   writeRaster(CanOSSEM_monthly_raster,
               filename = file.path(CanOSSEM_output_path,
                                    paste0('CanOSSEM_monthly_raster_',year_val,'_',month_range[j],'.tif')),
               overwrite=TRUE)
   
   
}


# annual ------------------------------------------------------------------
#set output paths
CanOSSEM_output_path <- 'path/CanOSSEM_RASTERS_VERSION_3/annual/'


#start with a clean dataframe every iteration
one_year_data <- CanOSSEM_estimates %>%
   ungroup() %>% 
   dplyr::select(CanOSSEM_rCN,`year(date_val)` ,annual_predicted_mean) %>% 
   distinct()

#select relevant vars, renaming to match the var names of the CanOSSEM_predictions_df
#complete the seq of cells, and fill the blank ones with NA

# join with the multipolygon sf
updated_sf <- left_join(CanOSSEM_polygon_sf,
                        one_year_data,
                        by = 'CanOSSEM_rCN') %>% 
   st_transform(.,3347)


CanOSSEM_annual_raster <- rasterize(updated_sf, CanOSSEM_raster, field = 'annual_predicted_mean')

rm(updated_sf)

#write rasters
writeRaster(CanOSSEM_annual_raster,
            filename = file.path(CanOSSEM_output_path,
                                 paste0('CanOSSEM_annual_raster_',year_val,'.tif')),
            overwrite=TRUE)



# **seasonal** --------------------------------------------------------------

# assign seasons
CanOSSEM_estimates <- CanOSSEM_estimates %>% 
   mutate(Season = case_when(date_val >= "2010-01-01" & date_val <= "2010-03-19" ~ "Winter",
                             date_val >= "2010-03-20" & date_val <= "2010-06-20" ~ "Spring",
                             date_val >= "2010-06-21" & date_val <= "2010-09-21" ~ "Summer",
                             date_val >= "2010-09-22" & date_val <= "2010-12-20" ~ "Autumn",
                             date_val >= "2010-12-21" & date_val <= "2010-12-31" ~ "Winter")) %>% 
   group_by(CanOSSEM_rCN, Season) %>%
   mutate(seasonal_predicted_mean = mean(predicted_pm25, na.rm = T),
          seasonal_observed_mean = mean(daily_mean_pm25, na.rm = T)) %>% 
   ungroup()


# date range
year_val <- 2010

seasons <- c('Spring','Summer','Autumn','Winter')

#set output paths
CanOSSEM_output_path <- 'path/CanOSSEM_RASTERS_VERSION_3/seasonal/2010/'

#foreach loop
foreach(k = seq_along(seasons)) %do% {
   
   print(seasons[k])
   
   #start with a clean dataframe every iteration
   one_season_data <- CanOSSEM_estimates %>%
      ungroup() %>% 
      filter(Season == seasons[k]) %>%
      dplyr::select(CanOSSEM_rCN, Season ,seasonal_predicted_mean) %>% 
      distinct()
   
   #select relevant vars, renaming to match the var names of the CanOSSEM_predictions_df
   #complete the seq of cells, and fill the blank ones with NA
   
   # join with the multipolygon sf
   updated_sf <- left_join(CanOSSEM_polygon_sf,
                           one_season_data,
                           by = 'CanOSSEM_rCN') %>% 
      st_transform(.,3347)
   
   
   CanOSSEM_seasonal_raster <- rasterize(updated_sf, CanOSSEM_raster, field = 'seasonal_predicted_mean')
   
   rm(updated_sf)
   
   #write rasters
   writeRaster(CanOSSEM_seasonal_raster,
               filename = file.path(CanOSSEM_output_path,
                                    paste0('CanOSSEM_seasonal_raster_',year_val,'_',seasons[k],'.tif')),
               overwrite=TRUE)
   
   
}
