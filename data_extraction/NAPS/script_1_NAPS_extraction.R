 #####################################################################
 # Purpose: ECCC-NAPS data extraction 2023
 # Author: namanpaul
 # Last modified by: 2024-12-16
 # R Version: # use sessionInfo()
 # Date: # use Sys.Date()
 #####################################################################

#load packages
library(tidyverse)
library(sf)
library(tmap)

# NAPS 2023 stations ---------------------------------------------------

#read in the NAPS csv data (2023)
NAPS_2023 <- read_csv("path_to/NAPS/2023_observations.csv", 
                      col_types = cols(date = col_datetime(format = "%Y-%m-%d %H:%M:%S")))

#distinct stations
NAPS_2023 <- NAPS_2023 %>% 
  ungroup() %>% 
  mutate(id = stringr::str_sub(id,-6)) %>% 
  rename(SiteID = id,
         SiteName = name,
         Longitude = lon,
         Latitude = lat) %>% 
  mutate(SiteID = str_pad(SiteID, width = 6, side = 'left', pad = '0'))



#ymd_hms
NAPS_2023$UTC_date_time <- as_datetime(NAPS_2023$date)

#separate out date_val
ECCC_NAPS_2023 <- NAPS_2023 %>% 
  mutate(CST_date_time = UTC_date_time-hours(6),
         date_val = date(CST_date_time)) %>% 
  group_by(date_val, SiteID) %>% 
  mutate(ECCC_PM25 = mean(PM2.5, na.rm = T)) %>% 
  select(-date:-CST_date_time) %>% 
  distinct() %>% 
  mutate(source = 'ECCC')


# save the df
save(ECCC_NAPS_2023,
     file = 'path/ECCC_NAPS_2023.RData')

