#####################################################################
# Purpose: UNBC-NAPS extraction
# Author: namanpaul
# Last modified by: namanpaul
# R Version: # use sessionInfo()
# Date: 2024-12-23
#####################################################################

# load the packages
library(readr)
library(tidyverse)

# read the UNBC meta data
UNBC_meta_data <- read_csv("path_to_UNBC_csv")

# clean up the meta data
UNBC_meta_data <- UNBC_meta_data %>% 
  select(site_id:prov_terr) %>% 
  rename(SiteName = name,
         SiteID = site_id,
         Lon = lng,
         Lat = lat)

# read the newest UNBC file
# the starting date for this dataset is 2021-07-11, we will need the previous ones as well.
UNBC_NAPS <-  read_csv("path_to_UNBC_csv", 
                       col_types = cols(date = col_character()))

# UNBC NAPS
UNBC_NAPS <- UNBC_NAPS %>% 
  mutate(date_UTC = ymd_hms(date),
         date_CST = date_UTC - hours(6), # central time
         ValidTime = str_sub(date_CST, 12, 19), #last two chars that represent the hour of the day
         ValidDate = str_sub(date_CST, 1,10)) %>% #YYYYMMDD first 8 chars
  rename(SiteID = site_id,
         PM25 = pm25_fem) #renaming the site name var to match the main AQ hourly file naming convention


# calculate the daily mean
UNBC_NAPS_2023 <- UNBC_NAPS %>% 
  select(SiteID, ValidDate, PM25) %>% 
  group_by(SiteID, ValidDate) %>% 
  mutate(UNBC_PM25 = mean(PM25, na.rm = T)) %>% 
  select(-PM25) %>% 
  distinct() %>% 
  left_join(.,UNBC_meta_data,
            by = 'SiteID') %>% 
  mutate(source = 'UNBC')



# remove the concentrations above 1000 ug/m3
UNBC_NAPS_2023 <- UNBC_NAPS_2023 %>% 
  filter(UNBC_PM25 <= 1000 && UNBC_PM25 >= 0.1,
         prov_terr != 'United States')

save(UNBC_NAPS_2023,
     file = 'path_to_UNBC_RData')
