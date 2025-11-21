#####################################################################
# Purpose: Bind ECCC-NAPS, AirNow, AQMap
# Author: namanpaul
# Last modified by: namanpaul
# R Version: # use sessionInfo()
# Date: # use Sys.Date()
#####################################################################


#load packages

library(dplyr)
library(stringr)
library(lubridate)


# UNBC --------------------------------------------------------------------
load('path/UNBC_daily_PM25_data_20210330_20221231.RData')

UNBC_PM25_data <- UNBC_PM25_daily_data %>% 
  mutate(Source = 'UNBC') %>% 
  select(SiteID, SiteName, Longitude, Latitude, ValidDate, daily_mean_pm25, Source)

rm(UNBC_PM25_daily_data)


# AirNow ------------------------------------------------------------------

#2020
load('path/AirNow_daily_2020.RData')

AirNow_2020 <- AirNow_daily %>% 
  rename(SiteID = AQSID,
         daily_mean_pm25 = PM25) %>% 
  mutate(Source = 'AirNow')

#2021
load('path/AirNow_daily_2021.RData')

AirNow_2021 <- AirNow_daily %>% 
  rename(SiteID = AQSID,
         daily_mean_pm25 = PM25) %>% 
  mutate(Source = 'AirNow')


#2022
load('path/AirNow_daily_2022.RData')

AirNow_2022 <- AirNow_daily_2022 %>% 
  rename(SiteID = AQSID,
         daily_mean_pm25 = PM25) %>% 
  mutate(Source = 'AirNow')


#AirNow bound
AirNow_PM25_data <- bind_rows(AirNow_2020,
                              AirNow_2021,
                              AirNow_2022) %>% 
  select(SiteID, SiteName, Longitude, Latitude, ValidDate, daily_mean_pm25, Source) %>% 
  mutate(daily_mean_pm25 = ifelse(is.nan(daily_mean_pm25) ==T, NA,daily_mean_pm25))


AirNow_PM25_data <- AirNow_PM25_data %>% 
  na.omit()


rm(AirNow_2020)
rm(AirNow_2021)
rm(AirNow_2022)
rm(AirNow_daily_2022)
rm(AirNow_daily)



# NAPS --------------------------------------------------------------------
load('path/NAPS_daily_data_20200101_20221231.RData')

NAPS_2020_2022 <- NAPS_2020_2022 %>% 
  rename(ValidDate = date_val) %>% 
  mutate(Source = 'NAPS') %>% 
  select(SiteID, SiteName, Longitude, Latitude, ValidDate, daily_mean_pm25, Source)







# PM25_2020_2022 ----------------------------------------------------------

PM25_2020_2022 <- bind_rows(AirNow_PM25_data,
                            NAPS_2020_2022,
                            UNBC_PM25_data)

#make sure we retain only the last 6 characters for the SiteID
PM25_2020_2022 <- PM25_2020_2022 %>% 
  mutate(SiteID = stringr::str_sub(SiteID,-6))






#find the duplicate/quad 

#filter for more than two
stations_with_same_ID <- PM25_2020_2022 %>% 
  ungroup() %>% 
  select(-daily_mean_pm25) %>% 
  distinct() %>% 
  #group_by(Longitude, Latitude) %>% 
  count(SiteID, ValidDate) %>% 
  filter(n >=2)

unique_stations <- unique(stations_with_same_ID$SiteID)

#get the pm25 data for just those stations
multiple_stations_data <- PM25_2020_2022 %>% 
  filter(SiteID %in% unique_stations)


#Station source count
station_source_count <- multiple_stations_data %>% 
  group_by(SiteID, ValidDate) %>% 
  summarise(NAPS = sum(Source == 'NAPS'),
            AirNow = sum(Source == 'AirNow'),
            UNBC = sum(Source == 'UNBC')) %>% 
  mutate(max_vals = pmax(AirNow, NAPS, UNBC),
         Source = case_when(max_vals == AirNow ~ "AirNow",
                            max_vals == NAPS ~ "NAPS",
                            max_vals == UNBC ~ "UNBC")) 


station_source_count <- station_source_count %>% 
  select(SiteID, ValidDate, Source) %>% 
  distinct()




#get the pm25 data for just those stations
filtered_data_multisource <- inner_join(multiple_stations_data,
                                        station_source_count,
                                        keep= F)



#now remove the SiteIDs from the PM25_2020_2022
remaining_PM25_2020_2022 <- anti_join(PM25_2020_2022,
                                      multiple_stations_data)


  
#bind it with the multiple stations df
PM25_2020_2022 <- bind_rows(remaining_PM25_2020_2022,
                            filtered_data_multisource)



save(PM25_2020_2022,
     file = 'path/PM25_2020_2022_AirNow_NAPS_UNBC.RData',
     compress = T)

save(PM25_2020_2022,
     file = 'path/extracted_data/PM25_2020_2022_AirNow_NAPS_UNBC.RData',
     compress = T)


