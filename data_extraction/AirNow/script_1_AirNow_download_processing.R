#####################################################################
# Purpose: AirNow data download
# Author: namanpaul
# Last modified by: namanpaul
# R Version: # use sessionInfo()
# Date: # use Sys.Date()
#####################################################################


#load the packages
library(tidyverse)

# download and extract ----------------------------------------------------

#year val
year_val <- 2023

#create the date range (need to expand this to 2021-03-31)
date_range <- seq(as.Date(paste(year_val,"-01-01", sep = "")),
                  as.Date(paste(year_val,"-12-31", sep = "")),
                  by=1)

#change the format
date_range <- format(date_range, "%Y%m%d")

#manually created the vector
hours <- c('00','01','02','03','04','05','06','07','08','09','10','11',
           '12','13','14','15','16','17','18','19','20','21','22','23')


# Download AirNow ---------------------------------------------------------


for (i in seq_along(date_range)) {
  for(k in seq_along(hours)){
    
    
    #create the URL which should match this format: https://s3-us-west-1.amazonaws.com//files.airnowtech.org/airnow/2021/20210101/daily_data_v2.dat
    url_1 <- paste0('https://s3-us-west-1.amazonaws.com//files.airnowtech.org/airnow/',year_val,'/',date_range[i],'/','HourlyAQObs_',date_range[i],hours[k],'.dat')
    
    #download the URLs
    download.file(url = url_1,
                  destfile = paste0("D:/for_sync_data/v9_data/AirNow_NAPS/AirNow/",year_val,'/','HourlyAQObs_',date_range[i],hours[k],'.dat'),
                  mode='wb')
    
  }
}

#column names as per the file specification
#column_names <- c('valid_date','AQSID','site_name','parameter_name','reporting_units','value','averaging_period','data_source','AQI', 'AQI_category',
#                  'latitude','longitude','AQSID_with_country_code')




# Process AirNow Daily ----------------------------------------------------


AirNow_daily <- data.frame()

for (j in seq_along(date_range)){
  
  #get the date pattern
  date_pattern = date_range[j]
  
  #list of all files
  file.ls <- list.files(path='path/AirNow_NAPS/AirNow/2023/', pattern=paste0('^HourlyAQObs_',date_pattern, collapse="|"))
  
  for(k in seq_along(file.ls)){
    AirNow_hourly <- readr::read_csv(file = paste0('path/AirNow_NAPS/AirNow/2023/', file.ls[k]))
    
    AirNow_hourly <- AirNow_hourly %>% 
      filter(CountryCode == 'CA',
             Status == 'Active') %>% 
      mutate(UTC_Date_Time = mdy_hms(str_c(ValidDate," ",ValidTime)),
             CST_Date_Time = UTC_Date_Time - hours(6),
             ValidDate = date(CST_Date_Time)) %>% 
      select(AQSID, SiteName, Latitude, Longitude, ValidDate, DataSource, PM25) 
    
    #all of these can be bound into a single large dataframe ()
    AirNow_daily <- bind_rows(AirNow_daily, AirNow_hourly)
  }
 
  AirNow_daily <- AirNow_daily %>% 
    group_by(AQSID, ValidDate) %>% 
    mutate(PM25 = mean(PM25, na.rm = T)) %>% 
    distinct()
  
  AirNow_daily$ValidDate <- as.Date(AirNow_daily$ValidDate)
  
  save(AirNow_daily,
       file = paste0('path/AirNow_NAPS/AirNow/2023_extracted/AirNow_daily_',date_pattern,'.RData'))
  
  
}










# Brayden-AirNow ----------------------------------------------------------

#initial finding: the start of the valid date period is 2021-03-30

#having the initial data sorted out : bringing in the data file from Brayden
Airnow_HourlyPM25_aqmap <- read_delim("path/2021_2022/AirNow_NAPS/Airnow_HourlyPM25_aqmap.csv", 
                                      delim = ";", escape_double = FALSE, trim_ws = TRUE)


Airnow_2021_03_30_onwards <- Airnow_HourlyPM25_aqmap %>% 
  mutate(ValidTime = stringr::str_sub(date,-2), #last two chars that represent the hour of the day
         ValidTime = str_c(ValidTime,':00:00'),
         ValidDate = str_sub(date, 1,8)) %>% #YYYMMDD first 8 chars
  rename(SiteName = site,
         PM25 = pm25) #renaming the site name var to match the main AQ hourly file naming convention




#concatenate Airnow date time (time is in UTC)
Airnow_2021_03_30_onwards <- Airnow_2021_03_30_onwards %>% 
  mutate(UTC_Date_Time = ymd_hms(str_c(ValidDate," ",ValidTime)),
         CST_Date_Time = UTC_Date_Time - hours(6))


#now get the ValidDate and Time separately
Airnow_2021_03_30_onwards <- Airnow_2021_03_30_onwards %>% 
  mutate(ValidDate = date(CST_Date_Time),
         ValidTime = (stringr::str_sub(CST_Date_Time,-8)))


Airnow_2021_03_30_onwards$ValidDate <- as_date(Airnow_2021_03_30_onwards$ValidDate)


# UNBC FEM data Brayden shared ---------------------------------------------------------
FEM_sensors_AQmap <- read_csv("path/2021_2022/AirNow_NAPS/FEMsensors_aqmap.csv")

#rename vars
FEM_sensors_AQmap <- FEM_sensors_AQmap %>% 
  rename(SiteName = site,
         SiteID = siteID,
         Latitude = lat,
         Longitude = lon)

#left join the FEM lon-lat, airnow
Airnow_2021_03_30_onwards <- left_join(Airnow_2021_03_30_onwards,
                                       FEM_sensors_AQmap,
                                       by = 'SiteName')


#rearranging the df
Airnow_2021_03_30_onwards <- Airnow_2021_03_30_onwards %>% 
  select(-date) %>% 
  relocate(SiteID, SiteName, Longitude, Latitude, ValidDate, ValidTime, CST_Date_Time, UTC_Date_Time)



save(Airnow_2021_03_30_onwards,
     file = 'path/2021_2022/AirNow_NAPS/AirNow_2021-03-30_to_2022-09-29.RData',
     compress = T)



#let's get the pm and date free data from the AirNow daily data to match the lon-lat etc
AirNow_stations_data <- AirNow_daily %>% 
  select(AQSID:CountryCode) %>% 
  distinct()

Brayden_distinct_stations <- Airnow_2021_03_onwards %>% 
  select(SiteName) %>% 
  distinct()

#difference in stations
different_stations <- anti_join(Brayden_distinct_stations,
                                AirNow_stations_data,
                                by = 'SiteName')

#with the hourly files we are missing about 79 monitoring stations : need to verify



# comparing AirNow vs NAPS ------------------------------------------------



#AirNow stations
FEM_sensors_AQmap <- read_csv("path/2021_2022/AirNow_NAPS/FEMsensors_aqmap.csv")

#rename vars
FEM_sensors_AQmap <- FEM_sensors_AQmap %>% 
  rename(SiteName = site,
         SiteID = siteID,
         Latitude = lat,
         Longitude = lon)





#let's compare this with the NAPS stations
load('path/dataset_exploration/NAPS/multiple_stations/NAPS_stations_data_cleaned.RData')

#NAPS stations
NAPS_stations <- NAPS_stations_data_cleaned %>% 
  select(NAPS_stn_ID:latitude) %>% 
  distinct()


rm(NAPS_stations_data_cleaned)

NAPS_stations <- NAPS_stations %>% 
  rename(Latitude = latitude,
         Longitude = longitude)


#join FEM and NAPS
NAPS_FEM <- left_join(NAPS_stations,
                       FEM_sensors_AQmap,
                       by = c('Latitude','Longitude'))

NAPS_FEM <- NAPS_FEM %>%
  mutate(province = ifelse(city == 'Iqaluit', 'NU', province))


save(NAPS_FEM,
     file = 'path/2021_2022/AirNow_NAPS/NAPS_FEM_comparison.RData',
     compress = T)



#Testing if I join with the AirNow_daily data stations
stations_AirNow_daily <- AirNow_daily %>% 
  select(AQSID:Longitude, CountryCode) %>% 
  distinct() 








NA_NAPS_FEM <- NAPS_FEM %>% 
  filter(is.na(SiteID))
s



NAPS_stations <- NAPS_stations %>% 
  select(-province) %>% 
  mutate(origin = 'NAPS')



AirNow_stations <- AirNow_stations %>% 
  rename(NAPS_stn_ID = AQSID,
         city = site_name) %>% 
  mutate(origin = 'AirNow')


#bind rows
AirNow_vs_NAPS <- bind_rows(AirNow_stations,
                            NAPS_stations)

save(AirNow_vs_NAPS,
     file = 'path/2021_2022/AirNow_NAPS/AirNow_vs_NAPS.RData',
     compress = T)



#let's mapview this
AirNow_vs_NAPS_1 <- sf::st_as_sf(x = AirNow_vs_NAPS,
                                 coords = c('longitude','latitude'),
                                 crs = 4326)

NAPS <- AirNow_vs_NAPS_1 %>% filter(origin == 'NAPS') 

AirNow <- AirNow_vs_NAPS_1 %>% filter(origin == 'AirNow')

NAPS_map <- mapview::mapview(NAPS, color = 'red')
AirNow_map <- mapview::mapview(AirNow, color = "blue")


dual_map <- sync(NAPS_map, AirNow_map)

dual_map



#cross-check with the Hourly data file
HourlyAQObs_2021010100 <- read_csv("path/HourlyAQObs_2021010100.dat")

Hourly_data <- HourlyAQObs_2021010100 %>% 
  filter(CountryCode == 'CA') %>% 
  select(AQSID, SiteName, Longitude, Latitude) %>% 
  distinct()


Hourly_sf <- sf::st_as_sf(x = Hourly_data,
                          coords = c('Longitude','Latitude'),
                          crs = 4326)

AirNow_hourly <- mapview::mapview(Hourly_sf, color = 'green')

three_map <- sync(NAPS_map, AirNow_map, AirNow_hourly)
three_map

#checking the hourly files
Hourly_PM25_AirNow <- HourlyAQObs_2021010100 %>% 
  select(AQSID:Status, Latitude:Elevation, GMTOffset:DataSource, PM25_Measured) %>% 
  filter(CountryCode == 'CA')
  