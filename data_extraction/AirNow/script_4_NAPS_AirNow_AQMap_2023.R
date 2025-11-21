 #####################################################################
 # Purpose: AirNow-ECCC-UNBC
 # Author: namanpaul
 # Last modified by:
 # R Version: # use sessionInfo()
 # Date: # use Sys.Date()
 #####################################################################
 
 # AirNow
 load('path/2023_extracted_data/AirNow_NAPS/AirNow_daily_2023.RData')
 
 AirNow_daily <- AirNow_daily %>% 
   rename(AirNow_lon = Longitude,
          AirNow_lat = Latitude)
 
 # ECCC
 load('path/2023_extracted_data/ECCC_NAPS/ECCC_NAPS_2023.RData')
 
 ECCC_NAPS_2023 <- ECCC_NAPS_2023 %>% 
   rename(ECCC_lon = Longitude,
          ECCC_lat = Latitude,
          ValidDate = date_val)
 
 # UNBC
 load('path/2023_extracted_data/UNBC_NAPS/UNBC_NAPS_2023.RData')
 
 UNBC_NAPS_2023$SiteID <- as.character(UNBC_NAPS_2023$SiteID)
 UNBC_NAPS_2023$ValidDate <- as.Date(UNBC_NAPS_2023$ValidDate)
 
 UNBC_NAPS_2023 <- UNBC_NAPS_2023 %>% 
   rename(UNBC_lon = Lon,
          UNBC_lat = Lat) %>% 
   mutate(SiteID = ifelse(str_length(SiteID) == 5, str_c('0',SiteID), SiteID))
 
 
 
 
 

 # common_siteIDs <- Reduce(intersect, list(AirNow_daily$SiteID, 
 #                                          ECCC_NAPS_2023$SiteID,
 #                                          UNBC_NAPS_2023$SiteID))
 # 
 # # finding common sites
 # combined_common <- bind_rows(
 #   AirNow_daily %>% filter(SiteID %in% common_siteIDs),
 #   ECCC_NAPS_2023 %>% filter(SiteID %in% common_siteIDs),
 #   UNBC_NAPS_2023 %>% filter(SiteID %in% common_siteIDs)
 # )

 
 # Find unique SiteIDs in each data frame 
 unique_to_AirNow <- setdiff(AirNow_daily$SiteID, union(ECCC_NAPS_2023$SiteID, UNBC_NAPS_2023$SiteID))
 unique_to_ECCC <- setdiff(ECCC_NAPS_2023$SiteID, union(AirNow_daily$SiteID, UNBC_NAPS_2023$SiteID))
 unique_to_UNBC <- setdiff(UNBC_NAPS_2023$SiteID, union(AirNow_daily$SiteID, ECCC_NAPS_2023$SiteID)) 

 
 # get the unique sites from ECCC not found in UNBC data
 ECCC_NAPS_2023 <- ECCC_NAPS_2023 %>% 
   filter(SiteID %in% unique_to_ECCC)
 
 # now bind UNBC and ECCC
 UNBC_NAPS_2023 <- UNBC_NAPS_2023 %>% 
   select(SiteID, SiteName, Lon = UNBC_lon, Lat = UNBC_lat, source, ValidDate, PM25 = UNBC_PM25)

 # ECCC
 ECCC_NAPS_2023 <- ECCC_NAPS_2023 %>% 
   select(SiteID, SiteName, Lon = ECCC_lon, Lat = ECCC_lat, source, ValidDate, PM25 = ECCC_PM25)

 
 # bind the 2023 data
 UNBC_ECCC_NAPS_2023 <- bind_rows(UNBC_NAPS_2023,
                                  ECCC_NAPS_2023)
 
 # match variable names to the NAPS 2010-2022 dataframe
 load("D:/git_national_ossem/nationalOSSEM/dataset_exploration/NAPS/Canada_NAPS_PM25_2010_2022.RData")

 
 # update the 2023 dataframe
 UNBC_ECCC_NAPS_2023 <- UNBC_ECCC_NAPS_2023 %>% 
   mutate(SiteID = str_c('ID_',SiteID)) %>% 
   rename(Longitude = Lon,
          Latitude = Lat,
          Source = source,
          daily_mean_pm25 = PM25)

 
 # bind it to the 2010-2022 dataframe
 Canada_NAPS_PM25_2010_2023 <- bind_rows(Canada_NAPS_PM25_2010_2022,
                                         UNBC_ECCC_NAPS_2023) %>% 
   mutate(SiteID = ifelse(str_length(SiteID) == 6, str_c('ID_',SiteID), SiteID))
 
 # get rid of 0 values
 Canada_NAPS_PM25_2010_2023 <- Canada_NAPS_PM25_2010_2023 %>% 
   filter(daily_mean_pm25 >= 0.1)

 save(Canada_NAPS_PM25_2010_2023,
      file = 'path/Canada_NAPS_PM25_2010_2023.RData') 

 
 # unique stations, visualize
 unique_stations <- Canada_NAPS_PM25_2010_2023 %>% 
   ungroup() %>% 
   select(SiteID, SiteName, Source, Longitude, Latitude) %>% 
   distinct()

 
 sf_unique <- sf::st_as_sf(unique_stations,
                           coords = c('Longitude','Latitude'),
                           crs = 4326) 

 mapview::mapview(sf_unique) 
 