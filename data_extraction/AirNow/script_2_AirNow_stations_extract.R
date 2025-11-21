 #####################################################################
 # Purpose: AirNow data processing
 # Author: namanpaul
 # Last modified by: namanpaul
 # R Version: # use sessionInfo()
 # Date: # use Sys.Date()
 #####################################################################


 #Save a list of stations from the daily data
 #load the packages
 library(tidyverse)

# daily data -------------------------------------------------------------
 # after binding all of the daily data files
 
 # load the AirNow data
 load("path/2023_extracted_data/AirNow_NAPS/AirNow_daily_2023.RData")
 
 # clean up station names
 AirNow_daily <- AirNow_daily %>%
   mutate(SiteID = str_sub(SiteID, -6),
          source = 'AirNow') #%>% 
   #filter(PM25 >= 0.1) #%>% 
   #rename(AirNow_PM25 = PM25,
    #      SiteID = AQSID)

 # save 
 save(AirNow_daily,
      file = 'path/2023_extracted_data/AirNow_NAPS/AirNow_daily_2023.RData')


