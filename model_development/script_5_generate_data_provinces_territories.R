#####################################################################
# Purpose: Generate CanOSSEM estimate dataset for the regions
# Author: namanpaul
# Last modified by: namanpaul
# R Version: 4.3.0
# Date: 2025-03-03
#####################################################################

# load the packages
library(tidyverse)

load("path/CanOSSEM_populated_rCN_near_a_postalcode.RData")


# restricting it for BC right now
provinces_territories <- c('QC')



year_val <- c(2010:2023)

for(k in seq_along(year_val)){
  load(paste0('path/MAIN_DATA_REPOSITORY/annual_predictions/Primary_Secondary_combined/CanOSSEM_estimates_',
              year_val[k],'.RData'))
  
  
  # split rCN_date_identifier
  CanOSSEM_estimates <- CanOSSEM_estimates %>% 
    mutate(date_val = ymd(str_sub(rCN_date_identifier, 1,10)),
           CanOSSEM_rCN = as.numeric(str_sub(rCN_date_identifier, 12)))
  
  
  
  
  # iterate over each region
  for(i in seq_along(provinces_territories)){
    
    provinces_territories_data <- CanOSSEM_nearest_rCN_all_postal_codes %>% 
      filter(PROVINCE %in% provinces_territories[i]) %>% 
      select(CanOSSEM_rCN, PROVINCE, POSTAL_CODE:COMM_NAME)
    
    CanOSSEM_estimates_ <- CanOSSEM_estimates %>% 
      filter(CanOSSEM_rCN %in% provinces_territories_data$CanOSSEM_rCN) %>% 
      left_join(.,provinces_territories_data,
                by = 'CanOSSEM_rCN') %>% 
      select(-rCN_date_identifier)
    
    assign(paste("CanOSSEM_estimates_", provinces_territories[i], sep=""), CanOSSEM_estimates_)
    
    rm(CanOSSEM_estimates_)
    
    list_df <- lapply(ls(pattern="CanOSSEM_estimates_+"), function(x) get(x))
    
    save(list_df,
         file = paste0('path/CanOSSEM_RASTERS_VERSION_3/daily_rasters_by_region_other_geographies/provinces_territories/',year_val[k],'/CanOSSEM_estimates_',provinces_territories[i],'_',year_val[k],'.RData'))
    
    rm(list_df)
    
  }
  rm(list=ls(pattern="CanOSSEM_estimates+"))
}
