#####################################################################
# Purpose: Generate CanOSSEM estimate dataset for the regions
# Author: namanpaul
# Last modified by: namanpaul
# R Version: 4.3.0
# Date: 2025-03-02
#####################################################################

# load the packages
library(tidyverse)

year_val <- c(2023)

for(k in seq_along(year_val))
{
  load(paste0('path/MAIN_DATA_REPOSITORY/annual_predictions/Primary_Secondary_combined/CanOSSEM_estimates_',
              year_val[k],'.RData'))
  
  # split rCN_date_identifier
  CanOSSEM_estimates <- CanOSSEM_estimates %>% 
    mutate(date_val = ymd(str_sub(rCN_date_identifier, 1,10)),
           CanOSSEM_rCN = as.numeric(str_sub(rCN_date_identifier, 12)))
  
  # load the CanVec
  load("path/CanVec_CanOSSEM_region_assigned_dedup.RData")
  
  CanVec_CanOSSEM_updated <- CanVec_CanOSSEM_updated %>% 
    distinct()
  
  region_id <- unique(CanVec_CanOSSEM_updated$region)
  
  
  
  
  # iterate over each region
  for(i in seq_along(region_id)){
    
    region_data <- CanVec_CanOSSEM_updated %>% 
      filter(region %in% region_id[i]) %>% 
      select(CanOSSEM_rCN, region)
    
    CanOSSEM_estimates_region_ <- CanOSSEM_estimates %>% 
      filter(CanOSSEM_rCN %in% region_data$CanOSSEM_rCN)
    
    assign(paste("CanOSSEM_estimates_region_", region_id[i], sep=""), CanOSSEM_estimates_region_)
    
    rm(CanOSSEM_estimates_region_)
    
    list_df <- lapply(ls(pattern="CanOSSEM_estimates_region_+"), function(x) get(x))
    
    save(list_df,
         file = paste0('path/CanOSSEM_RASTERS_VERSION_3/daily_rasters_by_region_other_geographies/region/',year_val[k],'/CanOSSEM_estimates_region_',region_id[i],'_',year_val[k],'.RData'))
    
    
    rm(list_df)
  }
  rm(list=ls(pattern="CanOSSEM_estimates+"))
}