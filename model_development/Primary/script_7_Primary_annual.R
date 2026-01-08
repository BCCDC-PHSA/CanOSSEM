#####################################################################
# Purpose: Generate estimates for each year (Primary)
# Author: namanpaul 
# Last modified by: namanpaul
# R Version: # use sessionInfo()
# Date: # use Sys.Date()
#####################################################################
library(doParallel)
library(tidyverse)
library(ranger)

# Parallel Setup -----------------------------------------------------------

# Read node list from environment and setup cluster
nodeslist <- unlist(strsplit(Sys.getenv("NODESLIST"), split = " "))
cl <- makeCluster(nodeslist, type = "PSOCK")
registerDoParallel(cl)


#load the ranger model


#load the yearly datasets one at a time and generate CanOSSEM estimates
years_vector <- c(2010:2011)


for(i in seq_along(years_vector)){
  
  # find file path dynamically
  
  
  file_path <- paste0('path/')
  # print
  print(file_path)
  
  # assign a generic name to the dataset
  annual_dataset <- get(load(file_path))
  
  annual_dataset <- CanOSSEM_2012_input
  
  rm(CanOSSEM_2012_input)
  
  # reduce var space before finding complete cases
  annual_dataset <- annual_dataset %>%
    select(-date_val, -CanOSSEM_rCN, -SiteID, -SiteName, -min_TLML, -max_TLML,
           -min_QLML, -max_QLML, -AOD_distance, -MERRA_distance, -NAPS_distance, -Source)
  
  
  # keep complete cases only
  annual_dataset <- annual_dataset %>%
    ungroup() %>% 
    select(daily_mean_pm25, everything()) %>%
    filter(complete.cases(.[,-1]))
  
  # print the summary to double check
  print(summary(annual_dataset))
  
  # generate predictions
  annual_predictions <- predict(Primary_ranger, data = annual_dataset)
  
  print('prediction complete')
  
  # save the predictions
  annual_dataset <- annual_dataset %>%
    select(rCN_date_identifier, daily_mean_pm25) %>%
    mutate(predicted_by = 'Primary',
           predicted_pm25 = annual_predictions$predictions)
  
  save(annual_dataset,
       file = paste0('path/MAIN_DATA_REPOSITORY/annual_predictions/Primary_annual/Primary_predictions_',years_vector[i],'.RData'))
  
  gc()
  
  
}
# Cleanup -----------------------------------------------------------------
stopCluster(cl)
