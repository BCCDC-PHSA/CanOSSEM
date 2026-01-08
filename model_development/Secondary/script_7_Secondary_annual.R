#####################################################################
# Purpose: Generate estimates for each year (Secondary)
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
load('path/MAIN_DATA_REPOSITORY/model_output/Secondary_ranger/MAIN_MODEL/Secondary_ranger.RData')

#load the yearly datasets one at a time and generate CanOSSEM estimates
years_vector <- c(2010:2011)


for(i in seq_along(years_vector)){
  
  # find file path dynamically
  file_path <- paste0('path/MAIN_DATA_REPOSITORY/annual_predictions/annual_input_datasets/CanOSSEM_',years_vector[i],'_input.RData')
  
  # print
  print(file_path)
  
  # assign a generic name to the dataset
  annual_dataset <- get(load(file_path))
  
  
  # reduce var space before finding complete cases
  annual_dataset <- annual_dataset %>%
    select(-year_val, -date_val, -CanOSSEM_rCN, -SiteID, -SiteName, -region, -min_TLML, -max_TLML,
           -min_QLML, -max_QLML, -ctry_en, -AOD_distance, -MERRA_distance, -NAPS_distance, -Source,
           -AOD_distance, -mass_conc_land, -opt_depth_470nm, -opt_depth_550nm, -opt_depth_660nm, -opt_depth_2130nm)
  
  
  
  # keep complete cases only
  annual_dataset <- annual_dataset %>%
    select(daily_mean_pm25, everything()) %>%
    filter(complete.cases(.[,-1]))
  
  # print the summary to double check
  print(summary(annual_dataset))
  
  # generate predictions
  annual_predictions <- predict(Secondary_ranger, data = annual_dataset)
  
  print('prediction complete')
  
  # save the predictions
  annual_dataset <- annual_dataset %>%
    select(rCN_date_identifier, daily_mean_pm25) %>%
    mutate(predicted_by = 'Secondary',
           predicted_pm25 = annual_predictions$predictions)
  
  save(annual_dataset,
       file = paste0('path/MAIN_DATA_REPOSITORY/annual_predictions/Secondary_annual/Secondary_predictions_',years_vector[i],'.RData'))
  
  gc()
  
  
}
# Cleanup -----------------------------------------------------------------
stopCluster(cl)
