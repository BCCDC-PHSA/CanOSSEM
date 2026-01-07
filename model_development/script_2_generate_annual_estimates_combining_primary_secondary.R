 #####################################################################
 # Purpose: Generate annual CanOSSEM estimates (primary and secondary)
 # Author: namanpaul
 # Last modified by: namanpaul
 # R Version: 4.3.0
 # Date: 2025-02-26
 #####################################################################
 
 # load the libraries
 library(tidyverse)
 
 # basically, what we want is that if a Primary CanOSSEM estimate is available, 
 # for the date-CanOSSEM rCN combination, use Primary CanOSSEM estimate else
 # use the Secondary CanOSSEM estimate
 
 # load the annual estimate dataset (Primary)
 load('path/MAIN_DATA_REPOSITORY/annual_predictions/Primary_annual/Primary_predictions_2017.RData')
 
 Primary_CanOSSEM_estimates <- annual_dataset
 
 # load the annual estimate dataset (Secondary)
 load('path/MAIN_DATA_REPOSITORY/annual_predictions/Secondary_annual/Secondary_predictions_2017.RData')
 
 Secondary_CanOSSEM_estimates <- annual_dataset
 
 rm(annual_dataset)

 
 CanOSSEM_estimates <- full_join(Primary_CanOSSEM_estimates,
                                 Secondary_CanOSSEM_estimates,
                                 by = c('rCN_date_identifier')) %>% 
   mutate(across(ends_with(".x"), ~ coalesce(.x, get(sub(".x$", ".y", cur_column()))))) %>%
   rename_with(~ sub(".x$", "", .), ends_with(".x")) %>%
   select(-ends_with(".y"))  # Remove unnecessary columns from df2

 
 save(CanOSSEM_estimates,
      file = 'path/MAIN_DATA_REPOSITORY/annual_predictions/Primary_Secondary_combined/CanOSSEM_estimates_2017.RData')
 
 

 
 
 
 
 
 
 
 
 
# check for duplicates, print ranges, calculate metrics -------------------
 
 load('path/MAIN_DATA_REPOSITORY/annual_predictions/Primary_Secondary_combined/CanOSSEM_estimates_2013.RData')
 
 CanOSSEM_estimates <- CanOSSEM_estimates %>%
    group_by(rCN_date_identifier) %>%
    filter(!n() > 1) %>%
    ungroup()
 
 save(CanOSSEM_estimates,
      file = 'path/MAIN_DATA_REPOSITORY/annual_predictions/Primary_Secondary_combined/CanOSSEM_estimates_2013.RData')
 
 
 # check for duplicates
 # df_counts <- CanOSSEM_estimates %>%
 #    group_by(rCN_date_identifier) %>%
 #    summarise(count = n(), .groups = "drop") %>%
 #    filter(count > 1)
 
 # overall range observed
 CanOSSEM_estimates %>%
    summarise(range(daily_mean_pm25, na.rm = T))
 
 # overall range predicted
 CanOSSEM_estimates %>%
    summarise(range(predicted_pm25, na.rm = T))
 
 # Overall correlation
 CanOSSEM_estimates %>%
    summarise(cor(daily_mean_pm25, predicted_pm25, use="complete.obs"))
 
 
 
 
 # primary secondary range observed
 CanOSSEM_estimates %>%
    group_by(predicted_by) %>% 
    summarise(range(daily_mean_pm25, na.rm = T))
 
 
 # Primary/Secondary range predicted
 CanOSSEM_estimates %>%
    group_by(predicted_by) %>% 
    summarise(range(predicted_pm25, na.rm = T))
 
 
 # Primary/Secondary correlation
 CanOSSEM_estimates %>%
    group_by(predicted_by) %>% 
    summarise(cor(daily_mean_pm25, predicted_pm25, use="complete.obs"))
 