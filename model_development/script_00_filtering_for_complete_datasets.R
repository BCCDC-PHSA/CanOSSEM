######################################################################
# Purpose: Filter for complete datasets prior creating modeling datasets
# Author: namanpaul
# Last modified by: namanpaul
# R Version: 4.3.0
# Date: 2025-02-26
#####################################################################

library(dplyr)


#this is required only for 2020,2021 and 2022
load("path/70_Expanded_CanOSSEM_Grid_Input/post_expansion/CanOSSEM_2022.RData")

#summary before any modifications
summary(CanOSSEM_2022)

#if 0, make it NA, >5 ~ 5
CanOSSEM_2022 <- CanOSSEM_2022 %>% 
  mutate(opt_depth_470nm = ifelse(opt_depth_470nm == 0,
                                  NA,
                                  opt_depth_470nm),
         opt_depth_470nm = ifelse(opt_depth_470nm >= 5,
                                  5,
                                  opt_depth_470nm),
         opt_depth_550nm = ifelse(opt_depth_550nm == 0,
                                  NA,
                                  opt_depth_550nm),
         opt_depth_660nm = ifelse(opt_depth_660nm == 0,
                                  NA,
                                  opt_depth_660nm),
         opt_depth_2110nm = ifelse(opt_depth_2110nm == 0,
                                  NA,
                                  opt_depth_2110nm),
         mass_conc_land = ifelse(mass_conc_land == 0,
                                 NA, 
                                 mass_conc_land),
         FRP_daily_sum = ifelse(is.na(FRP_daily_sum),
                                0,
                                FRP_daily_sum))

summary(CanOSSEM_2022)


#remove NAs
CanOSSEM_2022 <- CanOSSEM_2022 %>% 
  filter(!is.na(mass_conc_land),
         !is.na(PBLH),
         !is.na(daily_mean_pm25))
  

save(CanOSSEM_2022,
     file = 'path/76_CanOSSEM_retrain/annual_complete_sets_with_PM2.5_values/CanOSSEM_2022.RData',
     compress = T)
