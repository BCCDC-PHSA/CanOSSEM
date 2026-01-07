######################################################################
# Purpose: Generate modeling datasets
# Author: namanpaul
# Last modified by: namanpaul
# R Version: 4.3.0
# Date: 2025-02-26
#####################################################################

#load the packages
library(tidyverse)
library(Rfast)

### Steps needed for generating datasets to retrain CanOSSEM
#1. bind all datasets 2010-2022 (C)
#2. drop NAs in daily_mean_pm25 var
#3. shuffle all the rows
#4. calculate size of the (C), i.e. number of rows, 10% of (C) becomes prediction set(P)
#5. setdiff (P) from (C)
#6. assigning weights to higher daily_mean_pm25 values, arrange by descending
#7. keep top 60% of these as reduced set (R)
#8. take a 20% sample of these as validation set (V)
#9. setdiff (V) from (R), which becomes training set (T)
#10. save them.



# step 1:bind all ------------------------------------------------------------------

 # input data location
input_path <- 'path/MAIN_DATA_REPOSITORY/complete_cases/'

# list the files
file_list <- list.files(path = input_path, pattern = paste("*",sep="_"),
                        full.names = T, recursive = T)

# bind them all
complete_set <- bind_rows(sapply(file_list, function(x) mget(load(x)), simplify = TRUE))

# get the region values and set year_val, for easier manipulation for LORO, LOYO
load('D:/git_national_ossem/nationalOSSEM/refinement/Identify_remote_locations/CanVec_CanOSSEM_region_assigned_dedup.RData')

CanVec_CanOSSEM_updated <- CanVec_CanOSSEM_updated %>% 
  ungroup() %>% 
  select(-is_duplicate, -rCN_area) %>% 
  distinct()

#CanVec_CanOSSEM_updated <- CanVec_CanOSSEM_updated %>%
 # mutate(is_duplicate = duplicated(CanOSSEM_rCN) | duplicated(CanOSSEM_rCN, fromLast = TRUE))

#CanVec_CanOSSEM_updated <- editData::editData(CanVec_CanOSSEM_updated)



# duplicates
#duplicates <- CanVec_CanOSSEM  %>% 
#  group_by(CanOSSEM_rCN) %>% 
#  filter(n() > 1) %>% 
#  ungroup()

complete_set <- complete_set %>% 
  mutate(year_val = year(date_val)) %>% 
  left_join(.,CanVec_CanOSSEM_updated,
            by = 'CanOSSEM_rCN') %>% 
  ungroup()

# some rCNS have missing region
# na_region <- complete_set %>% 
#   filter(is.na(region)) %>% 
#   select(CanOSSEM_rCN, region, SiteID, SiteName) %>% 
#   distinct()
# 
# write.csv(na_region,
#           file = 'path/rCN_assigned/region_assignment.csv')

# save
save(complete_set,
     file = 'path/MAIN_DATA_REPOSITORY/modeling_datasets/CanOSSEM_complete_set.RData')

# step 2: drop NA ---------------------------------------------------------

#load('/home/naman/projects/def-sarahhen/naman/CanOSSEM_extension/annual_rCN_assigned/CanOSSEM_retrained_complete_set.RData')
#complete_set <- CanOSSEM_PM25_date_rCN_identifier %>% 
#  filter(!is.na(daily_mean_pm25))

#run missRanger
#Run a quick missRanger for filling up HMS_vals
# complete_set <- missRanger::missRanger(complete_set,
#                                        HMS_value ~ HMS_value + FRP_daily_sum + season , pmm.k = 3, num.trees = 100)
# 
# 
# save(complete_set,
#      file = '/home/naman/projects/def-sarahhen/naman/CanOSSEM_extension/annual_rCN_assigned/CanOSSEM_retrained_complete_set.RData',
#      compress= T)


# step 3: shuffle -----------------------------------------------------------------
load('path/MAIN_DATA_REPOSITORY/modeling_datasets_FULL_SIZE_not_feasible/CanOSSEM_complete_set.RData')

#set the seed
set.seed(131)

# shuffle cols and rows
complete_set <- complete_set %>%
  sample_frac(size = 1) %>%
  select(sample(ncol(complete_set))) %>% 
  sample_n(18e6)

# organizing the dataframe structure
complete_set <- complete_set %>% 
  # select(CanOSSEM_rCN, year_val, date_val, rCN_date_identifier, region, season,
  #        NAPS_distance, SiteID, SiteName, Source, daily_mean_pm25, 
  #        HMS_value, FRP_daily_sum,
  #        AOD_distance, mass_conc_land, opt_depth_470nm, opt_depth_550nm, opt_depth_660nm, opt_depth_2130nm,
  #        MERRA_distance, PBLH, SPEED, SPEEDMAX, QLML, TLML, min_QLML, max_QLML, max_TLML,
  #        U2M, U10M, U50M, U250, U500, U850, ULML,
  #        V2M, V10M, V50M, V250, V500, V850, VLML) %>% 
  ungroup()


# step 4: calculate nrow, P 10% -------------------------------------------
(count_rows <- nrow(complete_set))

save(complete_set,
     file = 'path/MAIN_DATA_REPOSITORY/modeling_datasets/CanOSSEM_complete_set.RData')

#prediction set 10% of complete_set
prediction_set <- sample_n(tbl = complete_set,
                           size = 0.1*count_rows,
                           replace = FALSE)

print('prediction set rows#')
(nrow(prediction_set))


year_predictions <- prediction_set %>% 
  count(year_val) 


#save

#gc()
# step 5-7: setdiff for reduced set -----------------------------------------

# create the reduced set
reduced_set <- dplyr::setdiff(complete_set, prediction_set)

rm(complete_set)

gc()

# assign pm2.5 weights
reduced_set <- reduced_set %>% 
  mutate(weights = runif(n=nrow(reduced_set), min=0, max=1),
         weight_product_pm25 = weights*daily_mean_pm25) %>%
  arrange(desc(weight_product_pm25))

print('reduced_set after weight assignment row #')
(nrow(reduced_set))

#taking the top 60% of this reduced set
reduced_set <- reduced_set %>% 
  head(n = 0.6*nrow(.))

print('reduced_set new row #')
(nrow(reduced_set))

gc()

# step 8-9-10: generate V and T ------------------------------------------------

#setting aside 20% of the data for validation set
validation_set <- sample_n(tbl = reduced_set,
                           size = 0.2*nrow(reduced_set),
                           replace = FALSE)

print('validation set rows#')
(nrow(validation_set))

year_validation_set <- validation_set %>% 
  count(year_val) 


#the remainder 80% becomes our weighted training set
training_set <- dplyr::setdiff(reduced_set, validation_set)

print('training set rows#')
(nrow(training_set))


year_training_set <- training_set %>% 
  count(year_val) 

#removing the weights
training_set <- training_set %>%
  select(-weights, -weight_product_pm25) %>% 
  ungroup()

#removing the weights
validation_set <- validation_set %>%
  select(-weights, -weight_product_pm25) %>% 
  ungroup()

gc()

#save the RData files
prediction_set <- prediction_set %>% ungroup()
training_set <- training_set %>% ungroup()
validation_set <- validation_set %>% ungroup()


save(prediction_set,
     file = 'path/MAIN_DATA_REPOSITORY/modeling_datasets/CanOSSEM_prediction_set.RData')

save(validation_set,
     file = 'path/MAIN_DATA_REPOSITORY/modeling_datasets/CanOSSEM_validation_set.RData')

save(training_set,
     file = 'path/MAIN_DATA_REPOSITORY/modeling_datasets/CanOSSEM_training_set.RData')

save(complete_set,
     file = 'path/MAIN_DATA_REPOSITORY/modeling_datasets/CanOSSEM_complete_set.RData')

gc()


#-AOD_distance, -mass_conc_land, -opt_depth_470nm, -opt_depth_550nm, -opt_depth_660nm, -opt_depth_2130nm



training_set <- training_set %>%
  ungroup() %>% 
  select(-year_val, -date_val, -CanOSSEM_rCN, -SiteID, -SiteName, -region, -min_TLML, -max_TLML,
         -min_QLML, -max_QLML, -ctry_en, -AOD_distance, -MERRA_distance, -NAPS_distance, -Source) %>% 
  ungroup()
