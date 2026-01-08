######################################################################
# Purpose: Leave one region out CV
# Author: namanpaul
# Last modified by: namanpaul
# R Version: 4.3.0
# Date: 2025-02-15
#####################################################################

library(spm)
library(doParallel)
library(ranger)
library(tidyverse)

# Parallel Setup -----------------------------------------------------------

# Read node list from environment and setup cluster
nodeslist <- unlist(strsplit(Sys.getenv("NODESLIST"), split = " "))
cl <- makeCluster(nodeslist, type = "PSOCK")
registerDoParallel(cl)

# ranger ------------------------------------
# load the dataset
load('path/MAIN_DATA_REPOSITORY/modeling_datasets/CanOSSEM_training_set.RData')

# randomizing the dataset
set.seed(141)

# training set, keep relevant cols
training_set <- training_set %>% 
  select(-rCN_date_identifier, -year_val, -SiteID, -SiteName, -min_TLML, -max_TLML,
         -min_QLML, -max_QLML, -ctry_en, -AOD_distance, -MERRA_distance, -NAPS_distance, -Source)



#load the dataset prediction
load('path/MAIN_DATA_REPOSITORY/modeling_datasets/CanOSSEM_prediction_set.RData')


prediction_set <- prediction_set %>% 
  select(-rCN_date_identifier, -SiteID, -SiteName, -min_TLML, -max_TLML,
         -min_QLML, -max_QLML, -ctry_en, -AOD_distance, -MERRA_distance, -NAPS_distance, -Source)


# leave one out region CV -------------------------------------------------

region_id <- unique(training_set$region)


loo_region <- NULL


for(i in region_id){
  ranger_LOO <- ranger(daily_mean_pm25 ~ .-date_val -CanOSSEM_rCN -region,
                       data=training_set[training_set$region!=i,],
                       save.memory = T)
  
  loo_region[[i]] <- predict(ranger_LOO,
                             data=prediction_set)
  
  ranger_LOO
  
}

save(loo_region,
     file = 'path/MAIN_DATA_REPOSITORY/model_output/Primary_ranger/LORO/Primary_ranger_LORO_list.RData')


rm(ranger_LOO)


# load the LOO REGIONCV ---------------------------------------------------


# Atlantic ----------------------------------------------------------------
predicted_pm25 = loo_region[["Atlantic"]][["predictions"]]

# filtering for Atlantic only
#Atlantic_compare <- Atlantic_compare 

Atlantic_compare <- prediction_set %>% 
  mutate(predicted_pm25 = predicted_pm25) %>% 
  mutate(`Predicted - Observed` = predicted_pm25-daily_mean_pm25)%>% 
  filter(region %in% 'Atlantic')



# MAE
MAE <- mean(abs(Atlantic_compare$`Predicted - Observed`))

print(paste0("Atlantic MAE: ", MAE))


# MSE
MSE <- mean((Atlantic_compare$`Predicted - Observed`)^2)

print(paste0("Atlantic MSE: ", MSE))

# RMSE
RMSE <- sqrt(MSE)

print(paste0("Atlantic RMSE: ", RMSE))

(nrow(Atlantic_compare))

Atlantic_compare %>% 
  filter(abs(`Predicted - Observed`) <= 5) %>% 
  nrow()


# Eastern -----------------------------------------------------------------

predicted_pm25 = loo_region[["Eastern"]][["predictions"]]

Eastern_compare <- prediction_set %>% 
  mutate(predicted_pm25 = predicted_pm25) %>% 
  mutate(`Predicted - Observed` = predicted_pm25-daily_mean_pm25)

Eastern_compare <- Eastern_compare %>% 
  filter(region %in% 'Eastern')


# MAE
MAE <- mean(abs(Eastern_compare$`Predicted - Observed`))

print(paste0("Eastern MAE: ", MAE))


#MSE
MSE <- mean((Eastern_compare$`Predicted - Observed`)^2)

print(paste0("Eastern MSE: ", MSE))

#RMSE
RMSE <- sqrt(MSE)

print(paste0("Eastern RMSE: ", RMSE))


(nrow(Eastern_compare))

Eastern_compare %>% 
  filter(abs(`Predicted - Observed`) <= 5) %>% 
  nrow()



# Northern ----------------------------------------------------------------
predicted_pm25 = loo_region[["Northern"]][["predictions"]]

Northern_compare <- prediction_set %>% 
  mutate(predicted_pm25 = predicted_pm25) %>% 
  mutate(`Predicted - Observed` = predicted_pm25-daily_mean_pm25)


Northern_compare <- Northern_compare %>% 
  filter(region %in% 'Northern')

#MAE
MAE <- mean(abs(Northern_compare$`Predicted - Observed`))

print(paste0("Northern MAE: ", MAE))


#MSE
MSE <- mean((Northern_compare$`Predicted - Observed`)^2)

print(paste0("Northern MSE: ", MSE))

#RMSE
RMSE <- sqrt(MSE)

print(paste0("Northern RMSE: ", RMSE))


(nrow(Northern_compare))

Northern_compare %>% 
  filter(abs(`Predicted - Observed`) <= 5) %>% 
  nrow()


# Prairies ----------------------------------------------------------------

predicted_pm25 = loo_region[["Prairies"]][["predictions"]]

Prairies_compare <- prediction_set %>% 
  mutate(predicted_pm25 = predicted_pm25) %>% 
  mutate(`Predicted - Observed` = predicted_pm25-daily_mean_pm25)


Prairies_compare <- Prairies_compare %>% 
  filter(region %in% 'Prairies')

#MAE
MAE <- mean(abs(Prairies_compare$`Predicted - Observed`))

print(paste0("Prairies MAE: ", MAE))


#MSE
MSE <- mean((Prairies_compare$`Predicted - Observed`)^2)

print(paste0("Prairies MSE: ", MSE))

#RMSE
RMSE <- sqrt(MSE)

print(paste0("Prairies RMSE: ", RMSE))

(nrow(Prairies_compare))

Prairies_compare %>% 
  filter(abs(`Predicted - Observed`) <= 5) %>% 
  nrow()




# West Coast --------------------------------------------------------------

predicted_pm25 = loo_region[["West Coast"]][["predictions"]]

WestCoast_compare <- prediction_set %>% 
  mutate(predicted_pm25 = predicted_pm25) %>% 
  mutate(`Predicted - Observed` = predicted_pm25-daily_mean_pm25)

WestCoast_compare <- WestCoast_compare %>% 
  filter(region %in% 'West Coast')

#MAE
MAE <- mean(abs(WestCoast_compare$`Predicted - Observed`))

print(paste0("WestCoast MAE: ", MAE))


#MSE
MSE <- mean((WestCoast_compare$`Predicted - Observed`)^2)

print(paste0("WestCoast MSE: ", MSE))

#RMSE
RMSE <- sqrt(MSE)

print(paste0("WestCoast RMSE: ", RMSE))

(nrow(WestCoast_compare))

WestCoast_compare %>% 
  filter(abs(`Predicted - Observed`) <= 5) %>% 
  nrow()




# end ---------------------------------------------------------------------

save(Atlantic_compare,
     file = 'Primary_ranger_Atlantic.RData')

save(Eastern_compare,
     file = 'Primary_ranger_Eastern.RData')


save(Northern_compare,
     file = 'Primary_ranger_Northern.RData')

save(Prairies_compare,
     file = 'Primary_ranger_Prairies.RData')

save(WestCoast_compare,
     file = 'Primary_ranger_WestCoast.RData')

# Cleanup -----------------------------------------------------------------
stopCluster(cl)