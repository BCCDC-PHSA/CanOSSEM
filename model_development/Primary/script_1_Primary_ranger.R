######################################################################
# Purpose: Train primary CanOSSEM
# Author: namanpaul
# Last modified by: namanpaul
# R Version: 4.3.0
# Date: 2025-02-15
#####################################################################

library(doParallel)
library(ranger)
library(tidyverse)

# Parallel Setup -----------------------------------------------------------

# Read node list from environment and setup cluster
nodeslist <- unlist(strsplit(Sys.getenv("NODESLIST"), split = " "))
cl <- makeCluster(nodeslist, type = "PSOCK")
registerDoParallel(cl)

# ranger ------------------------------------
#load the dataset
load('path/MAIN_DATA_REPOSITORY/modeling_datasets/CanOSSEM_training_set.RData')


# split the dataset into train and validation -----------------------------

#randomizing the dataset
set.seed(141)

#training set, keep relevant cols
training_set <- training_set %>% 
  select(-year_val,-date_val, -CanOSSEM_rCN, -SiteID, -SiteName, -region, -min_TLML, -max_TLML,
         -min_QLML, -max_QLML, -ctry_en, -AOD_distance, -MERRA_distance, -NAPS_distance, -Source)

# ranger_modeling ---------------------------------------------------------

#multiyear model
Primary_ranger <- ranger(daily_mean_pm25 ~ .-rCN_date_identifier,
                    importance = 'permutation', num.trees = 500, mtry=5, 
                    data = training_set,
                    save.memory = T)

#printing the model
Primary_ranger

#save the model
save(Primary_ranger,
     file = 'path/MAIN_DATA_REPOSITORY/model_output/Primary_ranger/Primary_ranger.RData')

#importance of the model variables
importance <- as.data.frame(importance(Primary_ranger))

#save feature importance
save(importance,
     file = 'path/MAIN_DATA_REPOSITORY/model_output/Primary_ranger/Primary_ranger_importance.RData')

# prediction on validation set --------------------------------------------

#loading the validation set
load('path/MAIN_DATA_REPOSITORY/modeling_datasets/CanOSSEM_validation_set.RData')

validation_set <- validation_set %>% 
  select(-year_val,-date_val, -CanOSSEM_rCN, -SiteID, -SiteName, -region, -min_TLML, -max_TLML,
         -min_QLML, -max_QLML, -ctry_en, -AOD_distance, -MERRA_distance, -NAPS_distance, -Source)

#predict on validation set
Primary_ranger_validation <- predict(Primary_ranger, data = validation_set)


#binding it with validation set
#calculate residual

Primary_ranger_validation_set_with_predictions <- validation_set %>% 
  select(rCN_date_identifier, daily_mean_pm25, season) %>% 
  mutate(predicted_pm25 = Primary_ranger_validation$predictions,
    `Predicted - Observed` = predicted_pm25 - daily_mean_pm25)


save(Primary_ranger_validation_set_with_predictions,
     file = 'path/MAIN_DATA_REPOSITORY/model_output/Primary_ranger/Primary_CanOSSEM_validation_set_with_predictions.RData')


# prediction_set ----------------------------------------------------------
load('path/MAIN_DATA_REPOSITORY/modeling_datasets/CanOSSEM_prediction_set.RData')

prediction_set <- prediction_set %>% 
  select(-year_val,-date_val, -CanOSSEM_rCN, -SiteID, -SiteName, -region, -min_TLML, -max_TLML,
         -min_QLML, -max_QLML, -ctry_en, -AOD_distance, -MERRA_distance, -NAPS_distance, -Source)

# generate_predictions ----------------------------------------

#predict on prediction set
Primary_ranger_prediction <- predict(Primary_ranger, data = prediction_set)

#cal residual and bind with prediction set
Primary_ranger_prediction_set_with_predictions <- prediction_set %>% 
  select(rCN_date_identifier, daily_mean_pm25, season) %>% 
  mutate(predicted_pm25 = Primary_ranger_prediction$predictions,
         `Predicted - Observed` = predicted_pm25 - daily_mean_pm25)

save(Primary_ranger_prediction_set_with_predictions,
     file = 'path/MAIN_DATA_REPOSITORY/model_output/Primary_ranger/Primary_ranger_prediction_set_with_predictions.RData')



# prediction set MAE-MSE-RMSE ---------------------------------------------

#MAE
P_MAE <- mean(abs(Primary_ranger_prediction_set_with_predictions$`Predicted - Observed`))
print('P MAE: ')
P_MAE


#MSE
P_MSE <- mean((Primary_ranger_prediction_set_with_predictions$`Predicted - Observed`)^2)
print('P MSE: ')
P_MSE

#RMSE
P_RMSE <- sqrt(P_MSE)
print('P RMSE: ')
P_RMSE

#num of rows with error +-5
Primary_ranger_prediction_set_with_predictions %>% 
  filter(abs(`Predicted - Observed`) <= 5.0) %>% 
  nrow()


# validation set MAE-MSE-RMSE ---------------------------------------------

#MAE
V_MAE <- mean(abs(Primary_ranger_validation_set_with_predictions$`Predicted - Observed`))
print('V MAE: ')
V_MAE


#MSE
V_MSE <- mean((Primary_ranger_validation_set_with_predictions$`Predicted - Observed`)^2)
print('V MSE: ')
V_MSE

#RMSE
V_RMSE <- sqrt(V_MSE)
print('V RMSE: ')
V_RMSE

#num of rows with error +-5
Primary_ranger_validation_set_with_predictions %>% 
  filter(abs(`Predicted - Observed`) <= 5.0) %>% 
  nrow()

# Cleanup -----------------------------------------------------------------
stopCluster(cl)
