######################################################################
# Purpose: Leave one year out CV (2)
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
#load the dataset
load('path/MAIN_DATA_REPOSITORY/modeling_datasets/CanOSSEM_training_set.RData')

# split the dataset into train and validation -----------------------------

#separate out the rCN and date
training_set <-  training_set %>% 
  mutate(year_val = as.character(year_val)) %>% 
  select(-rCN_date_identifier, -SiteID, -SiteName, -min_TLML, -max_TLML,
         -min_QLML, -max_QLML, -ctry_en, -AOD_distance, -MERRA_distance, -NAPS_distance, -Source)



# load prediction data ----------------------------------------------------
load('path/MAIN_DATA_REPOSITORY/modeling_datasets/CanOSSEM_prediction_set.RData')

prediction_set <- prediction_set %>% 
  mutate(year_val = as.character(year_val)) %>% 
  select(-rCN_date_identifier, -SiteID, -SiteName, -min_TLML, -max_TLML,
         -min_QLML, -max_QLML, -ctry_en, -AOD_distance, -MERRA_distance, -NAPS_distance, -Source)


# leave one out region CV -------------------------------------------------
year_id <- as.character(c("2017","2018","2019","2020","2021","2022","2023"))

loo_year <- NULL

print('Training models now')

for(i in year_id){
  
  print(i)
  ranger_LOO <- ranger(daily_mean_pm25 ~ .-date_val -CanOSSEM_rCN -year_val,
                       data=training_set[training_set$year_val!= i,],
                       save.memory = T)
  
  loo_year[[i]] <- predict(ranger_LOO,
                           data=prediction_set)
  
  ranger_LOO
  
}

save(loo_year,
     file = 'Primary_ranger_LOYO_CV_2017-23.RData')



# 2017 ----------------------------------------------------------------
compare_2017 <- prediction_set %>%
  mutate(predicted_pm25 = loo_year[["2017"]][["predictions"]],
         `Predicted - Observed` = predicted_pm25-daily_mean_pm25) %>% 
  select(date_val, year_val, season,  daily_mean_pm25, predicted_pm25, `Predicted - Observed`) %>% 
  filter(year_val %in% '2017')


#MAE
MAE <- mean(abs(compare_2017$`Predicted - Observed`))

print(paste0("2017 MAE: ", MAE))


#MSE
MSE <- mean((compare_2017$`Predicted - Observed`)^2)

print(paste0("2017 MSE: ", MSE))

#RMSE
RMSE <- sqrt(MSE)

print(paste0("2017 RMSE: ", RMSE))

compare_2017 %>% 
  filter(abs(`Predicted - Observed`) <= 5) %>% 
  nrow()/nrow(compare_2017)


print(paste0("2017 nrow: ", nrow(compare_2017)))

rm(compare_2017)

# 2018 --------------------------------------------------------------------

compare_2018 <- prediction_set %>%
  mutate(predicted_pm25 = loo_year[["2018"]][["predictions"]],
         `Predicted - Observed` = predicted_pm25-daily_mean_pm25) %>% 
  select(date_val, year_val, season, daily_mean_pm25, predicted_pm25, `Predicted - Observed`) %>% 
  filter(year_val %in% '2018')


#MAE
MAE <- mean(abs(compare_2018$`Predicted - Observed`))

print(paste0("2018 MAE: ", MAE))


#MSE
MSE <- mean((compare_2018$`Predicted - Observed`)^2)

print(paste0("2018 MSE: ", MSE))

#RMSE
RMSE <- sqrt(MSE)

print(paste0("2018 RMSE: ", RMSE))

compare_2018 %>% 
  filter(abs(`Predicted - Observed`) <= 5) %>% 
  nrow()/nrow(compare_2018)

print(paste0("2018 nrow: ", nrow(compare_2018)))

rm(compare_2018)

# 2019 --------------------------------------------------------------------

compare_2019 <- prediction_set %>%
  mutate(predicted_pm25 = loo_year[["2019"]][["predictions"]],
         `Predicted - Observed` = predicted_pm25-daily_mean_pm25) %>% 
  select(date_val, year_val, season, daily_mean_pm25, predicted_pm25, `Predicted - Observed`) %>% 
  filter(year_val %in% '2019')


#MAE
MAE <- mean(abs(compare_2019$`Predicted - Observed`))

print(paste0("2019 MAE: ", MAE))


#MSE
MSE <- mean((compare_2019$`Predicted - Observed`)^2)

print(paste0("2019 MSE: ", MSE))

#RMSE
RMSE <- sqrt(MSE)

print(paste0("2019 RMSE: ", RMSE))

compare_2019 %>% 
  filter(abs(`Predicted - Observed`) <= 5) %>% 
  nrow()/nrow(compare_2019)

print(paste0("2019 nrow: ", nrow(compare_2019)))

rm(compare_2019)

# 2020 --------------------------------------------------------------------

compare_2020 <- prediction_set %>%
  mutate(predicted_pm25 = loo_year[["2020"]][["predictions"]],
         `Predicted - Observed` = predicted_pm25-daily_mean_pm25) %>% 
  select(date_val, year_val, season, daily_mean_pm25, predicted_pm25, `Predicted - Observed`) %>% 
  filter(year_val %in% '2020')


#MAE
MAE <- mean(abs(compare_2020$`Predicted - Observed`))

print(paste0("2020 MAE: ", MAE))


#MSE
MSE <- mean((compare_2020$`Predicted - Observed`)^2)

print(paste0("2020 MSE: ", MSE))

#RMSE
RMSE <- sqrt(MSE)

print(paste0("2020 RMSE: ", RMSE))

compare_2020 %>% 
  filter(abs(`Predicted - Observed`) <= 5) %>% 
  nrow()/nrow(compare_2020)


print(paste0("2020 nrow: ", nrow(compare_2020)))

rm(compare_2020)

# 2021 --------------------------------------------------------------------

compare_2021 <- prediction_set %>%
  mutate(predicted_pm25 = loo_year[["2021"]][["predictions"]],
         `Predicted - Observed` = predicted_pm25-daily_mean_pm25) %>% 
  select(date_val, year_val, season, daily_mean_pm25, predicted_pm25, `Predicted - Observed`) %>% 
  filter(year_val %in% '2021')



#MAE
MAE <- mean(abs(compare_2021$`Predicted - Observed`))

print(paste0("2021 MAE: ", MAE))


#MSE
MSE <- mean((compare_2021$`Predicted - Observed`)^2)

print(paste0("2021 MSE: ", MSE))

#RMSE
RMSE <- sqrt(MSE)


print(paste0("2021 RMSE: ", RMSE))

compare_2021 %>% 
  filter(abs(`Predicted - Observed`) <= 5) %>% 
  nrow()/nrow(compare_2021)

print(paste0("2021 nrow: ", nrow(compare_2021)))


rm(compare_2021)




# 2022 --------------------------------------------------------------------

compare_2022 <- prediction_set %>%
  mutate(predicted_pm25 = loo_year[["2022"]][["predictions"]],
         `Predicted - Observed` = predicted_pm25-daily_mean_pm25) %>% 
  select(date_val, year_val, season, daily_mean_pm25, predicted_pm25, `Predicted - Observed`) %>% 
  filter(year_val %in% '2022')



#MAE
MAE <- mean(abs(compare_2022$`Predicted - Observed`))

print(paste0("2022 MAE: ", MAE))


#MSE
MSE <- mean((compare_2022$`Predicted - Observed`)^2)

print(paste0("2022 MSE: ", MSE))

#RMSE
RMSE <- sqrt(MSE)


print(paste0("2022 RMSE: ", RMSE))

compare_2022 %>% 
  filter(abs(`Predicted - Observed`) <= 5) %>% 
  nrow()/nrow(compare_2022)

print(paste0("2022 nrow: ", nrow(compare_2021)))


rm(compare_2022)


# 2023 --------------------------------------------------------------------

compare_2023 <- prediction_set %>%
  mutate(predicted_pm25 = loo_year[["2023"]][["predictions"]],
         `Predicted - Observed` = predicted_pm25-daily_mean_pm25) %>% 
  select(date_val, year_val, season, daily_mean_pm25, predicted_pm25, `Predicted - Observed`) %>% 
  filter(year_val %in% '2023')



#MAE
MAE <- mean(abs(compare_2023$`Predicted - Observed`))

print(paste0("2023 MAE: ", MAE))


#MSE
MSE <- mean((compare_2023$`Predicted - Observed`)^2)

print(paste0("2023 MSE: ", MSE))

#RMSE
RMSE <- sqrt(MSE)


print(paste0("2023 RMSE: ", RMSE))

compare_2023 %>% 
  filter(abs(`Predicted - Observed`) <= 5) %>% 
  nrow()/nrow(compare_2023)

print(paste0("2023 nrow: ", nrow(compare_2021)))


rm(compare_2023)


# end ---------------------------------------------------------------------
# Cleanup -----------------------------------------------------------------
stopCluster(cl)

