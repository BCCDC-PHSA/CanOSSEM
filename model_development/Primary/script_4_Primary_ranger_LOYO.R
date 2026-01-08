######################################################################
# Purpose: Leave one year out CV (1)
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
year_id <- as.character(c("2010","2011","2012","2013","2014","2015","2016"))

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
     file = 'Primary_ranger_LOYO_CV_2010-16.RData')



# 2010 ----------------------------------------------------------------
compare_2010 <- prediction_set %>%
  mutate(predicted_pm25 = loo_year[["2010"]][["predictions"]],
         `Predicted - Observed` = predicted_pm25-daily_mean_pm25) %>% 
  select(date_val, year_val, season,  daily_mean_pm25, predicted_pm25, `Predicted - Observed`) %>% 
  filter(year_val %in% '2010')


#MAE
MAE <- mean(abs(compare_2010$`Predicted - Observed`))

print(paste0("2010 MAE: ", MAE))


#MSE
MSE <- mean((compare_2010$`Predicted - Observed`)^2)

print(paste0("2010 MSE: ", MSE))

#RMSE
RMSE <- sqrt(MSE)

print(paste0("2010 RMSE: ", RMSE))

compare_2010 %>% 
  filter(abs(`Predicted - Observed`) <= 5) %>% 
  nrow()/nrow(compare_2010)


print(paste0("2010 nrow: ", nrow(compare_2010)))

rm(compare_2010)

# 2011 --------------------------------------------------------------------

compare_2011 <- prediction_set %>%
  mutate(predicted_pm25 = loo_year[["2011"]][["predictions"]],
         `Predicted - Observed` = predicted_pm25-daily_mean_pm25) %>% 
  select(date_val, year_val, season, daily_mean_pm25, predicted_pm25, `Predicted - Observed`) %>% 
  filter(year_val %in% '2011')


#MAE
MAE <- mean(abs(compare_2011$`Predicted - Observed`))

print(paste0("2011 MAE: ", MAE))


#MSE
MSE <- mean((compare_2011$`Predicted - Observed`)^2)

print(paste0("2011 MSE: ", MSE))

#RMSE
RMSE <- sqrt(MSE)

print(paste0("2011 RMSE: ", RMSE))

compare_2011 %>% 
  filter(abs(`Predicted - Observed`) <= 5) %>% 
  nrow()/nrow(compare_2011)

print(paste0("2011 nrow: ", nrow(compare_2011)))

rm(compare_2011)

# 2012 --------------------------------------------------------------------

compare_2012 <- prediction_set %>%
  mutate(predicted_pm25 = loo_year[["2012"]][["predictions"]],
         `Predicted - Observed` = predicted_pm25-daily_mean_pm25) %>% 
  select(date_val, year_val, season, daily_mean_pm25, predicted_pm25, `Predicted - Observed`) %>% 
  filter(year_val %in% '2012')


#MAE
MAE <- mean(abs(compare_2012$`Predicted - Observed`))

print(paste0("2012 MAE: ", MAE))


#MSE
MSE <- mean((compare_2012$`Predicted - Observed`)^2)

print(paste0("2012 MSE: ", MSE))

#RMSE
RMSE <- sqrt(MSE)

print(paste0("2012 RMSE: ", RMSE))

compare_2012 %>% 
  filter(abs(`Predicted - Observed`) <= 5) %>% 
  nrow()/nrow(compare_2012)

print(paste0("2012 nrow: ", nrow(compare_2012)))

rm(compare_2012)

# 2013 --------------------------------------------------------------------

compare_2013 <- prediction_set %>%
  mutate(predicted_pm25 = loo_year[["2013"]][["predictions"]],
         `Predicted - Observed` = predicted_pm25-daily_mean_pm25) %>% 
  select(date_val, year_val, season, daily_mean_pm25, predicted_pm25, `Predicted - Observed`) %>% 
  filter(year_val %in% '2013')


#MAE
MAE <- mean(abs(compare_2013$`Predicted - Observed`))

print(paste0("2013 MAE: ", MAE))


#MSE
MSE <- mean((compare_2013$`Predicted - Observed`)^2)

print(paste0("2013 MSE: ", MSE))

#RMSE
RMSE <- sqrt(MSE)

print(paste0("2013 RMSE: ", RMSE))

compare_2013 %>% 
  filter(abs(`Predicted - Observed`) <= 5) %>% 
  nrow()/nrow(compare_2013)


print(paste0("2013 nrow: ", nrow(compare_2013)))

rm(compare_2013)

# 2014 --------------------------------------------------------------------

compare_2014 <- prediction_set %>%
  mutate(predicted_pm25 = loo_year[["2014"]][["predictions"]],
         `Predicted - Observed` = predicted_pm25-daily_mean_pm25) %>% 
  select(date_val, year_val, season, daily_mean_pm25, predicted_pm25, `Predicted - Observed`) %>% 
  filter(year_val %in% '2014')



#MAE
MAE <- mean(abs(compare_2014$`Predicted - Observed`))

print(paste0("2014 MAE: ", MAE))


#MSE
MSE <- mean((compare_2014$`Predicted - Observed`)^2)

print(paste0("2014 MSE: ", MSE))

#RMSE
RMSE <- sqrt(MSE)


print(paste0("2014 RMSE: ", RMSE))

compare_2014 %>% 
  filter(abs(`Predicted - Observed`) <= 5) %>% 
  nrow()/nrow(compare_2014)

print(paste0("2014 nrow: ", nrow(compare_2014)))


rm(compare_2014)




# 2015 --------------------------------------------------------------------

compare_2015 <- prediction_set %>%
  mutate(predicted_pm25 = loo_year[["2015"]][["predictions"]],
         `Predicted - Observed` = predicted_pm25-daily_mean_pm25) %>% 
  select(date_val, year_val, season, daily_mean_pm25, predicted_pm25, `Predicted - Observed`) %>% 
  filter(year_val %in% '2015')



#MAE
MAE <- mean(abs(compare_2015$`Predicted - Observed`))

print(paste0("2015 MAE: ", MAE))


#MSE
MSE <- mean((compare_2015$`Predicted - Observed`)^2)

print(paste0("2015 MSE: ", MSE))

#RMSE
RMSE <- sqrt(MSE)


print(paste0("2015 RMSE: ", RMSE))

compare_2015 %>% 
  filter(abs(`Predicted - Observed`) <= 5) %>% 
  nrow()/nrow(compare_2015)

print(paste0("2015 nrow: ", nrow(compare_2015)))


rm(compare_2015)


# 2016 --------------------------------------------------------------------

compare_2016 <- prediction_set %>%
  mutate(predicted_pm25 = loo_year[["2016"]][["predictions"]],
         `Predicted - Observed` = predicted_pm25-daily_mean_pm25) %>% 
  select(date_val, year_val, season, daily_mean_pm25, predicted_pm25, `Predicted - Observed`) %>% 
  filter(year_val %in% '2016')



#MAE
MAE <- mean(abs(compare_2016$`Predicted - Observed`))

print(paste0("2016 MAE: ", MAE))


#MSE
MSE <- mean((compare_2016$`Predicted - Observed`)^2)

print(paste0("2016 MSE: ", MSE))

#RMSE
RMSE <- sqrt(MSE)


print(paste0("2016 RMSE: ", RMSE))

compare_2016 %>% 
  filter(abs(`Predicted - Observed`) <= 5) %>% 
  nrow()/nrow(compare_2016)

print(paste0("2016 nrow: ", nrow(compare_2016)))


rm(compare_2016)


# end ---------------------------------------------------------------------

# Cleanup -----------------------------------------------------------------
stopCluster(cl)
