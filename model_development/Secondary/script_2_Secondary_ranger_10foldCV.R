######################################################################
# Purpose: 10-fold cross validation Secondary CanOSSEM
# Author: namanpaul
# Last modified by: namanpaul
# R Version: 4.3.0
# Date: 2025-02-16
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
# load the dataset
load('path/MAIN_DATA_REPOSITORY/modeling_datasets/CanOSSEM_training_set.RData')

# randomizing the dataset
set.seed(141)

# training set, keep relevant cols
training_set <- training_set %>% 
  select(-year_val, -date_val, -CanOSSEM_rCN, -SiteID, -SiteName, -region, -min_TLML, -max_TLML,
         -min_QLML, -max_QLML, -ctry_en, -AOD_distance, -MERRA_distance, -NAPS_distance, -Source,
         -AOD_distance, -mass_conc_land, -opt_depth_470nm, -opt_depth_550nm, -opt_depth_660nm, -opt_depth_2130nm)

# setting up the response var
training_set_response <- training_set$daily_mean_pm25

# executing the CV
rgcv(trainx = training_set_vars,
     trainy = training_set_response,
     cv.fold = 10,
     mtry = 5,
     verbose = TRUE)

# Cleanup -----------------------------------------------------------------
stopCluster(cl)