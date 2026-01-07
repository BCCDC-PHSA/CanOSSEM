 #####################################################################
 # Purpose: Patch PM2.5 holes, list of rCN-date identifiers
 # Author: namanpaul
 # Last modified by: namanpaul
 # R Version: 4.3.0
 # Date: 2025-03-07
 #####################################################################

 # load packages
 library(tidyverse)
 library(sf)
 
 # load input data
 load('path/MAIN_DATA_REPOSITORY/annual_predictions/annual_input_datasets/CanOSSEM_2010_input.RData')
 
 # check for missing values (var structure in M1)
 CanOSSEM_2010_input <- CanOSSEM_2010_input %>% 
   select(-SiteID, -SiteName, -min_TLML, -max_TLML,
          -min_QLML, -max_QLML, -AOD_distance, -MERRA_distance, -NAPS_distance, -Source) %>% 
   mutate(HMS_value = ifelse(is.na(HMS_value), 0, 1))
 
 # missing MERRA
 missing_input <- CanOSSEM_2010_input %>% 
   filter(is.na(PBLH)) %>% 
   select(rCN_date_identifier, CanOSSEM_rCN, date_val) %>% 
   distinct()

 save(missing_input,
      file = 'path/MAIN_DATA_REPOSITORY/annual_predictions/missing_estimates_rCN_date_identifier/missing_identifier_2010.RData')
 
 rm(CanOSSEM_2010_input)
 gc()
 
  # and now
 # find nearest raster cell that estimates are available for
 #load('H:/MAIN_DATA_REPOSITORY/annual_predictions/missing_estimates_rCN_date_identifier/missing_identifier_2018.RData')
 
 load("path/CanOSSEM_list_of_populated_cells_197471.RData")
 load("path/CanOSSEM_4326.RData")
 # load the 197471 coordinates raster-df
 CanOSSEM_sf <- left_join(CanOSSEM_list_of_populated_cells,
                                               CanOSSEM_base_raster_4326_dataframe,
                                               by = 'CanOSSEM_rCN')

 CanOSSEM_sf <- st_as_sf(CanOSSEM_sf,
                         coords = c('longitude','latitude'))

 # create a smaller subset to match with
 unique_missing_rCNs <- unique(missing_input$CanOSSEM_rCN)
 
 missing_sf <- CanOSSEM_sf %>% 
   filter(CanOSSEM_rCN %in% unique_missing_rCNs)

 nearest_neighbours <- st_nn(missing_sf, CanOSSEM_sf, k = 2, returnDist = TRUE) 

 neighbour_df <- do.call(rbind, lapply(seq_along(nearest_neighbours$nn), function(i) {
   data.frame(
     query_id = missing_sf$CanOSSEM_rCN[i],  # ID from sf_points_1
     neighbour_id = CanOSSEM_sf$CanOSSEM_rCN[nearest_neighbours$nn[[i]]],  # Nearest neighbors from sf_points_2
     distance = nearest_neighbours$dist[[i]]  # Corresponding distances
   )
 }))

 # filter if the distance is 0
 neighbour_df <- neighbour_df %>% 
   filter(!distance == 0)

 # check estimates
 load("path/MAIN_DATA_REPOSITORY/annual_predictions/Primary_Secondary_combined/CanOSSEM_estimates_2018.RData")
 
 # neigbours have estimate data?
 CanOSSEM_estimates <- CanOSSEM_estimates %>% 
   mutate(date_val = ymd(str_sub(rCN_date_identifier, 1,10)),
          CanOSSEM_rCN = as.numeric(str_sub(rCN_date_identifier, 12))) 

 neighbour_df <- neighbour_df %>%
   mutate(status = neighbour_id %in% CanOSSEM_estimates$CanOSSEM_rCN)   
 
 # first pass
 first_pass_true <- neighbour_df %>% 
   filter(status == T)
 
 second_pass_true <- neighbour_df %>% 
   filter(status == T,
          !query_id %in% first_pass_true$query_id)

