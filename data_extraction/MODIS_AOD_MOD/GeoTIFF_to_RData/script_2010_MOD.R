#####################################################################
# Purpose: Bind tiffs, extract data, bind all data for a day
# Author: namanpaul
# Last modified by: namanpaul
# R Version: # use sessionInfo()
# Date: # use Sys.Date()
#####################################################################

# load the packages
#library(terra)  # For handling raster data
library(tidyverse)
library(stars)# For data manipulation

# input and output paths
MOD_input_path <- "input_path/"
MOD_output_path <- "output_path/"

# scaling factor for opt depth 470,550,660,2130 nm
scaling_factor  <- 0.001000000047497451

# year val
year_val <- 2010

#create the date range in julian days
date_range <- seq(ymd(paste(year_val,"-01-01", sep = "")),
                  ymd(paste(year_val,"-12-31", sep = "")),
                  by='day')


list_julian_days <- lubridate::yday(date_range)
list_julian_days <- paste0(sprintf("%03d", list_julian_days))

for(i in seq_along(list_julian_days)){

counter <- 0
#list of all the files
MOD_files <- list.files(path = paste0(MOD_input_path, year_val,'/',list_julian_days[i]),
                        pattern = paste0('MOD04_3K.A',year_val,list_julian_days[i]),
                        full.names = T)


# now in this list of MOD tif files, find the ones with the same base name
# Function to extract the base identifier from the full file path
# Define the function to extract identifiers (already provided)
extract_identifier <- function(file) {
  if (grepl("A[0-9]{7}\\.[0-9]{4}\\.[0-9]{3}\\.[0-9]{13}", file)) {
    sub("^.*?(A[0-9]{7}\\.[0-9]{4}\\.[0-9]{3}\\.[0-9]{13}).*$", "\\1", file)
  } else {
    NA
  }
}

# Extract unique identifiers
identifiers <- sapply(basename(MOD_files), extract_identifier)


unique_ids <- unique(identifiers[!is.na(identifiers)])

# Initialize an empty list to store daily data frames


# Process each unique identifier
for (id in unique_ids) {
  # Get files corresponding to the current identifier
  matching_files <- MOD_files[identifiers == id]
  
  # counter + 1
  counter <- counter+1
  
  # Read the matching rasters
  raster_stack <- lapply(matching_files, stars::read_stars)
  
  # convert to df, xy = T
  raster_df <- as.data.frame(raster_stack, xy = TRUE, long = FALSE)
  
  # count the number of nas
  nan_proportion <- sum(is.na(raster_df[, 3])/nrow(raster_df))
  
  # Check if the proportion of NaN values is greater than 99%
   if (nan_proportion >= 0.99) {
     # Print a message and skip to the next file
     print(paste0("Skipping ",matching_files," due to 99% or more NaN values"))
   }
  
   else{
    # clean up the dataframe
    raster_df_clean <- raster_df %>% 
      rename_with(~ ifelse(grepl("Band1", .x), "opt_depth_470nm", .x), everything()) %>%
      rename_with(~ ifelse(grepl("Band2", .x), "opt_depth_550nm", .x), everything()) %>%
      rename_with(~ ifelse(grepl("Band3", .x), "opt_depth_660nm", .x), everything()) %>%
      rename_with(~ ifelse(grepl("wav2p1", .x), "opt_depth_2130nm", .x), everything()) %>%
      rename_with(~ ifelse(grepl("Mass", .x), "mass_conc_land", .x), everything()) %>%
      rename_with(~ ifelse(grepl("Time", .x), "time", .x), everything()) %>% 
      mutate(time = as.numeric(time)) %>% 
      select(longitude = x,
             latitude = y,
             time,
             opt_depth_470nm, 
             opt_depth_550nm, 
             opt_depth_660nm,
             opt_depth_2130nm,
             mass_conc_land) %>% # test for NAs
      mutate(time_UTC = lubridate::as_datetime(time, origin = "1993-01-01 00:00:00"),
             time_CST = time_UTC-hours(6),
             acq_date = as_date(time_CST),
             opt_depth_470nm = scaling_factor*opt_depth_470nm,
             opt_depth_550nm = scaling_factor*opt_depth_550nm,
             opt_depth_660nm = scaling_factor*opt_depth_660nm,
             opt_depth_2130nm = scaling_factor*opt_depth_2130nm,
             satellite_name = 'MOD') %>% 
      select(-time_UTC, -time) %>% 
      mutate(opt_depth_470nm = case_when(opt_depth_470nm < -0.1 ~ -0.1, #this would deal with the fill value as well -9.99 (valid range -0.1, +5)
                                         opt_depth_470nm >= 5 ~ 5,
                                         TRUE ~ as.numeric(opt_depth_470nm)),
             opt_depth_550nm = case_when(opt_depth_550nm < -0.1 ~ -0.1,
                                         opt_depth_550nm >= 5 ~ 5,
                                         TRUE ~ as.numeric(opt_depth_550nm)),
             opt_depth_660nm = case_when(opt_depth_660nm < -0.1 ~ -0.1,
                                         opt_depth_660nm >= 5 ~ 5,
                                         TRUE ~ as.numeric(opt_depth_660nm)),
             opt_depth_2130nm = case_when(opt_depth_2130nm < -0.1 ~ -0.1,
                                          opt_depth_2130nm >= 5 ~ 5,
                                          TRUE ~ as.numeric(opt_depth_2130nm)))
  # save 
  save(raster_df_clean,
          file = paste0(MOD_output_path,'MOD_data_',year_val,'_',list_julian_days[i],'_',counter,'.RData'))
   }
}
}
