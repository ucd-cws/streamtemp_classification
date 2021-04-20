
# Code description --------------------------------------------------------

# This code compiles the information generated during the analysis for each of the 77 stream sites and identifies:

# -stream ID, site name,  lon, lat, operator, hydro_region, year range, k5_names, annual mean, annual max, and DOWY.


# Libraries ---------------------------------------------------------------

library(tidyverse)
library(sf)
library(lubridate)

# Load data ---------------------------------------------------------------

#This 11a data includes station_id, k5_names, site_name, lon, lat, operator, and HR_NAME
load("output/11a_agnes_k_5_final_w_centdist.rda") 

#This 09b data includes station_id, and model results for annual mean, annual max, DOWY
load("output/models/09b_annual_cluster_metrics_all_gages.rda")

#This data has date begin/end columns for usgs filled in, but not CDEC
load("data/all_gages.rda")

#This has date year range for CDEC stations, for all observations:
cdec_metadata <- read_csv("data/cdec_stations_metadata_filt8yr.csv")

load("data/cdec_stations_completed.rda") #this load as the station_list dataframe


# Filter data to build Table 1 --------------------------------------------

#drop geometry from data_k_sf dataframe
data_k_no_sf <- st_drop_geometry(data_k_sf_w_hydro_regions)

all_gages_no_sf <- all_gages %>% 
  rename(station_id = site_id) %>% 
  st_set_geometry(NULL)

all_data_no_range <- left_join(data_k_no_sf,ann_metrics)

all_data_usgs_range <- left_join(all_data_no_range,all_gages_no_sf)

#convert class type for date_begin and date_end so that the dates will read consistently with cdec

all_data_usgs_range$date_begin <- mdy(all_data_usgs_range$date_begin)

all_data_usgs_range$date_end <- mdy(all_data_usgs_range$date_end)

#wrangle cdec data to get sites with no duplicates
cdec_metadata_filtered <- cdec_metadata %>%
  select(site_id, interval, date_begin, date_end) 

cdec_all_data <- left_join(station_list,cdec_metadata_filtered, by = "site_id")

cdec_all_data <- cdec_all_data %>% 
  rename(station_id = site_id)

#convert class type in cdec_info so that date_begin and date_end match all_data_usgs_range

# cdec_all_data$date_begin <- as.character(cdec_all_data$date_begin)
# cdec_all_data$date_end <- as.character(cdec_all_data$date_end)
# cdec_all_data <- cdec_all_data %>% 
#   rename(station_id = site_id)

all_data_usgs_cdec_range <- left_join(all_data_usgs_range, cdec_all_data, by = "station_id")

# Add Shasta sites - start coding from here. Need to make a separate dataframe, then rbind it to the existing one.



#clean columns

all_data_usgs <- all_data_usgs_cdec_range %>% 
  filter(data_source == "USGS") %>% 
  rename(date_begin = date_begin.x) %>% 
  rename(date_end = date_end.x) %>% 
  select(station_id, site_name, lon, lat, HR_NAME, operator, data_source, date_begin, date_end,  k5_names, ann_mean, ann_max, DOWY)

all_data_cdec <- all_data_usgs_cdec_range %>% 
  filter(data_source == "CDEC") %>% 
  rename(date_begin = date_begin.y) %>% 
  rename(date_end = date_end.y) %>% 
  select(station_id, site_name, lon, lat, HR_NAME, operator, data_source, date_begin, date_end,  k5_names, ann_mean, ann_max, DOWY)

all_sites_final <- rbind(all_data_usgs, all_data_cdec)

#SAVE!!!
write_csv(all_sites_final, "output/all_sites_metadata_model_results.csv")


# Skip to here ------------------------------------------------------------

TableS1 <- read_csv("output/all_sites_metadata_model_results.csv")

sites_reg_unreg <- read_csv("output/12b_all_data_k_no_dam_dor.csv")

reg_sites_dor <- read_csv("output/12b_dam_data_k_dor_only.csv")
