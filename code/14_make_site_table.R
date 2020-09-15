
# Code description --------------------------------------------------------

# This code compiles the information generated during the analysis for each of the 77 stream sites and identifies:

# -stream ID, site name,  lon, lat, operator, hydro_region, year range, k5_names, annual mean, annual max, and DOWY.


# Libraries ---------------------------------------------------------------

library(tidyverse)
library(sf)


# Load data ---------------------------------------------------------------

#This 11a data includes station_id, k5_names, site_name, lon, lat, operator, and HR_NAME
load("output/11a_agnes_k_5_final_w_centdist.rda") 

#This 09b data includes station_id, and model results for annual mean, annual max, DOWY
load("output/models/09b_annual_cluster_metrics_all_gages.rda")

#This data has date begin/end columns for usgs filled in, but not CDEC
load("data/all_gages.rda")

#This has date year range for CDEC stations, for all observations:
load("data/cdec_temps_all_Daily_filt8yr.rda")

#Still need total number of years for all stations. Can add this to the table I'm setting up with the site information and model metrics.



# Filter data to build Table 1 --------------------------------------------

#drop geometry from data_k_sf dataframe
data_k_no_sf <- st_set_geometry(data_k_sf_w_hydro_regions, NULL)

all_gages_no_sf <- all_gages %>% 
  rename(station_id = site_id) %>% 
  st_set_geometry(NULL)

all_data_no_range <- left_join(data_k_no_sf,ann_metrics)

all_data_usgs_range <- left_join(all_data_no_range,all_gages_no_sf)

#filter cdec_temps_day so that there's a single observation for each site, then combine with all_data_no_range; or figure out how to do a partial join based on a vlookup type of command


#filter all_data to include columns ordered for the final table.
filtered_data <- all_data %>% 
  select(station_id, site_name, lon, lat, operator, HR_NAME, k5_names, ann_mean, ann_max, DOWY)
