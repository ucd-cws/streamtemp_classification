
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

all_data_usgs_cdec_range <- left_join(all_data_usgs_range, cdec_all_data, by = "station_id")

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

all_data_other <- all_data_usgs_cdec_range %>% 
  filter(operator == "CDFW"|operator == "TNC"|operator == "UCD") %>%
  rename(date_begin = date_begin.x) %>% 
  rename(date_end = date_end.x) %>% 
  select(station_id, site_name, lon, lat, HR_NAME, operator, data_source, date_begin, date_end,  k5_names, ann_mean, ann_max, DOWY)

all_sites_final <- rbind(all_data_usgs, all_data_cdec, all_data_other)

#remove duplicates based on station_id columns
all_sites_final <- all_sites_final[!duplicated(all_sites_final$station_id),]

#add metadata to 6 sites: BSC_*, PC_mouth, SR_*
all_sites_final[all_sites_final$station_id=="BSC_dam", 7:9] <- c("UCD", "2008-07-30","2019-09-30")

all_sites_final[all_sites_final$station_id=="BSC_mouth", 7:9] <- c("UCD", "2008-03-11","2019-09-30")

all_sites_final[all_sites_final$station_id=="BSC_spring", 7:9] <- c("UCD", "2008-03-10","2017-05-23")

all_sites_final[all_sites_final$station_id=="PC_mouth", 7:9] <- c("UCD", "2008-04-25","2019-09-30")

all_sites_final[all_sites_final$station_id=="SR_abv_BSC", 7:9] <- c("UCD", "2008-08-13","2019-09-30")

all_sites_final[all_sites_final$station_id=="SR_abv_Parks", 7:9] <- c("UCD", "2008-03-26","2019-09-30")

#SAVE!!!

write_csv(all_sites_final, "output/all_sites_metadata_model_results.csv")

# Table S1: Reg/Unreg --------------------------------------------------------

#Import data from Ryan with reg/unregulated labels and degree of regulation
sites_reg_unreg <- read_csv("output/12b_all_data_k_no_dam_dor.csv")

#filter data frame for only necessary columns
sites_reg_unreg_filtered <- sites_reg_unreg %>% 
  select(station_id, dist_to_centroid, reg_type)

#join with all sites dataframe to make S1 Table

S1_Table <- full_join(all_sites_final, sites_reg_unreg_filtered, by = "station_id")

# SAVE!!!
write_csv(S1_Table, "output/TableS1.csv")


# Table S2: degree of regulation ------------------------------------------

#degree of regulation
reg_sites_dor <- read_csv("output/12b_dam_data_k_dor_only.csv")

#distance from dam
load("output/12_data_k_centdist_damdist.rda") #object name: data_k_dist

#drop geometry
data_k_dist_no_sf <- st_drop_geometry(data_k_dist)

#filter for only necessary columns

reg_sites_dor_filtered <- reg_sites_dor %>% 
  select(station_id, site_name, k5_names, RIVER, DRAIN_SQKM, runoff_m3, dam_name, dam_lon, dam_lat, STO_m3, CUMSTO_m3, DOR, CDOR)

dist_to_dam <- data_k_dist_no_sf %>% 
  select(station_id, HR_NAME, cum_len_km)

dor_and_dist_to_dam <- left_join(reg_sites_dor_filtered,dist_to_dam, by = "station_id")

S2_Table <- dor_and_dist_to_dam %>% 
  select(station_id, site_name, k5_names, HR_NAME, RIVER, DRAIN_SQKM, runoff_m3, cum_len_km, dam_name, dam_lon, dam_lat, STO_m3, CUMSTO_m3, DOR, CDOR) %>% 
  rename(dist_blw_dam_km = cum_len_km)

#  SAVE!!!
write_csv(S2_Table, "output/TableS2.csv")
