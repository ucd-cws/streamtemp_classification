
# Filter out gages --------------------------------------------------------

library(tidyverse)
library(sf)
library(mapview) 

#Read the original list of sites from CDEC, downloaded to ID all stream temp monitoring stations
CDEC <- read.csv("data/CDEC_stations_with_water_temp.csv")

summary(CDEC)

#Make a new data frame that transforms the original data into data that can be mapped.
CDEC_sf <- CDEC %>% 
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326, remove = FALSE) %>% 
  mutate(data_source = "CDEC") %>% 
  st_transform(3310)

#Read the original USGS data
USGS <- read_csv("data/USGS_station_with_water_temp.csv", col_names = c("agency_cd
", "site_no", "station_nm", "site_tp_cd", "dec_lat_va", "dec_long_va", "coord_acy_cd", "dec_coord_datum_cd", "state_cd", "huc_cd", "inventory_dt", "qw_begin_date", "qw_end_date", "qw_count_nu"), skip = 35)

summary(USGS)

#Make a new data frame that transforms the original data to data that can be mapped
USGS_CA <- USGS %>%
  filter(state_cd == 6) %>%
  st_as_sf(coords = c("dec_long_va", "dec_lat_va"), crs = 4269, remove = FALSE) %>% 
  st_transform(3310)


# Load shape file ---------------------------------------------------------

#Load a shape file of the Bay Delta, which I will use to filter out in-Delta gages
delta <- st_read("data/Bay_delta_selection.shp") %>% 
  st_transform(3310) %>% 
  st_buffer(dist = 500)

mapview(delta)


# Remove Delta gages ------------------------------------------------------

#make a new object that includes only the USGS gages overlapping the Delta
in_delta_USGS <- st_intersection(USGS_CA, delta)

#make a new data frame that filters out the gages located in the Delta
USGS_no_delta <- USGS_CA %>% 
  filter(!(site_no %in% in_delta_USGS$site_no))

#Map both the in_delta and no_delta gages
mapview(USGS_no_delta)+mapview(in_delta_USGS, col.regions = "green")+mapview(delta, col.regions = "blue")

#Review the map and identify any gages that I want to include. Save the data in a csv.
#Repeat for CDEC gages

#make a new object that includes only the CDEC gages overlapping the Delta
in_delta_CDEC <- st_intersection(CDEC_sf, delta)

#make a new data frame that filters out the gages located in the Delta
CDEC_no_delta <- CDEC_sf %>% 
  filter(!(ID %in% in_delta_CDEC$ID))

#Map both the in_delta and no_delta gages
mapview(CDEC_no_delta)+mapview(in_delta_CDEC, col.regions = "magenta")+mapview(delta, col.regions = "blue")

#review the filtered gages, make a dataframe of gages to keep
CDEC_do_not_filter <- read.csv("data/CDEC_do_not_filter.csv")
USGS_do_not_filter <- read.csv("data/USGS_do_not_filter.csv")

CDEC_refined <- CDEC_sf %>% 
  select(ID, Station.Name, Longitude, Latitude, Operator) %>% 
  filter(ID %in% CDEC_do_not_filter$ID | ID %in% CDEC_no_delta$ID) 

length(unique(CDEC_refined$ID))

write.csv(CDEC_refined, "data/CDEC_refined.csv") #Manually review the sites to pull out ones that have "well", "slough", or are unclear. Save these in a new .csv file names CDEC_review.

CDEC_review <- read.csv("data/CDEC_review.csv")

CDEC_maybe_filter <- CDEC_refined %>% 
  filter(ID %in% CDEC_review$ID)

mapview(CDEC_maybe_filter) #review the sites on the map and determine which ones should be removed from the filter

CDEC_no_wells_sloughs <- read.csv("data/CDEC_review.csv")

CDEC_final <- CDEC_refined %>% 
  filter(!(ID %in% CDEC_no_wells_sloughs$ID))

length(unique(CDEC_final$ID))

mapview(CDEC_final)

USGS_final <- USGS_CA %>% 
  select(site_no, site_tp_cd ,dec_long_va, dec_lat_va, qw_begin_date, qw_end_date) %>% 
  filter(site_no %in% USGS_do_not_filter$site_no | site_no %in% USGS_no_delta$site_no) %>% 
  filter(site_tp_cd == "ST") #this filters the USGS data by site type to only include streams

length(unique(USGS_final$site_no))

#Map the final dataset

mapview(CDEC_final, col.regions = "magenta")+mapview(USGS_final, col.regions = "turquoise")

# Filtering duplicate sites -----------------------------------------------

CDEC_buffer <- st_buffer(CDEC_final, dist = 500)

gage_duplicate <- st_intersection(USGS_final, CDEC_buffer)

mapview(USGS_final, col.regions = "blue")+mapview(CDEC_final, col.region = "red") + mapview(gage_duplicate, col.regions = "purple")

write.csv(gage_duplicate, "data/gage_duplicate.csv") #manually review these gages to see what is available;

#All gages and full records from duplicate site are available from USGS, so I'm going to remove them from the CDEC list

CDEC_final_no_duplicates <- CDEC_final %>% 
  filter(!(ID %in% gage_duplicate$ID))

# save out for visualization later
save(CDEC_final_no_duplicates, USGS_final, file = "data/cdec_usgs_final_selected_no_dups.rda")

mapview(CDEC_final_no_duplicates, col.regions = "magenta")+mapview(USGS_final, col.regions = "turquoise")

#write csv files and put in data folder: final gages list for both USGS and CDEC
write_csv(USGS_final, path = "data/USGS_gages_final.csv")
write_csv(CDEC_final_no_duplicates, path = "data/CDEC_gages_final.csv")
