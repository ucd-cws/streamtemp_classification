
# Code description --------------------------------------------------------

# This code add the Shasta coordinates to the gage list and maps the cluster results from code 10_calculate_model_metrics_for_classification


# Libraries ---------------------------------------------------------------

library(mapview)
library(sf)
library(tidyverse)


# Load data ---------------------------------------------------------------

hydro_regions <- st_read("data/DWR_HydrologicRegions-utm11.shp") %>%
  st_transform(4326)

all_sites <- read_csv("data/data_review/gage_QA_progress.csv")

sites <- all_sites %>% 
  filter(grepl("QA complete", notes)) %>% 
  rename(station_id = site_id)

# Add K_group Data --------------------------------------------------------

load("output/models/classification_group_results.rda") # class_groups
unique(class_groups$station_id)

# find differences between lists:
anti_join(sites, class_groups) # localities in sites df not in class_groups
anti_join(class_groups, sites) # localiteis in class_groups not in sites

# these sites are different and won't join...
# class_grps  = sites df above
# HIQ  ?
# 11468900  ?
# SRabvBSC = SR_abv_BSC    
# SRabvPC = SR_abv_Parks 

class_groups <- class_groups %>% 
  mutate(station_id = case_when(
    #grepl("HIQ", station_id) ~ "11468900",
    grepl("SRabvBSC", station_id) ~ "SR_abv_BSC",
    grepl("SRabvPC", station_id) ~ "SR_abv_Parks",
    TRUE ~ station_id # if already exists leave it as is
  ))

# join and save
data_k <- left_join(sites, class_groups) %>% 
  filter(!is.na(k_3)) # drop the one NA from the group: 11468900

# how many per group?
table(data_k$k_3)
table(data_k$k_5)

# make spatial ------------------------------------------------------------

data_k_sf <- data_k %>% 
  st_as_sf(coords = c("lon", "lat"), crs = 4326, remove = FALSE)

# map k3
mapview(data_k_sf, zcol="k_3", 
        #col.regions=RColorBrewer::brewer.pal(3, "Set1") , 
        burst=TRUE, hide=FALSE, homebutton=FALSE) +
  mapview(hydro_regions, col.regions = NA, legend=FALSE)

# map k5
mapview(data_k_sf,  zcol="k_5",
        #col.regions=RColorBrewer::brewer.pal(5, "Set1") , 
        burst=TRUE, hide=FALSE, homebutton=FALSE) +
  mapview(hydro_regions, col.regions = NA, legend=FALSE)

