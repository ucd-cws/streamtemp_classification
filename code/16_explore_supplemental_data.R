
# Code description --------------------------------------------------------

# This code is to explore the results of the supplemental table that includes the degree of regulation results from Grantham et al. (2014). 


# Libraries ---------------------------------------------------------------

library(tidyverse)
library(sf)

# Load data ---------------------------------------------------------------

DOR_table <- read_csv("output/12b_dam_data_k_dor_only.csv")

cluster_results <- read_csv("output/12b_all_data_k_no_dam_dor.csv")

load("output/12_data_k_centdist_damdist.rda")

# Wrangle data ------------------------------------------------------------

# Explore data to see the relationship between the degree of regulation, downstream distance from dam, and distance from centroid to see if there's a relationship

dam_dist <- data_k_dist %>%
  st_drop_geometry() %>% 
  select(station_id,dist_to_centroid,cum_len_km) %>% 
  filter(!is.na(cum_len_km))
  
CDOR_descending <- DOR_table %>%
  select(station_id, RIVER, dam_name, DOR, CDOR) %>% 
  filter(!is.na(CDOR)) %>% 
  arrange(desc(CDOR)) %>% 
  filter(CDOR >= 1.0)

dist_and_DOR <- full_join(dam_dist,CDOR_descending, by = "station_id")

dist_and_DOR_sorted <- dist_and_DOR %>% 
  filter(!is.na(CDOR)) %>% 
  arrange(desc(CDOR))
