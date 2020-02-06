
# Code description --------------------------------------------------------

# This code maps the final set of gage data that went through the QA process and then averages daily values, by water day, to create a single dataset that will be used for thermal regime modeling and classification.


# Libraries ---------------------------------------------------------------

library(mapview)
library(sf)
library(tidyverse)
library(tmap)
library(ggrepel)
library(ggspatial)

library(lubridate)
library(plotly)

# Import list of gages ----------------------------------------------------

gage_QA_progress <- read_csv("data/data_review/gage_QA_progress.csv")

gage_QA_all <- gage_QA_progress %>% 
  filter(notes == "QA complete") %>% 
  select(site_id, site_name, lon, lat, operator)

# Map remaining sites in modeling analysis --------------------------------

#Transform gage data to spatial data

gage_QA_all <- gage_QA_all %>% 
  st_as_sf(coords = c("lon", "lat"), crs = 3310, remove = FALSE)

hydro_regions <- st_read("data/DWR_HydrologicRegions-utm11.shp") %>% 
  st_transform(3310)

mapview(gage_QA_all, zcol = "operator") + mapview(hydro_regions, col.regions = NA)

#Need to fix map; not sure why data isn't plotting in the correct projection. 


# Compile multi-year data into single annual record -----------------------

### Write code compiling data into single time series for each data file in the data/QA_data folder.

#Start with cdec files; the columns are in a different format and order than the usgs files, so we need to analyze them separately

cdec_site <- list.files("data/QA_data","cdec_.*")
cdec_site_w_path <- list.files("data/QA_data","cdec_.*", full.names = TRUE)

cdec_dfs <- lapply(X = cdec_site_w_path, FUN  = function(x) read_rds(x))

for(i in 1:length(cdec_dfs)){
  df <- cdec_dfs[[i]] #a single dataframe from the list of all data frames
  
  # df <- cdec_dfs[[1]] #code I included when I was testing my code on a single site (i.e., before I tried the for-loop)
  
  df <- df %>% 
    mutate(julian_day = yday(date)) ###Add month-day (e.g. 01-01) so that when I compile the data into a single dataset, each julian day is associated with a month-day to make interpretation easier.
  
  model_data <- df %>% 
    mutate(water_day = if_else(julian_day > 273, julian_day-273, julian_day+(365-273))) %>% 
    group_by(station_id,water_day) %>%
    summarize(mean_temp = mean(value_mean_C))
  
#  ggplotly(
#    ggplot() + geom_line(data=model_data, aes(x=water_day, y=mean_temp))) #code I included to visualize the results from a single site, before I tried the for-loop
  
  write_csv(model_data, path = paste0("data/model_data/",model_data[1,1],"_model_data.csv"))
  
}

# Code runs through cdec site CLP, then stops with an error.
