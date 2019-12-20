# 05_split out data

library(sf)
library(tidyverse)
library(mapview)
library(purrr)
library(lubridate)

# LOAD DATA -----------------------------------------------------------

# load data
load("data/all_gages.rda")
all_gages %>% st_drop_geometry() -> all_gages

# cdec data
load("data/cdec_stations_completed.rda") # station list
cdec_stations <- station_list; rm(station_list)
load("data/cdec_stations_metadata_dateranges_112_stations.rda")
load("data/cdec_temps_all_daily_filt8yr.rda")
load("data/cdec_temps_all_hourly_filt8yr.rda")
load("data/cdec_temps_all_event_filt8yr.rda")

# usgs data
load("data/usgs_stations_completed.rda")
usgs_stations <- station_list_usgs
load("data/usgs_stations_metadata_dateranges_filt8.rda")
load("data/usgs_temps_all_daily_filt8yr.rda")
load("data/usgs_temps_all_iv_filt8yr.rda")



# PLot --------------------------------------------------------------------

# plot by facet
plotly::ggplotly(
  ggplot() +
    geom_line(data=cdec_temps_day %>% filter(flag=="N"),
               aes(x=datetime, y=value, color=station_id), alpha=.7,show.legend = F)+
    facet_grid(station_id~.) +
    scale_color_viridis_d()+
    theme_bw()+
    #ggdark::dark_theme_classic() +
    theme(axis.text.x = element_text(angle=90, hjust = 1))
)
