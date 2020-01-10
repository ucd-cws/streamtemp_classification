# 05_split out data

library(sf)
library(tidyverse)
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


# Get list of sites -------------------------------------------------------

usgs_temps_day %>% distinct(site_no) %>% tally() # 25
usgs_temps_iv %>% distinct(site_no) %>% tally() # 6

usgs_temps_iv %>% distinct(site_no) %>% 
  anti_join(., usgs_temps_day,  by="site_no") # should be 6


cdec_temps_day %>% distinct(site_id) %>% tally() # 7
cdec_temps_hr %>% distinct(site_id) %>% tally() # 39
cdec_temps_min %>% distinct(site_id) %>% tally() # 18

all_gages %>% st_drop_geometry %>% filter(data_source=="USGS") %>% 
  anti_join(., usgs_temps_day, by=c("site_id"="site_no")) %>% 
  as.data.frame()

# Split out Data and Write OUT -----------------------------------------

# originally wrote to CSV but file sizes are VERY large so opted for .rds (RDATA) files since they are more compressed.
# you can read in .rds files with readRDS or read_rds

## CDEC DAILY
cdec_temps_day %>%
  split(.$site_id) %>% # split by site ID
  #walk2(names(.), ~write_csv(.x, path = paste0("data/data_review/cdec_daily_",.y, '.csv'))) # write to CSV
  walk2(names(.), ~write_rds(.x, path = paste0("data/data_review/cdec_daily_",.y, '.rds'))) # write to RData file (.rds) 

## CDEC HOURLY
cdec_temps_hr %>% 
  split(.$site_id) %>% # split by site ID
  walk2(names(.), ~write_rds(.x, path = paste0("data/data_review/cdec_hourly_",.y, '.rds'),compress = "gz")) # more compressed

## CDEC MINUTES
cdec_temps_min %>% 
  split(.$site_id) %>% # split by site ID
  walk2(names(.), ~write_rds(.x, path = paste0("data/data_review/cdec_hourly_",.y, '.rds'), compress="gz")) # write to RData file

# to read one of these files in use:
# cdec_hourly_DNB <- read_rds("data/data_review/cdec_hourly_DNB.rds")

## USGS DAILY
usgs_temps_day %>%
  split(.$site_no) %>% # split by site
  walk2(names(.), ~write_rds(.x, path = paste0("data/data_review/usgs_daily_",.y, '.rds'))) # write to RData file (.rds) 

## USGS INSTANTANEOUS
usgs_temps_iv %>% 
  split(.$site_no) %>% # split by site ID
  walk2(names(.), ~write_rds(.x, path = paste0("data/data_review/usgs_minute_",.y, '.rds'),compress = "gz")) # more compressed


# check number of files matches number of total gages?
length(list.files("data/data_review")) ## should match cdec_stations == 64 + usgs_metadata_filt8 == 31 (n=95)
