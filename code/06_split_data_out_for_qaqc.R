# 06_split out data

library(sf)
library(tidyverse)
library(purrr)
library(lubridate)

# LOAD DATA -----------------------------------------------------------

# load data
load("data/all_gages.rda")
all_gages %>% st_drop_geometry() -> all_gages

# cdec data
cdec_metadata_filt8 <- read_csv("data/cdec_stations_metadata_filt8yr.csv")
load("data/cdec_temps_merged_daily_filt8yr.rda")

# usgs data
usgs_metadata_filt8 <- read_csv("data/usgs_stations_metadata_filt8yr.csv")
load("data/usgs_temps_merged_daily_filt8yr.rda")

# Check Number of Sites ---------------------------------------------------

usgs_daily %>% distinct(station_id) %>% tally() # 22
cdec_daily %>% distinct(station_id) %>% tally() # 63

# Split out Data and Write OUT -----------------------------------------

# originally wrote to CSV but file sizes are VERY large so opted for .rds (RDATA) files since they are more compressed.
# you can read in .rds files with readRDS or read_rds

## CDEC DAILY
cdec_daily %>%
  split(.$station_id) %>% # split by site ID
  #walk2(names(.), ~write_csv(.x, path = paste0("data/data_review/cdec_daily_",.y, '.csv'))) # write to CSV
  walk2(names(.), ~write_rds(.x, path = paste0("data/data_review/cdec_daily_",.y, '.rds'))) # write to RDS file (.rds) # add compression with "compress="gz"

# to read one of these files in use:
# cdec_daily_DNB <- read_rds("data/data_review/cdec_daily_DNB.rds")

## USGS DAILY
usgs_daily %>%
  split(.$station_id) %>% # split by site
  walk2(names(.), ~write_rds(.x, path = paste0("data/data_review/usgs_daily_",.y, '.rds'))) # write to RData file (.rds) 

# check number of files matches number of total gages?
length(list.files("data/data_review", pattern = ".rds")) ## should match cdec_stations == 63 + usgs_metadata_filt8 == 22 (n=95)
