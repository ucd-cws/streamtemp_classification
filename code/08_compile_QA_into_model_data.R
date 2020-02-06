
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
  st_as_sf(coords = c("lon", "lat"), crs = 4326, remove = FALSE)

hydro_regions <- st_read("data/DWR_HydrologicRegions-utm11.shp") %>% 
  st_transform(3310)

mapview(gage_QA_all, zcol = "operator") + mapview(hydro_regions, col.regions = NA)

#Need to fix map; not sure why data isn't plotting in the correct projection. 


# Compile multi-year data into single annual record -----------------------

### Write code compiling data into single time series for each data file in the data/QA_data folder.

#Start with cdec files; the columns are in a different format and order than the usgs files, so we need to analyze them separately

# specify the file ending more explicitly to make this more flexible:
cdec_site <- list.files(path = "data/QA_data", 
                        pattern = "cdec_*(.*)rds$", ignore.case = TRUE)

# add ignore case
cdec_site_w_path <- list.files("data/QA_data","cdec_*(.*)rds$", 
                               full.names = TRUE, ignore.case = TRUE)

library(purrr)

# now loop through and read in the files
cdec_dfs <- map(cdec_site_w_path, ~read_rds(.x)) %>%
bind_rows()

# lets see what sites are hourly/datetime and filter those
cdec_datetime <- cdec_dfs %>%
filter(!is.na(datetime))

# what stations are being/need to be fixed?
cdec_datetime %>% group_by(station_id) %>% tally()

# make a dataset of the data that is good (and all daily)
cdec_clean_df <- cdec_dfs %>%
  filter(is.na(datetime), !is.na(station_id)) %>% 
  # drop all the old hourly cols that are now totally NAs (check with summary())
  select(-c(site_id:water_day))

# add water year, water year day, etc:
cdec_clean_df <- cdec_clean_df %>% add_WYD(., "date")

names(cdec_clean_df)

# Add month-day (e.g. 01-01) so that when I compile the data into a single dataset, each julian day is associated with a month-day to make interpretation easier.
  
# now group and average by water day
model_data <- cdec_clean_df %>% 
  group_by(station_id, DOWY) %>% 
  summarize(mean_temp_C = mean(value_mean_C, na.rm = T))

# VIEW!
ggplotly(
  ggplot() + 
    geom_line(data=model_data, aes(x=DOWY, y=mean_temp_C, color=station_id), show.legend = F) +
    scale_color_viridis_d())
  
# to do...add month-day

# write_csv(model_data, path = paste0("data/model_data/",model_data[1,1],"_model_data.csv"))
  