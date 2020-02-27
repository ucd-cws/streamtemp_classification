
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
library(purrr)

# Import list of all gages ----------------------------------------------------

gage_QA_progress <- read_csv("data/data_review/gage_QA_progress.csv")

gage_QA_all <- gage_QA_progress %>% 
  filter(notes == "QA complete") %>% 
  select(site_id, site_name, lon, lat, operator)

# Map remaining sites in modeling analysis --------------------------------

#Transform gage data to spatial data

gage_QA_all <- gage_QA_all %>% 
  st_as_sf(coords = c("lon", "lat"), crs = 4326, remove = FALSE)

# hydro_regions <- st_read("data/DWR_HydrologicRegions-utm11.shp") %>%
#   st_transform(3310)
# 
# mapview(gage_QA_all, zcol = "operator") + mapview(hydro_regions, col.regions = NA)
# 
# ggplot()+
#   geom_sf(data = hydro_regions, fill = NA, color = 'darkslategrey', size = 1, alpha = 0.4) +
#   geom_sf(data = gage_QA_all, aes(fill = operator), pch = 21, size = 4) +
#   annotation_north_arrow(location = "tr", pad_y = unit(0.1,"cm")) +
#   annotation_scale()
# 
# ggsave(filename = "output/figures/final_gages_in_hyd_regions.png", dpi = 300, width = 8.5, height = 11, units = "in")

# Compile cdec multi-year data into single annual record -----------------------

### Write code compiling data into single time series for each data file in the data/QA_data folder.

#Start with cdec files; the columns are in a different format and order than the usgs files, so we need to analyze them separately

# specify the file ending more explicitly to make this more flexible:
cdec_site <- list.files(path = "data/QA_data", 
                        pattern = "cdec_*(.*)rds$", ignore.case = TRUE)

# add ignore case
cdec_site_w_path <- list.files("data/QA_data","cdec_*(.*)rds$", 
                               full.names = TRUE, ignore.case = TRUE)

# now loop through and read in the files
cdec_dfs <- map(cdec_site_w_path, ~read_rds(.x)) %>%
bind_rows() %>% 
  filter(!is.na(value_mean_C))

# check for NAs
summary(cdec_dfs)

# lets see what sites are hourly/datetime and filter those

#ADW: all sites have been fixed, so I commented out these lines 02/11/2020

#cdec_datetime <- cdec_dfs %>%
#filter(!is.na(datetime))

# what stations are being/need to be fixed?
#cdec_datetime %>% group_by(station_id) %>% tally()

# make a dataset of the data that is good (and all daily)
cdec_clean_df <- cdec_dfs 

# add water year, water year day, etc:
# need package: devtools::install_github("ryanpeek/wateRshedTools")
library(wateRshedTools)

#Ryan's day-of-water-year function
dowy<-function(YYYYMMDD_HMS) {   # Dates must be POSIXct
  
  YYYYMMDD_HMS<-YYYYMMDD_HMS
  doy<-lubridate::yday(YYYYMMDD_HMS)
  
  # make DOWY
  offsetday = ifelse(month(YYYYMMDD_HMS) > 9, -273, 92)
  DOWY = doy + offsetday
  
  # adjust for leap year
  offsetyr = ifelse(lubridate::leap_year(YYYYMMDD_HMS), 1, 0) # Leap Year offset
  adj.wyd = ifelse(offsetyr==1 & doy > 274, DOWY - 1, DOWY)
  
  return(adj.wyd)
}

#Ryan's water year function
wtr_yr <- function(dates, start_month=10) {
  # Convert dates into POSIXlt
  dates.posix = as.POSIXlt(dates)
  # Year offset
  offset = ifelse(dates.posix$mon >= start_month - 1, 1, 0)
  # Water year
  adj.year = dates.posix$year + 1900 + offset
  # Return the water year
  return(adj.year)
}

#Ryan's add water year data function
add_WYD <- function(df, datecolumn){
  datecolumn=datecolumn
  df["DOY"] <- as.integer(sapply(df[,c(datecolumn)], yday))
  df["WY"] <- as.integer(sapply(df[,c(datecolumn)], wtr_yr))
  df["DOWY"] <- as.integer(sapply(df[,c(datecolumn)], dowy))
  return(df)
  
}

# add Water year and Water year dat
cdec_clean_df <- cdec_clean_df %>% 
  add_WYD(., "date")

names(cdec_clean_df)

# now group and average by water day
cdec_model_data <- cdec_clean_df %>% 
  group_by(station_id, DOWY) %>% 
  summarize(mean_temp_C = mean(value_mean_C, na.rm = T))

# To join with month and month day make a clean Join table for DOWY that includes the month and month day
#cdec_clean_df_monthdays <- cdec_clean_df %>% distinct(month, month_day, DOWY)

# cdec_model_data <- cdec_model_data %>% 
#   left_join(., cdec_clean_df_monthdays[,c("month","month_day", "DOWY")], by="DOWY")

# Plot cdec model data ----------------------------------------------------

# VIEW!
ggplotly(
  ggplot() + 
    geom_line(data=cdec_model_data, aes(x=DOWY, y=mean_temp_C, color=station_id), show.legend = F) +
    scale_color_viridis_d())



# SAVE DATA ---------------------------------------------------------------

write_csv(cdec_model_data, path = paste0("data/model_data/cdec_model_data.csv"))

write_rds(cdec_model_data, path = paste0("data/model_data/cdec_model_data.rds"))

#Repeat code for usgs stations

# Compile usgs multi-year data into single record -------------------------

# specify the file ending more explicitly to make this more flexible:
usgs_site <- list.files(path = "data/QA_data", 
                        pattern = "usgs_*(.*)rds$", ignore.case = TRUE)

# add ignore case
usgs_site_w_path <- list.files("data/QA_data","usgs_*(.*)rds$", 
                               full.names = TRUE, ignore.case = TRUE)

# now loop through and read in the files
usgs_dfs <- map(usgs_site_w_path, ~read_rds(.x)) %>%
  bind_rows() %>% 
  filter(!is.na(value_mean_C))

# check for NAs
summary(usgs_dfs)


# lets see what sites are hourly/datetime and filter those

# ADW - all files fixed. Commented out these lines
# usgs_hourly <- usgs_dfs %>%
#   filter(!is.na(site_no))
# 
# # what stations are being/need to be fixed?
# usgs_hourly %>% 
#   group_by(site_no) %>% 
#   tally()

# create df of clean usgs data

usgs_clean_df <- usgs_dfs %>% 
  add_WYD(., "date")

names(usgs_clean_df)

# now group and average by water day
usgs_model_data <- usgs_clean_df %>% 
  group_by(station_id, DOWY) %>% 
  summarize(mean_temp_C = mean(value_mean_C, na.rm = T))

write_csv(usgs_model_data, path = paste0("data/model_data/usgs_model_data.csv"))
write_rds(usgs_model_data, path = paste0("data/model_data/usgs_model_data.rds"))

# Plot usgs model data ----------------------------------------------------

# VIEW!
ggplotly(
  ggplot() + 
    geom_line(data=usgs_model_data, aes(x=DOWY, y=mean_temp_C, color=station_id), show.legend = F) +
    scale_color_viridis_d())

# looks good! bind to cdec data and save master model data file

# Add Shasta Data ---------------------------------------------------------

shasta_site <- list.files(path = "data/QA_data", 
                        pattern = "shasta_*(.*)rds$", ignore.case = TRUE)

# add ignore case
shasta_site_w_path <- list.files("data/QA_data","shasta_*(.*)rds$", 
                               full.names = TRUE, ignore.case = TRUE)

# now loop through and read in the files
shasta_dfs <- map(shasta_site_w_path, ~read_rds(.x)) %>%
  bind_rows() %>% 
  filter(!is.na(value_mean_C)) #%>% 
#  rename(station_id = site_id)

# check for NAs
summary(shasta_dfs)

# add Water year and Water year dat
shasta_clean_df <- shasta_dfs %>% 
  add_WYD(., "date")

# now group and average by water day
shasta_model_data <- shasta_clean_df %>% 
  group_by(station_id, DOWY) %>% 
  summarize(mean_temp_C = mean(value_mean_C, na.rm = T)) %>% 
  select(station_id, DOWY, mean_temp_C)

# save out
write_csv(shasta_model_data, path = paste0("data/model_data/shasta_model_data.csv"))
write_rds(shasta_model_data, path = paste0("data/model_data/shasta_model_data.rds"))

# Combine datasets, plot, and save ----------------------------------------

#cdec_model_data <- read_csv("data/model_data/all_cdec_sites_model_data.csv")

all_sites_model_data <- bind_rows(cdec_model_data, usgs_model_data, shasta_model_data) 

summary(all_sites_model_data)
table(all_sites_model_data$station_id)

# save out
write_csv(all_sites_model_data, path = paste0("data/model_data/all_sites_model_data.csv"))
write_rds(all_sites_model_data, path = paste0("data/model_data/all_sites_model_data.rds"))

#Final step: plot all sites together
ggplotly(
  ggplot() + 
    geom_line(data=all_sites_model_data, aes(x=DOWY, y=mean_temp_C, color=station_id), show.legend = F) +
    scale_color_viridis_d())
