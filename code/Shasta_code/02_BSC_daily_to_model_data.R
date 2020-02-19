
# Code description --------------------------------------------------------

# This code processes the daily average time series into the compiled model dataset for the Big Springs Creek sites. Model data is appended to all_sites_model_data so it can be processed by the existing code 09_fit_models_to_sites and then merged into main workflow and analysis.


# Libraries ---------------------------------------------------------------

library(tidyverse)
library(lubridate)
library(plotly)
#library(wateRshedTools) still can't figure out why I can't get this from Ryan's github. Tried running devtools::install_github("ryanpeek/wateRshedTools")

# Ryan's wateRshedTools functions -----------------------------------------

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




# Load data ---------------------------------------------------------------

all_sites_model_data <- read_rds("data/model_data/all_sites_model_data.rds")

BSC_dam_QA <- read_csv("data/Shasta/Shasta_QA/BSC_dam_daily_QA.csv")
BSC_mouth_QA <- read_csv("data/Shasta/Shasta_QA/BSC_mouth_daily_QA.csv")


# Compile Shasta data into model datasets ---------------------------------

# give the Shasta model dataframe the same column names as the all_sites_model_data so I can rbind the two sets together at the end.

BSC_dam_model_data <- BSC_dam_QA %>% 
  add_WYD(., "date") %>%
  mutate(station_id = "BSC_dam") %>% 
  group_by(DOWY, station_id) %>% 
  summarize(mean_temp_C = mean(value_mean_C)) %>% 
  select(station_id, DOWY, mean_temp_C)

all_sites_model_data <- rbind(all_sites_model_data, BSC_dam_model_data)

BSC_mouth_model_data <- BSC_mouth_QA %>% 
  add_WYD(., "date") %>%
  mutate(station_id = "BSC_mouth") %>% 
  group_by(DOWY, station_id) %>% 
  summarize(mean_temp_C = mean(value_mean_C)) %>% 
  select(station_id, DOWY, mean_temp_C)

all_sites_model_data <- rbind(all_sites_model_data, BSC_mouth_model_data)


# Plot all model data -----------------------------------------------------

ggplotly(
  ggplot() + 
    geom_line(data=all_sites_model_data, aes(x=DOWY, y=mean_temp_C, color=station_id), show.legend = F) +
    scale_color_viridis_d())


# Save updated all_site_model_data file as rds and csv --------------------

write_csv(all_sites_model_data, path = paste0("data/model_data/all_sites_model_data.csv"))
write_rds(all_sites_model_data, path = paste0("data/model_data/all_sites_model_data.rds"))
