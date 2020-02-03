
# Code description --------------------------------------------------------

# Code to review temperature data from gage USGS_11261500.

library(tidyverse)
library(lubridate)
library(weathermetrics)
library(plotly)

# Get Data ----------------------------------------------------------------

file_list <- list.files("data/data_review/", pattern = ".rds")

# read in next file:
file_list[[76]]

usgs_daily_11261500 <- read_rds(path = paste0("data/data_review/",file_list[[76]]))

# Plot --------------------------------------------------------------------

#plot all data to look for obvious bad data points and gaps
ggplotly(
  ggplot() + geom_point(data=usgs_daily_11261500[,], aes(x=date, y=value_mean_C)))

# now make an interactive plot of first 1000 values
ggplotly(
  ggplot() + geom_point(data=usgs_daily_11261500[1:1000,], aes(x=date, y=value_mean_C)))

# Review, QA, and Repeat --------------------------------------------------
usgs_daily_11261500_QA <- usgs_daily_11261500[1:1000,]

ggplotly(
  ggplot() + geom_point(data=usgs_daily_11261500[1001:2000,], aes(x=date, y=value_mean_C)))

usgs_daily_11261500_QA <- rbind(usgs_daily_11261500_QA, usgs_daily_11261500[1001:2000,])

ggplotly(
  ggplot() + geom_point(data=usgs_daily_11261500[2001:3000,], aes(x=date, y=value_mean_C)))

usgs_daily_11261500_QA <- rbind(usgs_daily_11261500_QA, usgs_daily_11261500[2001:3000,])

ggplotly(
  ggplot() + geom_point(data=usgs_daily_11261500[3001:4000,], aes(x=date, y=value_mean_C)))

usgs_daily_11261500_QA <- rbind(usgs_daily_11261500_QA, usgs_daily_11261500[3001:4000,])

ggplotly(
  ggplot() + geom_point(data=usgs_daily_11261500[4001:5000,], aes(x=date, y=value_mean_C)))

usgs_daily_11261500_QA <- rbind(usgs_daily_11261500_QA, usgs_daily_11261500[4001:5000,])

ggplotly(
  ggplot() + geom_point(data=usgs_daily_11261500[5001:6000,], aes(x=date, y=value_mean_C)))

usgs_daily_11261500_QA <- rbind(usgs_daily_11261500_QA, usgs_daily_11261500[5001:6000,])

ggplotly(
  ggplot() + geom_point(data=usgs_daily_11261500[6001:7123,], aes(x=date, y=value_mean_C)))

usgs_daily_11261500_QA <- rbind(usgs_daily_11261500_QA, usgs_daily_11261500[6001:7123,])

# Final review ------------------------------------------------------------

#plot QA'd dataset to confirm all points look good
ggplotly(
  ggplot() +geom_point(data = usgs_daily_11261500_QA, aes(x=date, y=value_mean_C)))

#save QA'd dataset as a .rds file
write_rds(usgs_daily_11261500_QA, path = "data/QA_data/usgs_daily_11261500_QA.rds")

#update the gage_QA_progress
gage_QA_progress <- read_csv("data/data_review/gage_QA_progress.csv")

#note reviewer initials, whether review is complete, and any final notes
gage_QA_progress[gage_QA_progress$site_id=="11261500",6:8] <- c("ADW", "Y", "QA complete")

#save updated dataframe to the .csv
write_csv(gage_QA_progress, path = "data/data_review/gage_QA_progress.csv")
