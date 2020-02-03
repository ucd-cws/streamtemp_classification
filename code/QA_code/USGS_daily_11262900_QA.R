
# Code description --------------------------------------------------------

# Code to review temperature data from gage USGS_11262900.

library(tidyverse)
library(lubridate)
library(weathermetrics)
library(plotly)

# Get Data ----------------------------------------------------------------

file_list <- list.files("data/data_review/", pattern = ".rds")

# read in next file:
file_list[[77]]

usgs_daily_11262900 <- read_rds(path = paste0("data/data_review/",file_list[[77]]))

# Plot --------------------------------------------------------------------

#plot all data to look for obvious bad data points and gaps
ggplotly(
  ggplot() + geom_point(data=usgs_daily_11262900[,], aes(x=date, y=value_mean_C)))

# now make an interactive plot of first 1000 values
ggplotly(
  ggplot() + geom_point(data=usgs_daily_11262900[1:1000,], aes(x=date, y=value_mean_C)))

# Review, QA, and Repeat --------------------------------------------------
usgs_daily_11262900_QA <- usgs_daily_11262900[1:1000,]

ggplotly(
  ggplot() + geom_point(data=usgs_daily_11262900[1001:2000,], aes(x=date, y=value_mean_C)))

usgs_daily_11262900_QA <- usgs_daily_11262900[1:2000,]

ggplotly(
  ggplot() + geom_point(data=usgs_daily_11262900[2001:3000,], aes(x=date, y=value_mean_C)))

usgs_daily_11262900_QA <- usgs_daily_11262900[1:3000,]

ggplotly(
  ggplot() + geom_point(data=usgs_daily_11262900[3001:4000,], aes(x=date, y=value_mean_C)))

usgs_daily_11262900_QA <- usgs_daily_11262900[1:4000,]

ggplotly(
  ggplot() + geom_point(data=usgs_daily_11262900[4001:5000,], aes(x=date, y=value_mean_C)))

usgs_daily_11262900_QA <- usgs_daily_11262900[1:5000,]

ggplotly(
  ggplot() + geom_point(data=usgs_daily_11262900[5001:6000,], aes(x=date, y=value_mean_C)))

usgs_daily_11262900_QA <- usgs_daily_11262900[1:6000,]

ggplotly(
  ggplot() + geom_point(data=usgs_daily_11262900[6001:7000,], aes(x=date, y=value_mean_C)))

usgs_daily_11262900_QA <- usgs_daily_11262900[1:7000,]

ggplotly(
  ggplot() + geom_point(data=usgs_daily_11262900[7001:8000,], aes(x=date, y=value_mean_C)))

usgs_daily_11262900_QA <- usgs_daily_11262900[1:8000,]

ggplotly(
  ggplot() + geom_point(data=usgs_daily_11262900[8001:9000,], aes(x=date, y=value_mean_C)))

usgs_daily_11262900_QA <- usgs_daily_11262900[1:9000,]

ggplotly(
  ggplot() + geom_point(data=usgs_daily_11262900[9001:10000,], aes(x=date, y=value_mean_C)))

usgs_daily_11262900_QA <- rbind(usgs_daily_11262900_QA,usgs_daily_11262900[9001:10000,])

ggplotly(
  ggplot() + geom_point(data=usgs_daily_11262900[10001:10816,], aes(x=date, y=value_mean_C)))

usgs_daily_11262900_QA <- rbind(usgs_daily_11262900_QA, usgs_daily_11262900[10001:10816,])

# Final review ------------------------------------------------------------

#plot QA'd dataset to confirm all points look good
ggplotly(
  ggplot() +geom_point(data = usgs_daily_11262900_QA, aes(x=date, y=value_mean_C)))

#save QA'd dataset as a .rds file
write_rds(usgs_daily_11262900_QA, path = "data/QA_data/usgs_daily_11262900_QA.rds")

#update the gage_QA_progress
gage_QA_progress <- read_csv("data/data_review/gage_QA_progress.csv")

#note reviewer initials, whether review is complete, and any final notes
gage_QA_progress[gage_QA_progress$site_id=="11262900",6:8] <- c("ADW", "Y", "QA complete")

#save updated dataframe to the .csv
write_csv(gage_QA_progress, path = "data/data_review/gage_QA_progress.csv")
