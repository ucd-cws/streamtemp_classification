
# Code description --------------------------------------------------------

# Code to review temperature data from gage usgs_11390500.

library(tidyverse)
library(lubridate)
library(weathermetrics)
library(plotly)
library(dataRetrieval)

# Get Data ----------------------------------------------------------------

file_list <- list.files("data/data_review/")

# read in next file:
file_list[[89]]

usgs_daily_11390500 <- read_rds(path = paste0("data/data_review/",file_list[[89]]))


# Plot --------------------------------------------------------------------

# now make an interactive plot of first 1000 values
ggplotly(
  ggplot() + geom_point(data=usgs_daily_11390500[1:1000,], aes(x=date, y=value_mean_C)))

# Review, QA, and Repeat --------------------------------------------------

usgs_daily_11390500_QA <- usgs_daily_11390500[1:1000,]

ggplotly(
  ggplot() + geom_point(data=usgs_daily_11390500[1001:2000,], aes(x=date, y=value_mean_C)))

usgs_daily_11390500_QA <- usgs_daily_11390500[1:2000,]

ggplotly(
  ggplot() + geom_point(data=usgs_daily_11390500[2001:3000,], aes(x=date, y=value_mean_C)))

usgs_daily_11390500_QA <- usgs_daily_11390500[1:3000,]

ggplotly(
  ggplot() + geom_point(data=usgs_daily_11390500[3001:4000,], aes(x=date, y=value_mean_C)))

#Big data gap from 1998 to 2016, but enough data to complete the analysis; gage will remain in study

usgs_daily_11390500_QA <- usgs_daily_11390500[1:4000,]

ggplotly(
  ggplot() + geom_point(data=usgs_daily_11390500[4001:4318,], aes(x=date, y=value_mean_C)))

usgs_daily_11390500_QA <- usgs_daily_11390500[1:4318,]


# Final review ------------------------------------------------------------

# plot QA'd dataset to confirm all points look good
ggplotly(
  ggplot() +geom_point(data = usgs_daily_11390500_QA, aes(x=date, y=value_mean_C)))

#save QA'd dataset as a .rds file
write_rds(usgs_daily_11390500_QA, path = "data/QA_data/usgs_daily_11390500_QA.rds")

#update the gage_QA_progress
gage_QA_progress <- read_csv("data/data_review/gage_QA_progress.csv")

#confirm correct row to update by the site_id
#gage_QA_progress[190,1]

#note reviewer initials, whether review is complete, and any final notes
gage_QA_progress[gage_QA_progress$site_id=="11390500",6:8] <- c("ADW", "Y", "QA complete")

#save updated dataframe to the .csv
write_csv(gage_QA_progress, path = "data/data_review/gage_QA_progress.csv")
