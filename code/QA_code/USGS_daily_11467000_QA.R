
# Code description --------------------------------------------------------

# Code to review temperature data from gage usgs_11467000.

library(tidyverse)
library(lubridate)
library(weathermetrics)
library(plotly)

# Get Data ----------------------------------------------------------------

file_list <- list.files("data/data_review/")

# read in next file:
file_list[[94]]

usgs_daily_11467000 <- read_rds(path = paste0("data/data_review/",file_list[[94]]))


# Plot --------------------------------------------------------------------

# now make an interactive plot of first 1000 values
ggplotly(
  ggplot() + geom_point(data=usgs_daily_11467000[1:1000,], aes(x=date, y=value_mean_C)))

# Review, QA, and Repeat --------------------------------------------------

usgs_daily_11467000_QA <- usgs_daily_11467000[1:1000,]

ggplotly(
  ggplot() + geom_point(data=usgs_daily_11467000[1001:2000,], aes(x=date, y=value_mean_C)))

usgs_daily_11467000_QA <- usgs_daily_11467000[1:2000,]

ggplotly(
  ggplot() + geom_point(data=usgs_daily_11467000[2001:3000,], aes(x=date, y=value_mean_C)))

usgs_daily_11467000_QA <- usgs_daily_11467000[1:3000,]

ggplotly(
  ggplot() + geom_point(data=usgs_daily_11467000[3001:3965,], aes(x=date, y=value_mean_C)))

usgs_daily_11467000_QA <- usgs_daily_11467000[1:3965,]

# Final review ------------------------------------------------------------

#plot QA'd dataset to confirm all points look good
ggplotly(
  ggplot() +geom_point(data = usgs_daily_11467000_QA, aes(x=date, y=value_mean_C)))

# Data is not continuous; gap exists from 09-2010 until 10-2016; however, we still have enough data for the analysis, so we will keep the site.

# #save QA'd dataset as a .rds file
write_rds(usgs_daily_11467000_QA, path = "data/QA_data/usgs_daily_11467000_QA.rds")

#update the gage_QA_progress
gage_QA_progress <- read_csv("data/data_review/gage_QA_progress.csv")

#note reviewer initials, whether review is complete, and any final notes
gage_QA_progress[gage_QA_progress$site_id=="11467000",6:8] <- c("ADW", "Y", "QA complete")

#save updated dataframe to the .csv
write_csv(gage_QA_progress, path = "data/data_review/gage_QA_progress.csv")
