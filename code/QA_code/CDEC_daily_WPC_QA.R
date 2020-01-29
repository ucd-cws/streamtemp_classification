
# Code description --------------------------------------------------------

# Code to review temperature data from gage CDEC_WPC.

library(tidyverse)
library(lubridate)
library(weathermetrics)
library(plotly)

# Get Data ----------------------------------------------------------------

file_list <- list.files("data/data_review/", pattern = ".rds")

# read in next file:
file_list[[62]]

cdec_daily_WPC <- read_rds(path = paste0("data/data_review/",file_list[[62]]))

# Plot --------------------------------------------------------------------

#plot all data to look for obvious bad data points and gaps
ggplotly(
  ggplot() + geom_point(data=cdec_daily_WPC[,], aes(x=date, y=value_mean_C)))

# Insufficient data for full analysis. Site dropped from study. No data file exported.

# now make an interactive plot of first 1000 values
ggplotly(
  ggplot() + geom_point(data=cdec_daily_WPC[1:1000,], aes(x=date, y=value_mean_C)))

# Review, QA, and Repeat --------------------------------------------------
cdec_daily_WPC_QA <- cdec_daily_WPC[1:1000,]

ggplotly(
  ggplot() + geom_point(data=cdec_daily_WPC[1001:2000,], aes(x=date, y=value_mean_C)))

cdec_daily_WPC_QA <- rbind(cdec_daily_WPC_QA, cdec_daily_WPC[1001:2000,])

ggplotly(
  ggplot() + geom_point(data=cdec_daily_WPC[2001:3000,], aes(x=date, y=value_mean_C)))

cdec_daily_WPC_QA <- rbind(cdec_daily_WPC_QA, cdec_daily_WPC[2001:3000,])

ggplotly(
  ggplot() + geom_point(data=cdec_daily_WPC[3001:3673,], aes(x=date, y=value_mean_C)))

cdec_daily_WPC_QA <- rbind(cdec_daily_WPC_QA, cdec_daily_WPC[3001:3673,])
# Final review ------------------------------------------------------------

#plot QA'd dataset to confirm all points look good
ggplotly(
  ggplot() +geom_point(data = cdec_daily_WPC_QA, aes(x=date, y=value_mean_C)))

#save QA'd dataset as a .rds file
write_rds(cdec_daily_WPC_QA, path = "data/QA_data/cdec_daily_WPC_QA.rds")

#update the gage_QA_progress
gage_QA_progress <- read_csv("data/data_review/gage_QA_progress.csv")

#note reviewer initials, whether review is complete, and any final notes
gage_QA_progress[gage_QA_progress$site_id=="WPC",6:8] <- c("ADW", "Y", "QA complete")

#save updated dataframe to the .csv
write_csv(gage_QA_progress, path = "data/data_review/gage_QA_progress.csv")
