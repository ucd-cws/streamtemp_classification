
# Code description --------------------------------------------------------

# Code to review temperature data from gage USGS_11261100.

library(tidyverse)
library(lubridate)
library(weathermetrics)
library(plotly)

# Get Data ----------------------------------------------------------------

file_list <- list.files("data/data_review/", pattern = ".rds")

# read in next file:
file_list[[75]]

usgs_daily_11261100 <- read_rds(path = paste0("data/data_review/",file_list[[75]]))

# Plot --------------------------------------------------------------------

#plot all data to look for obvious bad data points and gaps
ggplotly(
  ggplot() + geom_point(data=usgs_daily_11261100[,], aes(x=date, y=value_mean_C)))

# now make an interactive plot of first 1000 values
ggplotly(
  ggplot() + geom_point(data=usgs_daily_11261100[1:1000,], aes(x=date, y=value_mean_C)))

# Review, QA, and Repeat --------------------------------------------------
usgs_daily_11261100_1_1000 <- usgs_daily_11261100[1:1000,] %>% 
  filter(date != "1986-05-08")

ggplotly(
  ggplot() + geom_point(data=usgs_daily_11261100_1_1000, aes(x=date, y=value_mean_C)))

usgs_daily_11261100_QA <- usgs_daily_11261100_1_1000

ggplotly(
  ggplot() + geom_point(data=usgs_daily_11261100[1001:2000,], aes(x=date, y=value_mean_C)))

usgs_daily_11261100_1001_2000 <- usgs_daily_11261100[1001:2000,] %>% 
  filter(date != "1990-12-22", date != "1990-12-23", date != "1990-12-24", date != "1990-12-25", date != "1990-12-26", date != "1990-12-27")

ggplotly(
  ggplot() + geom_point(data=usgs_daily_11261100_1001_2000, aes(x=date, y=value_mean_C)))

usgs_daily_11261100_QA <- rbind(usgs_daily_11261100_QA, usgs_daily_11261100_1001_2000)

ggplotly(
  ggplot() + geom_point(data=usgs_daily_11261100[2001:3000,], aes(x=date, y=value_mean_C)))

usgs_daily_11261100_QA <- rbind(usgs_daily_11261100_QA, usgs_daily_11261100[2001:3000,])

ggplotly(
  ggplot() + geom_point(data=usgs_daily_11261100[3001:4000,], aes(x=date, y=value_mean_C)))

usgs_daily_11261100_QA <- rbind(usgs_daily_11261100_QA, usgs_daily_11261100[3001:4000,])

ggplotly(
  ggplot() + geom_point(data=usgs_daily_11261100[4001:5000,], aes(x=date, y=value_mean_C)))

usgs_daily_11261100_QA <- rbind(usgs_daily_11261100_QA, usgs_daily_11261100[4001:5000,])

ggplotly(
  ggplot() + geom_point(data=usgs_daily_11261100[5001:6000,], aes(x=date, y=value_mean_C)))

usgs_daily_11261100_QA <- rbind(usgs_daily_11261100_QA, usgs_daily_11261100[5001:6000,])

ggplotly(
  ggplot() + geom_point(data=usgs_daily_11261100[6001:7000,], aes(x=date, y=value_mean_C)))

usgs_daily_11261100_QA <- rbind(usgs_daily_11261100_QA, usgs_daily_11261100[6001:7000,])

ggplotly(
  ggplot() + geom_point(data=usgs_daily_11261100[7001:8000,], aes(x=date, y=value_mean_C)))

usgs_daily_11261100_QA <- rbind(usgs_daily_11261100_QA, usgs_daily_11261100[7001:8000,])

ggplotly(
  ggplot() + geom_point(data=usgs_daily_11261100[8001:9000,], aes(x=date, y=value_mean_C)))

usgs_daily_11261100_QA <- rbind(usgs_daily_11261100_QA, usgs_daily_11261100[8001:9000,])

ggplotly(
  ggplot() + geom_point(data=usgs_daily_11261100[9001:10000,], aes(x=date, y=value_mean_C)))

usgs_daily_11261100_QA <- rbind(usgs_daily_11261100_QA, usgs_daily_11261100[9001:10000,])

ggplotly(
  ggplot() + geom_point(data=usgs_daily_11261100[10001:11046,], aes(x=date, y=value_mean_C)))


usgs_daily_11261100_QA <- rbind(usgs_daily_11261100_QA, usgs_daily_11261100[10001:11046,])

# Final review ------------------------------------------------------------

#plot QA'd dataset to confirm all points look good
ggplotly(
  ggplot() +geom_point(data = usgs_daily_11261100_QA, aes(x=date, y=value_mean_C)))

#save QA'd dataset as a .rds file
write_rds(usgs_daily_11261100_QA, path = "data/QA_data/usgs_daily_11261100_QA.rds")

#update the gage_QA_progress
gage_QA_progress <- read_csv("data/data_review/gage_QA_progress.csv")

#note reviewer initials, whether review is complete, and any final notes
gage_QA_progress[gage_QA_progress$site_id=="11261100",6:8] <- c("ADW", "Y", "QA complete")

#save updated dataframe to the .csv
write_csv(gage_QA_progress, path = "data/data_review/gage_QA_progress.csv")
