
# Code description --------------------------------------------------------

# Code to review temperature data from gage CDEC_GVO.

library(tidyverse)
library(lubridate)
library(weathermetrics)
library(plotly)

# Get Data ----------------------------------------------------------------

file_list <- list.files("data/data_review/")

# read in next file:
file_list[[26]]

cdec_daily_GVO <- read_rds(path = paste0("data/data_review/",file_list[[26]]))

# Plot --------------------------------------------------------------------

#plot all data to look for obvious bad data points and gaps
ggplotly(
  ggplot() + geom_point(data=cdec_daily_GVO[,], aes(x=date, y=value_mean_C)))

# now make an interactive plot of first 1000 values
ggplotly(
  ggplot() + geom_point(data=cdec_daily_GVO[1:1000,], aes(x=date, y=value_mean_C)))

# Review, QA, and Repeat --------------------------------------------------

cdec_daily_GVO_1_1000 <- cdec_daily_GVO[1:1000,]

cdec_daily_GVO_1_1000 <- cdec_daily_GVO_1_1000[-c(817:832),]

ggplotly(
  ggplot() + geom_point(data=cdec_daily_GVO_1_1000, aes(x=date, y=value_mean_C)))

cdec_daily_GVO_QA <- cdec_daily_GVO_1_1000

ggplotly(
  ggplot() + geom_point(data=cdec_daily_GVO[1001:2000,], aes(x=date, y=value_mean_C)))

cdec_daily_GVO_QA <- rbind(cdec_daily_GVO_QA, cdec_daily_GVO[1001:2000,])

ggplotly(
  ggplot() + geom_point(data=cdec_daily_GVO[2001:3000,], aes(x=date, y=value_mean_C)))

cdec_daily_GVO_QA <- rbind(cdec_daily_GVO_QA, cdec_daily_GVO[2001:3000,])

ggplotly(
  ggplot() + geom_point(data=cdec_daily_GVO[3001:4000,], aes(x=date, y=value_mean_C)))

cdec_daily_GVO_QA <- rbind(cdec_daily_GVO_QA, cdec_daily_GVO[3001:4000,])

ggplotly(
  ggplot() + geom_point(data=cdec_daily_GVO[4001:5000,], aes(x=date, y=value_mean_C)))

cdec_daily_GVO_4001_5000 <- cdec_daily_GVO[4001:5000,] %>% 
  filter(date != "2013-05-21", date != "2013-05-22", date != "2013-05-23")

ggplotly(
  ggplot() + geom_point(data=cdec_daily_GVO_4001_5000, aes(x=date, y=value_mean_C)))

cdec_daily_GVO_QA <- rbind(cdec_daily_GVO_QA, cdec_daily_GVO_4001_5000)

ggplotly(
  ggplot() + geom_point(data=cdec_daily_GVO[5001:6000,], aes(x=date, y=value_mean_C)))

cdec_daily_GVO_5001_6000 <- cdec_daily_GVO[5001:6000,] %>% 
  filter(date != "2015-12-31", date != "2016-01-01", date != "2016-01-02")

ggplotly(
  ggplot() + geom_point(data=cdec_daily_GVO_5001_6000, aes(x=date, y=value_mean_C)))

cdec_daily_GVO_QA <- rbind(cdec_daily_GVO_QA, cdec_daily_GVO_5001_6000)

ggplotly(
  ggplot() + geom_point(data=cdec_daily_GVO[6001:7243,], aes(x=date, y=value_mean_C)))

cdec_daily_GVO_QA <- rbind(cdec_daily_GVO_QA, cdec_daily_GVO[6001:7243,])
# Final review ------------------------------------------------------------

#plot QA'd dataset to confirm all points look good
ggplotly(
  ggplot() +geom_point(data = cdec_daily_GVO_QA, aes(x=date, y=value_mean_C)))

#save QA'd dataset as a .rds file
write_rds(cdec_daily_GVO_QA, path = "data/QA_data/cdec_daily_GVO_QA.rds")

#update the gage_QA_progress
gage_QA_progress <- read_csv("data/data_review/gage_QA_progress.csv")

#note reviewer initials, whether review is complete, and any final notes
gage_QA_progress[gage_QA_progress$site_id=="GVO",4:6] <- c("ADW", "Y", "QA complete")

#save updated dataframe to the .csv
write_csv(gage_QA_progress, path = "data/data_review/gage_QA_progress.csv")
