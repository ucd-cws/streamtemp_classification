
# Code description --------------------------------------------------------

# Code to review temperature data from gage CDEC_DVD.

library(tidyverse)
library(lubridate)
library(weathermetrics)
library(plotly)

# Get Data ----------------------------------------------------------------

file_list <- list.files("data/data_review/")

# read in next file:
file_list[[18]]

cdec_daily_DVD <- read_rds(path = paste0("data/data_review/",file_list[[18]]))

# Plot --------------------------------------------------------------------

#plot all data to look for obvious bad data points and gaps
ggplotly(
  ggplot() + geom_point(data=cdec_daily_DVD[,], aes(x=date, y=value_mean_C)))

# now make an interactive plot of first 1000 values
ggplotly(
  ggplot() + geom_point(data=cdec_daily_DVD[1:1000,], aes(x=date, y=value_mean_C)))

# Review, QA, and Repeat --------------------------------------------------
cdec_daily_DVD_1_1000 <- cdec_daily_DVD[1:1000,] %>% 
  filter(value_mean_C > 0, date != "1998-09-22", date != "1999-04-18", date != "2000-10-30", date != "2000-10-25", date != "2000-10-26")

ggplotly(
  ggplot() + geom_point(data=cdec_daily_DVD_1_1000, aes(x=date, y=value_mean_C)))

cdec_daily_DVD_QA <- cdec_daily_DVD_1_1000

ggplotly(
  ggplot() + geom_point(data=cdec_daily_DVD[1001:2000,], aes(x=date, y=value_mean_C)))

cdec_daily_DVD_1001_2000 <- cdec_daily_DVD[1001:2000,] %>% 
  filter(date != "2004-01-06")

ggplotly(
  ggplot() + geom_point(data=cdec_daily_DVD_1001_2000, aes(x=date, y=value_mean_C)))

cdec_daily_DVD_QA <- rbind(cdec_daily_DVD_QA, cdec_daily_DVD_1001_2000)

ggplotly(
  ggplot() + geom_point(data=cdec_daily_DVD[2001:3000,], aes(x=date, y=value_mean_C)))

cdec_daily_DVD_2001_3000 <- cdec_daily_DVD[2001:3000,] %>% 
  filter(value_mean_C > 3.38)

ggplotly(
  ggplot() + geom_point(data=cdec_daily_DVD_2001_3000, aes(x=date, y=value_mean_C)))

cdec_daily_DVD_QA <- rbind(cdec_daily_DVD_QA, cdec_daily_DVD_2001_3000)

ggplotly(
  ggplot() + geom_point(data=cdec_daily_DVD[3001:4000,], aes(x=date, y=value_mean_C)))

cdec_daily_DVD_QA <- rbind(cdec_daily_DVD_QA, cdec_daily_DVD[3001:4000,])

ggplotly(
  ggplot() + geom_point(data=cdec_daily_DVD[4001:5000,], aes(x=date, y=value_mean_C)))

cdec_daily_DVD_4001_5000 <- cdec_daily_DVD[4001:5000,] %>% 
  filter(value_mean_C > 1.84)

ggplotly(
  ggplot() + geom_point(data=cdec_daily_DVD_4001_5000, aes(x=date, y=value_mean_C)))

cdec_daily_DVD_QA <- rbind(cdec_daily_DVD_QA, cdec_daily_DVD_4001_5000)

ggplotly(
  ggplot() + geom_point(data=cdec_daily_DVD[5001:6000,], aes(x=date, y=value_mean_C)))

cdec_daily_DVD_QA <- rbind(cdec_daily_DVD_QA, cdec_daily_DVD[5001:6000,])

ggplotly(
  ggplot() + geom_point(data=cdec_daily_DVD[6001:7000,], aes(x=date, y=value_mean_C)))

cdec_daily_DVD_6001_7000 <- cdec_daily_DVD[6001:7000,] %>% 
  filter(date != "2017-09-12")

ggplotly(
  ggplot() + geom_point(data=cdec_daily_DVD_6001_7000, aes(x=date, y=value_mean_C)))

cdec_daily_DVD_QA <- rbind(cdec_daily_DVD_QA, cdec_daily_DVD_6001_7000)

ggplotly(
  ggplot() + geom_point(data=cdec_daily_DVD[7001:7642,], aes(x=date, y=value_mean_C)))

cdec_daily_DVD_QA <- rbind(cdec_daily_DVD_QA, cdec_daily_DVD[7001:7642,])

# Final review ------------------------------------------------------------

#plot QA'd dataset to confirm all points look good
ggplotly(
  ggplot() +geom_point(data = cdec_daily_DVD_QA, aes(x=date, y=value_mean_C)))

#save QA'd dataset as a .rds file
write_rds(cdec_daily_DVD_QA, path = "data/QA_data/cdec_daily_DVD_QA.rds")

#update the gage_QA_progress
gage_QA_progress <- read_csv("data/data_review/gage_QA_progress.csv")

#note reviewer initials, whether review is complete, and any final notes
gage_QA_progress[gage_QA_progress$site_id=="DVD",4:6] <- c("ADW", "Y", "QA complete")

#save updated dataframe to the .csv
write_csv(gage_QA_progress, path = "data/data_review/gage_QA_progress.csv")
