
# Code description --------------------------------------------------------

# Code to review temperature data from gage USGS_10265150.

library(tidyverse)
library(lubridate)
library(weathermetrics)
library(plotly)

# Get Data ----------------------------------------------------------------

file_list <- list.files("data/data_review/", pattern = ".rds")

# read in next file:
file_list[[66]]

usgs_daily_10265150 <- read_rds(path = paste0("data/data_review/",file_list[[66]]))

# Plot --------------------------------------------------------------------

#plot all data to look for obvious bad data points and gaps
ggplotly(
  ggplot() + geom_point(data=usgs_daily_10265150[,], aes(x=date, y=value_mean_C)))

# now make an interactive plot of first 1000 values
ggplotly(
  ggplot() + geom_point(data=usgs_daily_10265150[1:1000,], aes(x=date, y=value_mean_C)))

# Review, QA, and Repeat --------------------------------------------------
usgs_daily_10265150_1_1000 <- usgs_daily_10265150[1:1000,] %>% 
  filter(date != "1986-03-08", date != "1986-03-09", date != "1986-03-10", date != "1986-03-11", date != "1986-02-19", date != "1986-02-18", date != "1986-02-15", date != "1986-02-20", date != "1986-02-17", date != "1986-02-21", date != "1986-02-16", date != "1986-02-14", date != "1985-09-11")

ggplotly(
  ggplot() + geom_point(data=usgs_daily_10265150_1_1000, aes(x=date, y=value_mean_C)))

usgs_daily_10265150_QA <- usgs_daily_10265150_1_1000

ggplotly(
  ggplot() + geom_point(data=usgs_daily_10265150[1001:2000,], aes(x=date, y=value_mean_C)))

usgs_daily_10265150_1001_2000 <- usgs_daily_10265150[1001:2000,] %>% 
  filter(date != "1988-05-29")

usgs_daily_10265150_1001_2000 <- usgs_daily_10265150_1001_2000[-c(350:387),]

ggplotly(
  ggplot() + geom_point(data=usgs_daily_10265150_1001_2000, aes(x=date, y=value_mean_C)))

usgs_daily_10265150_QA <- rbind(usgs_daily_10265150_QA, usgs_daily_10265150_1001_2000)

ggplotly(
  ggplot() + geom_point(data=usgs_daily_10265150[2001:3000,], aes(x=date, y=value_mean_C)))

usgs_daily_10265150_QA <- rbind(usgs_daily_10265150_QA, usgs_daily_10265150[2001:3000,])

ggplotly(
  ggplot() + geom_point(data=usgs_daily_10265150[3001:4000,], aes(x=date, y=value_mean_C)))

usgs_daily_10265150_3001_4000 <- usgs_daily_10265150[3001:4000,] %>% 
  filter(value_mean_C > 25.3)

ggplotly(
  ggplot() + geom_point(data=usgs_daily_10265150_3001_4000, aes(x=date, y=value_mean_C)))

usgs_daily_10265150_QA <- rbind(usgs_daily_10265150_QA, usgs_daily_10265150_3001_4000)

ggplotly(
  ggplot() + geom_point(data=usgs_daily_10265150[4001:5000,], aes(x=date, y=value_mean_C)))

usgs_daily_10265150_QA <- rbind(usgs_daily_10265150_QA, usgs_daily_10265150[4001:5000,])

ggplotly(
  ggplot() + geom_point(data=usgs_daily_10265150[5001:6000,], aes(x=date, y=value_mean_C)))

usgs_daily_10265150_QA <- rbind(usgs_daily_10265150_QA, usgs_daily_10265150[5001:6000,])
# Final review ------------------------------------------------------------

#plot QA'd dataset to confirm all points look good
ggplotly(
  ggplot() +geom_point(data = usgs_daily_10265150_QA, aes(x=date, y=value_mean_C)))

#save QA'd dataset as a .rds file
write_rds(usgs_daily_10265150_QA, path = "data/QA_data/usgs_daily_10265150_QA.rds")

#update the gage_QA_progress
gage_QA_progress <- read_csv("data/data_review/gage_QA_progress.csv")

#note reviewer initials, whether review is complete, and any final notes
gage_QA_progress[gage_QA_progress$site_id=="10265150",6:8] <- c("ADW", "Y", "QA complete")

#save updated dataframe to the .csv
write_csv(gage_QA_progress, path = "data/data_review/gage_QA_progress.csv")
