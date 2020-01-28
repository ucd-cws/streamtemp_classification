
# Code description --------------------------------------------------------

# Code to review temperature data from gage CDEC_MSS.

library(tidyverse)
library(lubridate)
library(weathermetrics)
library(plotly)

# Get Data ----------------------------------------------------------------

file_list <- list.files("data/data_review/", pattern = ".rds")

# read in next file:
file_list[[39]]

cdec_daily_MSS <- read_rds(path = paste0("data/data_review/",file_list[[39]]))

# Plot --------------------------------------------------------------------

#plot all data to look for obvious bad data points and gaps
ggplotly(
  ggplot() + geom_point(data=cdec_daily_MSS[,], aes(x=date, y=value_mean_C)))

# now make an interactive plot of first 1000 values
ggplotly(
  ggplot() + geom_point(data=cdec_daily_MSS[1:1000,], aes(x=date, y=value_mean_C)))

# Review, QA, and Repeat --------------------------------------------------

cdec_daily_MSS_1_1000 <- cdec_daily_MSS[192:1000,] %>% 
  filter(date != "1990-06-06", date != "1990-06-07", date != "1990-06-08")

ggplotly(
  ggplot() + geom_point(data=cdec_daily_MSS_1_1000, aes(x=date, y=value_mean_C)))

cdec_daily_MSS_QA <- cdec_daily_MSS_1_1000

ggplotly(
  ggplot() + geom_point(data=cdec_daily_MSS[1001:2000,], aes(x=date, y=value_mean_C)))

cdec_daily_MSS_QA <- rbind(cdec_daily_MSS_QA, cdec_daily_MSS[1001:2000,])

ggplotly(
  ggplot() + geom_point(data=cdec_daily_MSS[2001:3000,], aes(x=date, y=value_mean_C)))

cdec_daily_MSS_2001_3000 <- cdec_daily_MSS[2001:3000,] %>% 
  filter(value_mean_C > 2.18, date != "1997-06-04", date != "1998-06-17")

ggplotly(
  ggplot() + geom_point(data=cdec_daily_MSS_2001_3000, aes(x=date, y=value_mean_C)))

cdec_daily_MSS_QA <- rbind(cdec_daily_MSS_QA, cdec_daily_MSS_2001_3000)

ggplotly(
  ggplot() + geom_point(data=cdec_daily_MSS[3001:4000,], aes(x=date, y=value_mean_C)))

cdec_daily_MSS_QA <- rbind(cdec_daily_MSS_QA, cdec_daily_MSS[3001:4000,])

ggplotly(
  ggplot() + geom_point(data=cdec_daily_MSS[4001:5000,], aes(x=date, y=value_mean_C)))

cdec_daily_MSS_4001_5000 <- cdec_daily_MSS[4001:5000,] %>% 
  filter(date != "2003-12-29")

ggplotly(
  ggplot() + geom_point(data=cdec_daily_MSS_4001_5000, aes(x=date, y=value_mean_C)))

cdec_daily_MSS_QA <- rbind(cdec_daily_MSS_QA, cdec_daily_MSS_4001_5000)

ggplotly(
  ggplot() + geom_point(data=cdec_daily_MSS[5001:6000,], aes(x=date, y=value_mean_C)))

cdec_daily_MSS_5001_6000 <- cdec_daily_MSS[5001:6000,] %>% 
  filter(value_mean_C > 2.63, value_mean_C < 20.31, date != "2004-05-25")

ggplotly(
  ggplot() + geom_point(data=cdec_daily_MSS_5001_6000, aes(x=date, y=value_mean_C)))

cdec_daily_MSS_QA <- rbind(cdec_daily_MSS_QA, cdec_daily_MSS_5001_6000)

ggplotly(
  ggplot() + geom_point(data=cdec_daily_MSS[6001:7000,], aes(x=date, y=value_mean_C)))

cdec_daily_MSS_QA <- rbind(cdec_daily_MSS_QA, cdec_daily_MSS[6001:7000,])

ggplotly(
  ggplot() + geom_point(data=cdec_daily_MSS[7001:8000,], aes(x=date, y=value_mean_C)))

cdec_daily_MSS_QA <- rbind(cdec_daily_MSS_QA, cdec_daily_MSS[7001:8000,])

ggplotly(
  ggplot() + geom_point(data=cdec_daily_MSS[8001:9000,], aes(x=date, y=value_mean_C)))

cdec_daily_MSS_QA <- rbind(cdec_daily_MSS_QA, cdec_daily_MSS[8001:9000,])

ggplotly(
  ggplot() + geom_point(data=cdec_daily_MSS[9001:10000,], aes(x=date, y=value_mean_C)))

cdec_daily_MSS_9001_10000 <- cdec_daily_MSS[9001:10000,] %>% 
  filter(value_mean_C > 0, date != "2016-05-06", date != "2017-01-19")

ggplotly(
  ggplot() + geom_point(data=cdec_daily_MSS_9001_10000, aes(x=date, y=value_mean_C)))

cdec_daily_MSS_QA <- rbind(cdec_daily_MSS_QA, cdec_daily_MSS_9001_10000)

ggplotly(
  ggplot() + geom_point(data=cdec_daily_MSS[10001:10726,], aes(x=date, y=value_mean_C)))

cdec_daily_MSS_10001_10726 <- cdec_daily_MSS[10001:10726,] %>% 
  filter(value_mean_C > 2.83, date != "2019-09-29", date != "2019-09-30", date != "2019-10-01")

ggplotly(
  ggplot() + geom_point(data=cdec_daily_MSS_10001_10726, aes(x=date, y=value_mean_C)))

cdec_daily_MSS_QA <- rbind(cdec_daily_MSS_QA, cdec_daily_MSS_10001_10726)
# Final review ------------------------------------------------------------

#plot QA'd dataset to confirm all points look good
ggplotly(
  ggplot() +geom_point(data = cdec_daily_MSS_QA, aes(x=date, y=value_mean_C)))

#save QA'd dataset as a .rds file
write_rds(cdec_daily_MSS_QA, path = "data/QA_data/cdec_daily_MSS_QA.rds")

#update the gage_QA_progress
gage_QA_progress <- read_csv("data/data_review/gage_QA_progress.csv")

#note reviewer initials, whether review is complete, and any final notes
gage_QA_progress[gage_QA_progress$site_id=="MSS",6:8] <- c("ADW", "Y", "QA complete")

#save updated dataframe to the .csv
write_csv(gage_QA_progress, path = "data/data_review/gage_QA_progress.csv")
