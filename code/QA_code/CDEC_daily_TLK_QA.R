
# Code description --------------------------------------------------------

# Code to review temperature data from gage CDEC_TLK.

library(tidyverse)
library(lubridate)
library(weathermetrics)
library(plotly)

# Get Data ----------------------------------------------------------------

file_list <- list.files("data/data_review/", pattern = ".rds")

# read in next file:
file_list[[59]]

cdec_daily_TLK <- read_rds(path = paste0("data/data_review/",file_list[[59]]))

# Plot --------------------------------------------------------------------

#plot all data to look for obvious bad data points and gaps
ggplotly(
  ggplot() + geom_point(data=cdec_daily_TLK[,], aes(x=date, y=value_mean_C)))

# now make an interactive plot of first 1000 values
ggplotly(
  ggplot() + geom_point(data=cdec_daily_TLK[1:1000,], aes(x=date, y=value_mean_C)))

# Review, QA, and Repeat --------------------------------------------------
cdec_daily_TLK_1_1000 <- cdec_daily_TLK[1:1000,] %>% 
  filter(value_mean_C > 2.63, date != "2003-05-28", date != "2003-05-23", date != "2001-07-12")

cdec_daily_TLK_1_1000 <- cdec_daily_TLK_1_1000[-c(265:291),]

ggplotly(
  ggplot() + geom_point(data=cdec_daily_TLK_1_1000, aes(x=date, y=value_mean_C)))

cdec_daily_TLK_QA <- cdec_daily_TLK_1_1000

ggplotly(
  ggplot() + geom_point(data=cdec_daily_TLK[1001:2000,], aes(x=date, y=value_mean_C)))

cdec_daily_TLK_QA <- rbind(cdec_daily_TLK_QA, cdec_daily_TLK[1001:2000,])

ggplotly(
  ggplot() + geom_point(data=cdec_daily_TLK[2001:3000,], aes(x=date, y=value_mean_C)))

cdec_daily_TLK_2001_3000 <- cdec_daily_TLK[2001:3000,] %>% 
  filter(value_mean_C > 0.89)

ggplotly(
  ggplot() + geom_point(data=cdec_daily_TLK_2001_3000, aes(x=date, y=value_mean_C)))

cdec_daily_TLK_QA <- rbind(cdec_daily_TLK_QA, cdec_daily_TLK_2001_3000)

ggplotly(
  ggplot() + geom_point(data=cdec_daily_TLK[3001:4000,], aes(x=date, y=value_mean_C)))

cdec_daily_TLK_QA <- rbind(cdec_daily_TLK_QA, cdec_daily_TLK[3001:4000,])

ggplotly(
  ggplot() + geom_point(data=cdec_daily_TLK[4001:5000,], aes(x=date, y=value_mean_C)))

cdec_daily_TLK_QA <- rbind(cdec_daily_TLK_QA, cdec_daily_TLK[4001:5000,])

ggplotly(
  ggplot() + geom_point(data=cdec_daily_TLK[5001:6000,], aes(x=date, y=value_mean_C)))

cdec_daily_TLK_5001_6000 <- cdec_daily_TLK[5001:6000,] %>% 
  filter(date != "2016-01-01", date != "2016-01-02", date != "2016-01-06", date != "2016-05-06")

ggplotly(
  ggplot() + geom_point(data=cdec_daily_TLK_5001_6000, aes(x=date, y=value_mean_C)))

cdec_daily_TLK_QA <- rbind(cdec_daily_TLK_QA, cdec_daily_TLK_5001_6000)

ggplotly(
  ggplot() + geom_point(data=cdec_daily_TLK[6001:7000,], aes(x=date, y=value_mean_C)))

cdec_daily_TLK_6001_7000 <- cdec_daily_TLK[6001:7000,] %>% 
  filter(date != "2019-02-13")

ggplotly(
  ggplot() + geom_point(data=cdec_daily_TLK_6001_7000, aes(x=date, y=value_mean_C)))

cdec_daily_TLK_QA <- rbind(cdec_daily_TLK_QA, cdec_daily_TLK_6001_7000)
# Final review ------------------------------------------------------------

#plot QA'd dataset to confirm all points look good
ggplotly(
  ggplot() +geom_point(data = cdec_daily_TLK_QA, aes(x=date, y=value_mean_C)))

#save QA'd dataset as a .rds file
write_rds(cdec_daily_TLK_QA, path = "data/QA_data/cdec_daily_TLK_QA.rds")

#update the gage_QA_progress
gage_QA_progress <- read_csv("data/data_review/gage_QA_progress.csv")

#note reviewer initials, whether review is complete, and any final notes
gage_QA_progress[gage_QA_progress$site_id=="TLK",6:8] <- c("ADW", "Y", "QA complete")

#save updated dataframe to the .csv
write_csv(gage_QA_progress, path = "data/data_review/gage_QA_progress.csv")
