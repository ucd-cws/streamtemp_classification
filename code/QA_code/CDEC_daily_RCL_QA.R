
# Code description --------------------------------------------------------

# Code to review temperature data from gage CDEC_RCL.

library(tidyverse)
library(lubridate)
library(weathermetrics)
library(plotly)

# Get Data ----------------------------------------------------------------

file_list <- list.files("data/data_review/", pattern = ".rds")

# read in next file:
file_list[[47]]

cdec_daily_RCL <- read_rds(path = paste0("data/data_review/",file_list[[47]]))

# Plot --------------------------------------------------------------------

#plot all data to look for obvious bad data points and gaps
ggplotly(
  ggplot() + geom_point(data=cdec_daily_RCL[,], aes(x=date, y=value_mean_C)))

# now make an interactive plot of first 1000 values
ggplotly(
  ggplot() + geom_point(data=cdec_daily_RCL[1:1000,], aes(x=date, y=value_mean_C)))

# Review, QA, and Repeat --------------------------------------------------
cdec_daily_RCL_1_1000 <- cdec_daily_RCL[628:1000,] %>% 
  filter(date != "2003-12-29")

ggplotly(
  ggplot() + geom_point(data=cdec_daily_RCL_1_1000, aes(x=date, y=value_mean_C)))

cdec_daily_RCL_QA <- cdec_daily_RCL_1_1000

ggplotly(
  ggplot() + geom_point(data=cdec_daily_RCL[1001:2000,], aes(x=date, y=value_mean_C)))

cdec_daily_RCL_1001_2000 <- cdec_daily_RCL[1001:2000,] %>% 
  filter(value_mean_C > 0.13, date != "2006-02-05", date != "2006-07-19")

ggplotly(
  ggplot() + geom_point(data=cdec_daily_RCL_1001_2000, aes(x=date, y=value_mean_C)))

cdec_daily_RCL_QA <- rbind(cdec_daily_RCL_QA, cdec_daily_RCL_1001_2000)

ggplotly(
  ggplot() + geom_point(data=cdec_daily_RCL[2001:3000,], aes(x=date, y=value_mean_C)))

cdec_daily_RCL_QA <- rbind(cdec_daily_RCL_QA, cdec_daily_RCL[2001:3000,])

ggplotly(
  ggplot() + geom_point(data=cdec_daily_RCL[3001:4000,], aes(x=date, y=value_mean_C)))

cdec_daily_RCL_3001_4000 <- cdec_daily_RCL[3001:4000,] %>% 
  filter(date != "2010-02-23", date != "2010-02-22", date != "2010-02-21")

ggplotly(
  ggplot() + geom_point(data=cdec_daily_RCL_3001_4000, aes(x=date, y=value_mean_C)))

cdec_daily_RCL_QA <- rbind(cdec_daily_RCL_QA, cdec_daily_RCL_3001_4000)

ggplotly(
  ggplot() + geom_point(data=cdec_daily_RCL[4001:5000,], aes(x=date, y=value_mean_C)))

cdec_daily_RCL_4001_5000 <- cdec_daily_RCL[4001:5000,] %>% 
  filter(date != "2012-06-14", date != "2012-06-15")

ggplotly(
  ggplot() + geom_point(data=cdec_daily_RCL_4001_5000, aes(x=date, y=value_mean_C)))

cdec_daily_RCL_QA <- rbind(cdec_daily_RCL_QA, cdec_daily_RCL_4001_5000)

ggplotly(
  ggplot() + geom_point(data=cdec_daily_RCL[5001:6000,], aes(x=date, y=value_mean_C)))

cdec_daily_RCL_QA <- rbind(cdec_daily_RCL_QA, cdec_daily_RCL[5001:6000,])

ggplotly(
  ggplot() + geom_point(data=cdec_daily_RCL[6001:6525,], aes(x=date, y=value_mean_C)))

cdec_daily_RCL_QA <- rbind(cdec_daily_RCL_QA, cdec_daily_RCL[6001:6525,])

# Final review ------------------------------------------------------------

#plot QA'd dataset to confirm all points look good
ggplotly(
  ggplot() +geom_point(data = cdec_daily_RCL_QA, aes(x=date, y=value_mean_C)))

#save QA'd dataset as a .rds file
write_rds(cdec_daily_RCL_QA, path = "data/QA_data/cdec_daily_RCL_QA.rds")

#update the gage_QA_progress
gage_QA_progress <- read_csv("data/data_review/gage_QA_progress.csv")

#note reviewer initials, whether review is complete, and any final notes
gage_QA_progress[gage_QA_progress$site_id=="RCL",6:8] <- c("ADW", "Y", "QA complete")

#save updated dataframe to the .csv
write_csv(gage_QA_progress, path = "data/data_review/gage_QA_progress.csv")
