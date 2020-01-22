
# Code description --------------------------------------------------------

# Code to review temperature data from gage CDEC_GVI.

library(tidyverse)
library(lubridate)
library(weathermetrics)
library(plotly)

# Get Data ----------------------------------------------------------------

file_list <- list.files("data/data_review/")

# read in next file:
file_list[[25]]

cdec_daily_GVI <- read_rds(path = paste0("data/data_review/",file_list[[25]]))

# Plot --------------------------------------------------------------------

#plot all data to look for obvious bad data points and gaps
ggplotly(
  ggplot() + geom_point(data=cdec_daily_GVI[,], aes(x=date, y=value_mean_C)))

# now make an interactive plot of first 1000 values
ggplotly(
  ggplot() + geom_point(data=cdec_daily_GVI[1:1000,], aes(x=date, y=value_mean_C)))

# Review, QA, and Repeat --------------------------------------------------
cdec_daily_GVI_1_1000 <- cdec_daily_GVI[1:1000,] %>% 
  filter(date != "2001-03-17", date != "2001-03-18", date != "2001-03-19", date != "2001-03-20", date != "2001-03-21", date != "2001-03-22", date != "2001-03-23", date != "2001-03-24", date != "2001-11-29")

ggplotly(
  ggplot() + geom_point(data=cdec_daily_GVI_1_1000, aes(x=date, y=value_mean_C)))

cdec_daily_GVI_QA <- cdec_daily_GVI_1_1000

ggplotly(
  ggplot() + geom_point(data=cdec_daily_GVI[1001:2000,], aes(x=date, y=value_mean_C)))

cdec_daily_GVI_1001_2000 <- cdec_daily_GVI[1001:2000,] %>% 
  filter(value_mean_C > 1.55, date != "2003-02-11", date != "2003-02-12", date != "2003-12-29", date != "2004-01-03")

ggplotly(
  ggplot() + geom_point(data=cdec_daily_GVI_1001_2000, aes(x=date, y=value_mean_C)))

cdec_daily_GVI_QA <- rbind(cdec_daily_GVI_QA, cdec_daily_GVI_1001_2000)

ggplotly(
  ggplot() + geom_point(data=cdec_daily_GVI[2001:3000,], aes(x=date, y=value_mean_C)))

cdec_daily_GVI_2001_3000 <- cdec_daily_GVI[2001:3000,] %>% 
  filter(value_mean_C > 0, value_mean_C < 17.33, date != "2005-02-12", date != "2005-02-15", date != "2005-02-16", date != "2005-02-18", date != "2005-04-23", date != "2005-03-19")

ggplotly(
  ggplot() + geom_point(data=cdec_daily_GVI_2001_3000, aes(x=date, y=value_mean_C)))

cdec_daily_GVI_QA <- rbind(cdec_daily_GVI_QA, cdec_daily_GVI_2001_3000)

ggplotly(
  ggplot() + geom_point(data=cdec_daily_GVI[3001:4000,], aes(x=date, y=value_mean_C)))

cdec_daily_GVI_3001_4000 <- cdec_daily_GVI[3001:4000,] %>% 
  filter(date != "2009-02-17", date != "2009-02-16", date != "2009-02-15", date != "2009-12-06", date != "2009-12-11", date != "2009-12-07", date != "2009-12-10", date != "2009-12-08", date != "2009-12-09")

ggplotly(
  ggplot() + geom_point(data=cdec_daily_GVI_3001_4000, aes(x=date, y=value_mean_C)))

cdec_daily_GVI_QA <- rbind(cdec_daily_GVI_QA, cdec_daily_GVI_3001_4000)

ggplotly(
  ggplot() + geom_point(data=cdec_daily_GVI[4001:5000,], aes(x=date, y=value_mean_C)))

cdec_daily_GVI_QA <- rbind(cdec_daily_GVI_QA, cdec_daily_GVI[4001:5000,])

ggplotly(
  ggplot() + geom_point(data=cdec_daily_GVI[5001:6000,], aes(x=date, y=value_mean_C)))

cdec_daily_GVI_QA <- rbind(cdec_daily_GVI_QA, cdec_daily_GVI[5001:6000,])

ggplotly(
  ggplot() + geom_point(data=cdec_daily_GVI[6001:6334,], aes(x=date, y=value_mean_C)))

cdec_daily_GVI_QA <- rbind(cdec_daily_GVI_QA, cdec_daily_GVI[6001:6334,])

cdec_daily_GVI_QA <- cdec_daily_GVI_QA %>% 
  filter(value_mean_C > 0)

# Final review ------------------------------------------------------------

#plot QA'd dataset to confirm all points look good
ggplotly(
  ggplot() +geom_point(data = cdec_daily_GVI_QA, aes(x=date, y=value_mean_C)))

#save QA'd dataset as a .rds file
write_rds(cdec_daily_GVI_QA, path = "data/QA_data/cdec_daily_GVI_QA.rds")

#update the gage_QA_progress
gage_QA_progress <- read_csv("data/data_review/gage_QA_progress.csv")

#note reviewer initials, whether review is complete, and any final notes
gage_QA_progress[gage_QA_progress$site_id=="GVI",4:6] <- c("ADW", "Y", "QA complete")

#save updated dataframe to the .csv
write_csv(gage_QA_progress, path = "data/data_review/gage_QA_progress.csv")
