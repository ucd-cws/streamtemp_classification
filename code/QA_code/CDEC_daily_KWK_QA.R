
# Code description --------------------------------------------------------

# Code to review temperature data from gage CDEC_KWK.

library(tidyverse)
library(lubridate)
library(weathermetrics)
library(plotly)

# Get Data ----------------------------------------------------------------

file_list <- list.files("data/data_review/")

# read in next file:
file_list[[35]]

cdec_daily_KWK <- read_rds(path = paste0("data/data_review/",file_list[[35]]))

# Plot --------------------------------------------------------------------

#plot all data to look for obvious bad data points and gaps
ggplotly(
  ggplot() + geom_point(data=cdec_daily_KWK[,], aes(x=date, y=value_mean_C)))

# now make an interactive plot of first 1000 values
ggplotly(
  ggplot() + geom_point(data=cdec_daily_KWK[1:1000,], aes(x=date, y=value_mean_C)))

# Review, QA, and Repeat --------------------------------------------------
cdec_daily_KWK_1_1000 <- cdec_daily_KWK[1:1000,] %>% 
  filter(value_mean_C > 7.19, date != "1989-12-27", date != "1990-01-04", date != "1990-10-15", date != "1991-02-14")

ggplotly(
  ggplot() + geom_point(data=cdec_daily_KWK_1_1000, aes(x=date, y=value_mean_C)))

cdec_daily_KWK_QA <- cdec_daily_KWK_1_1000

ggplotly(
  ggplot() + geom_point(data=cdec_daily_KWK[1001:2000,], aes(x=date, y=value_mean_C)))

cdec_daily_KWK_1001_2000 <- cdec_daily_KWK[1001:2000,] %>% 
  filter(value_mean_C > 7.17, date != "1993-05-21", date != "1993-06-01", date != "1993-07-06", date != "1993-07-15", date!= "1993-08-25", date != "1993-09-23", date!= "1994-01-06", date != "1994-07-25", date != "1994-08-30", date != "1995-09-05")

ggplotly(
  ggplot() + geom_point(data=cdec_daily_KWK_1001_2000, aes(x=date, y=value_mean_C)))

cdec_daily_KWK_QA <- rbind(cdec_daily_KWK_QA, cdec_daily_KWK_1001_2000)

ggplotly(
  ggplot() + geom_point(data=cdec_daily_KWK[2001:3000,], aes(x=date, y=value_mean_C)))

cdec_daily_KWK_2001_3000 <- cdec_daily_KWK[2002:3000,] %>% 
  filter(value_mean_C > 8.12, date != "1996-06-23", date != "1996-06-27", date != "1996-07-12", date!= "1996-07-29", date != "1996-09-04", date != "1996-09-14")

ggplotly(
  ggplot() + geom_point(data=cdec_daily_KWK_2001_3000, aes(x=date, y=value_mean_C)))

cdec_daily_KWK_QA <- rbind(cdec_daily_KWK_QA, cdec_daily_KWK_2001_3000)

ggplotly(
  ggplot() + geom_point(data=cdec_daily_KWK[3001:4000,], aes(x=date, y=value_mean_C)))

cdec_daily_KWK_3001_4000 <- cdec_daily_KWK[3001:4000,] %>% 
  filter(value_mean_C > 7.05, date != "2000-01-23", date != "2000-03-16", date != "2000-04-06", date != "2000-04-30", date != "2000-05-25", date != "2000-06-28", date != "2000-06-27", date != "2000-07-17", date != "2000-09-25", date != "2001-05-02", date != "2001-05-19", date != "2001-05-26", date != "2000-07-18")

ggplotly(
  ggplot() + geom_point(data=cdec_daily_KWK_3001_4000, aes(x=date, y=value_mean_C)))

cdec_daily_KWK_QA <- rbind(cdec_daily_KWK_QA, cdec_daily_KWK_3001_4000)

ggplotly(
  ggplot() + geom_point(data=cdec_daily_KWK[4001:5000,], aes(x=date, y=value_mean_C)))

cdec_daily_KWK_4001_5000 <- cdec_daily_KWK[4001:5000,] %>% 
  filter(value_mean_C > 8.01, date != "2001-06-22", date != "2002-11-28")

ggplotly(
  ggplot() + geom_point(data=cdec_daily_KWK_4001_5000, aes(x=date, y=value_mean_C)))

cdec_daily_KWK_QA <- rbind(cdec_daily_KWK_QA, cdec_daily_KWK_4001_5000)

ggplotly(
  ggplot() + geom_point(data=cdec_daily_KWK[5001:6000,], aes(x=date, y=value_mean_C)))

cdec_daily_KWK_5001_6000 <- cdec_daily_KWK[5001:6000,] %>% 
  filter(value_mean_C > 7.75, date != "2004-05-14", date != "2004-05-27", date != "2004-05-29", date != "2004-06-06", date != "2004-06-24", date != "2004-07-10")

ggplotly(
  ggplot() + geom_point(data=cdec_daily_KWK_5001_6000, aes(x=date, y=value_mean_C)))

cdec_daily_KWK_QA <- rbind(cdec_daily_KWK_QA, cdec_daily_KWK_5001_6000)

ggplotly(
  ggplot() + geom_point(data=cdec_daily_KWK[6001:7000,], aes(x=date, y=value_mean_C)))

cdec_daily_KWK_6001_7000 <- cdec_daily_KWK[6001:7000,] %>% 
  filter(value_mean_C > 6.73, date != "2007-07-22", date != "2007-11-12", date != "2007-11-11")

ggplotly(
  ggplot() + geom_point(data=cdec_daily_KWK_6001_7000, aes(x=date, y=value_mean_C)))

cdec_daily_KWK_QA <- rbind(cdec_daily_KWK_QA, cdec_daily_KWK_6001_7000)

ggplotly(
  ggplot() + geom_point(data=cdec_daily_KWK[7001:8000,], aes(x=date, y=value_mean_C)))

cdec_daily_KWK_7001_8000 <- cdec_daily_KWK[7001:8000,] %>% 
  filter(date != "2009-10-07", date != "2010-08-17")

ggplotly(
  ggplot() + geom_point(data=cdec_daily_KWK_7001_8000, aes(x=date, y=value_mean_C)))

cdec_daily_KWK_QA <- rbind(cdec_daily_KWK_QA, cdec_daily_KWK_7001_8000)

ggplotly(
  ggplot() + geom_point(data=cdec_daily_KWK[8001:9000,], aes(x=date, y=value_mean_C)))

cdec_daily_KWK_QA <- rbind(cdec_daily_KWK_QA, cdec_daily_KWK[8001:9000,])

ggplotly(
  ggplot() + geom_point(data=cdec_daily_KWK[9001:10000,], aes(x=date, y=value_mean_C)))

cdec_daily_KWK_9001_10000 <- cdec_daily_KWK[9001:10000,] %>% 
  filter(value_mean_C > 7.27)

ggplotly(
  ggplot() + geom_point(data=cdec_daily_KWK_9001_10000, aes(x=date, y=value_mean_C)))

cdec_daily_KWK_QA <- rbind(cdec_daily_KWK_QA, cdec_daily_KWK_9001_10000)

ggplotly(
  ggplot() + geom_point(data=cdec_daily_KWK[10001:10689,], aes(x=date, y=value_mean_C)))

cdec_daily_KWK_QA <- rbind(cdec_daily_KWK_QA, cdec_daily_KWK[10001:10689,])

# Final review ------------------------------------------------------------

#plot QA'd dataset to confirm all points look good
ggplotly(
  ggplot() +geom_point(data = cdec_daily_KWK_QA, aes(x=date, y=value_mean_C)))

#save QA'd dataset as a .rds file
write_rds(cdec_daily_KWK_QA, path = "data/QA_data/cdec_daily_KWK_QA.rds")

#update the gage_QA_progress
gage_QA_progress <- read_csv("data/data_review/gage_QA_progress.csv")

#note reviewer initials, whether review is complete, and any final notes
gage_QA_progress[gage_QA_progress$site_id=="KWK",4:6] <- c("ADW", "Y", "QA complete")

#save updated dataframe to the .csv
write_csv(gage_QA_progress, path = "data/data_review/gage_QA_progress.csv")
