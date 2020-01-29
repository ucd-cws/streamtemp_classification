
# Code description --------------------------------------------------------

# Code to review temperature data from gage CDEC_PMN.

library(tidyverse)
library(lubridate)
library(weathermetrics)
library(plotly)

# Get Data ----------------------------------------------------------------

file_list <- list.files("data/data_review/", pattern = ".rds")

# read in next file:
file_list[[46]]

cdec_daily_PMN <- read_rds(path = paste0("data/data_review/",file_list[[46]]))

# Plot --------------------------------------------------------------------

#plot all data to look for obvious bad data points and gaps
ggplotly(
  ggplot() + geom_point(data=cdec_daily_PMN[,], aes(x=date, y=value_mean_C)))

# now make an interactive plot of first 1000 values
ggplotly(
  ggplot() + geom_point(data=cdec_daily_PMN[1:1000,], aes(x=date, y=value_mean_C)))

# Review, QA, and Repeat --------------------------------------------------
cdec_daily_PMN_1_1000 <- cdec_daily_PMN[1:1000,] %>% 
  filter(date != "1990-06-07", date != "1990-06-08", date != "1991-08-25", date != "1991-09-25", date != "1992-12-22")

ggplotly(
  ggplot() + geom_point(data=cdec_daily_PMN_1_1000, aes(x=date, y=value_mean_C)))

cdec_daily_PMN_QA <- cdec_daily_PMN_1_1000

ggplotly(
  ggplot() + geom_point(data=cdec_daily_PMN[1001:2000,], aes(x=date, y=value_mean_C)))

cdec_daily_PMN_1001_2000 <- cdec_daily_PMN[1001:2000,] %>% 
  filter(value_mean_C > 4.66, date != "1993-07-21", date != "1993-08-24", date != "1993-09-27", date != "1994-03-24", date != "1994-07-05", date != "1994-07-18", date != "1994-07-22", date != "1994-08-06", date != "1994-08-07", date != "1994-08-08", date != "1994-08-10")

ggplotly(
  ggplot() + geom_point(data=cdec_daily_PMN_1001_2000, aes(x=date, y=value_mean_C)))

cdec_daily_PMN_QA <- rbind(cdec_daily_PMN_QA, cdec_daily_PMN_1001_2000)

ggplotly(
  ggplot() + geom_point(data=cdec_daily_PMN[2001:3000,], aes(x=date, y=value_mean_C)))

cdec_daily_PMN_2001_3000 <- cdec_daily_PMN[2001:3000,] %>% 
  filter(value_mean_C > 4.15, date != "1997-10-01", date != "1997-10-03")

ggplotly(
  ggplot() + geom_point(data=cdec_daily_PMN_2001_3000, aes(x=date, y=value_mean_C)))

cdec_daily_PMN_QA <- rbind(cdec_daily_PMN_QA, cdec_daily_PMN_2001_3000)

ggplotly(
  ggplot() + geom_point(data=cdec_daily_PMN[3001:4000,], aes(x=date, y=value_mean_C)))

cdec_daily_PMN_3001_4000 <- cdec_daily_PMN[3001:4000,] %>% 
  filter(value_mean_C > 4.40, date != "1999-11-07", date != "2000-10-13", date != "2001-07-21")

cdec_daily_PMN_3001_4000 <- cdec_daily_PMN_3001_4000[-c(764:787),]

ggplotly(
  ggplot() + geom_point(data=cdec_daily_PMN_3001_4000, aes(x=date, y=value_mean_C)))

cdec_daily_PMN_QA <- rbind(cdec_daily_PMN_QA, cdec_daily_PMN_3001_4000)

ggplotly(
  ggplot() + geom_point(data=cdec_daily_PMN[4001:5000,], aes(x=date, y=value_mean_C)))

cdec_daily_PMN_4001_5000 <- cdec_daily_PMN[4001:5000,] %>% 
  filter(value_mean_C > 5.0, date != "2004-06-02", date != "2004-05-27", date != "2003-12-06", date != "2003-05-07", date != "2003-05-16", date != "2003-08-19", date != "2003-09-04", date != "2003-09-06", date != "2003-10-15", date != "2003-10-17")

ggplotly(
  ggplot() + geom_point(data=cdec_daily_PMN_4001_5000, aes(x=date, y=value_mean_C)))

cdec_daily_PMN_QA <- rbind(cdec_daily_PMN_QA, cdec_daily_PMN_4001_5000)

ggplotly(
  ggplot() + geom_point(data=cdec_daily_PMN[5001:6000,], aes(x=date, y=value_mean_C)))

cdec_daily_PMN_5001_6000 <- cdec_daily_PMN[5001:6000,] %>% 
  filter(value_mean_C > 3.02)

ggplotly(
  ggplot() + geom_point(data=cdec_daily_PMN_5001_6000, aes(x=date, y=value_mean_C)))

cdec_daily_PMN_QA <- rbind(cdec_daily_PMN_QA, cdec_daily_PMN_5001_6000)

ggplotly(
  ggplot() + geom_point(data=cdec_daily_PMN[6001:7000,], aes(x=date, y=value_mean_C)))

cdec_daily_PMN_6001_7000 <-  cdec_daily_PMN[6001:7000,] %>% 
  filter(value_mean_C > 4.37)

ggplotly(
  ggplot() + geom_point(data=cdec_daily_PMN_6001_7000, aes(x=date, y=value_mean_C)))

cdec_daily_PMN_QA <- rbind(cdec_daily_PMN_QA, cdec_daily_PMN_6001_7000)

ggplotly(
  ggplot() + geom_point(data=cdec_daily_PMN[7001:8000,], aes(x=date, y=value_mean_C)))

cdec_daily_PMN_QA <- rbind(cdec_daily_PMN_QA, cdec_daily_PMN[7001:8000,])

ggplotly(
  ggplot() + geom_point(data=cdec_daily_PMN[8001:9000,], aes(x=date, y=value_mean_C)))

cdec_daily_PMN_QA <- rbind(cdec_daily_PMN_QA, cdec_daily_PMN[8001:9000,])

ggplotly(
  ggplot() + geom_point(data=cdec_daily_PMN[9001:10379,], aes(x=date, y=value_mean_C)))

cdec_daily_PMN_9001_10379 <- cdec_daily_PMN[9001:10379,] %>% 
  filter(value_mean_C > 1.87, date != "2017-08-09", date != "2018-03-23")

ggplotly(
  ggplot() + geom_point(data=cdec_daily_PMN_9001_10379, aes(x=date, y=value_mean_C)))

cdec_daily_PMN_QA <- rbind(cdec_daily_PMN_QA, cdec_daily_PMN_9001_10379)  
# Final review ------------------------------------------------------------

#plot QA'd dataset to confirm all points look good
ggplotly(
  ggplot() +geom_point(data = cdec_daily_PMN_QA, aes(x=date, y=value_mean_C)))

#save QA'd dataset as a .rds file
write_rds(cdec_daily_PMN_QA, path = "data/QA_data/cdec_daily_PMN_QA.rds")

#update the gage_QA_progress
gage_QA_progress <- read_csv("data/data_review/gage_QA_progress.csv")

#note reviewer initials, whether review is complete, and any final notes
gage_QA_progress[gage_QA_progress$site_id=="PMN",6:8] <- c("ADW", "Y", "QA complete")

#save updated dataframe to the .csv
write_csv(gage_QA_progress, path = "data/data_review/gage_QA_progress.csv")
