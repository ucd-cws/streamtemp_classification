
# Code description --------------------------------------------------------

# Code to review temperature data from gage CDEC_SHD.

library(tidyverse)
library(lubridate)
library(weathermetrics)
library(plotly)

# Get Data ----------------------------------------------------------------

file_list <- list.files("data/data_review/", pattern = ".rds")

# read in next file:
file_list[[51]]

cdec_daily_SHD <- read_rds(path = paste0("data/data_review/",file_list[[51]]))

# Plot --------------------------------------------------------------------

#plot all data to look for obvious bad data points and gaps
ggplotly(
  ggplot() + geom_point(data=cdec_daily_SHD[,], aes(x=date, y=value_mean_C)))

# now make an interactive plot of first 1000 values
ggplotly(
  ggplot() + geom_point(data=cdec_daily_SHD[1:1000,], aes(x=date, y=value_mean_C)))

# Review, QA, and Repeat --------------------------------------------------
cdec_daily_SHD_1_1000 <- cdec_daily_SHD[1:1000,] %>% 
  filter(value_mean_C > 6.58, value_mean_C < 18.80, date != "1992-05-08", date != "1993-07-08", date != "1993-07-03", date != "1993-07-15", date != "1993-07-18", date != "1993-07-22", date != "1993-07-23", date != "1993-08-04", date != "1993-08-25", date != "1993-09-17", date != "1993-10-29")

ggplotly(
  ggplot() + geom_point(data=cdec_daily_SHD_1_1000, aes(x=date, y=value_mean_C)))

cdec_daily_SHD_QA <- cdec_daily_SHD_1_1000

ggplotly(
  ggplot() + geom_point(data=cdec_daily_SHD[1001:2000,], aes(x=date, y=value_mean_C)))

cdec_daily_SHD_1001_2000 <- cdec_daily_SHD[1001:2000,] %>% 
  filter(value_mean_C > 7.88, date != "1993-12-30", date != "1994-01-20", date != "1994-06-02", date != "1994-06-07", date != "1994-07-13", date != "1994-11-08")

ggplotly(
  ggplot() + geom_point(data=cdec_daily_SHD_1001_2000, aes(x=date, y=value_mean_C)))

cdec_daily_SHD_QA <- rbind(cdec_daily_SHD_QA, cdec_daily_SHD_1001_2000)

ggplotly(
  ggplot() + geom_point(data=cdec_daily_SHD[2001:3000,], aes(x=date, y=value_mean_C)))

cdec_daily_SHD_2001_3000 <- cdec_daily_SHD[2001:3000,] %>% 
  filter(value_mean_C > 7.41, value_mean_C < 15.07, date != "1998-10-18", date != "1998-10-30", date != "1998-11-01", date != "1998-11-02", date != "1997-12-14")

ggplotly(
  ggplot() + geom_point(data=cdec_daily_SHD_2001_3000, aes(x=date, y=value_mean_C)))

cdec_daily_SHD_QA <- rbind(cdec_daily_SHD_QA, cdec_daily_SHD_2001_3000)

ggplotly(
  ggplot() + geom_point(data=cdec_daily_SHD[3001:4000,], aes(x=date, y=value_mean_C)))

cdec_daily_SHD_3001_4000 <- cdec_daily_SHD[3001:4000,] %>% 
  filter(value_mean_C > 6.95)

ggplotly(
  ggplot() + geom_point(data=cdec_daily_SHD_3001_4000, aes(x=date, y=value_mean_C)))

cdec_daily_SHD_QA <- rbind(cdec_daily_SHD_QA, cdec_daily_SHD_3001_4000)

ggplotly(
  ggplot() + geom_point(data=cdec_daily_SHD[4001:5000,], aes(x=date, y=value_mean_C)))

cdec_daily_SHD_4001_5000 <- cdec_daily_SHD[4001:5000,] %>% 
  filter(value_mean_C > 8.28)

ggplotly(
  ggplot() + geom_point(data=cdec_daily_SHD_4001_5000, aes(x=date, y=value_mean_C)))

cdec_daily_SHD_QA <- rbind(cdec_daily_SHD_QA, cdec_daily_SHD_4001_5000)

ggplotly(
  ggplot() + geom_point(data=cdec_daily_SHD[5001:6000,], aes(x=date, y=value_mean_C)))

cdec_daily_SHD_5001_6000 <- cdec_daily_SHD[5001:6000,] %>% 
  filter(value_mean_C > 8.07, value_mean_C < 14.20)

ggplotly(
  ggplot() + geom_point(data=cdec_daily_SHD_5001_6000, aes(x=date, y=value_mean_C)))

cdec_daily_SHD_QA <- rbind(cdec_daily_SHD_QA, cdec_daily_SHD_5001_6000)

ggplotly(
  ggplot() + geom_point(data=cdec_daily_SHD[6001:7000,], aes(x=date, y=value_mean_C)))

cdec_daily_SHD_6001_7000 <- cdec_daily_SHD[6001:7000,] %>% 
  filter(value_mean_C > 6.25, date != "2008-12-05", date != "2009-03-12", date != "2009-03-13", date != "2008-12-03")

ggplotly(
  ggplot() + geom_point(data=cdec_daily_SHD_6001_7000, aes(x=date, y=value_mean_C)))

cdec_daily_SHD_QA <- rbind(cdec_daily_SHD_QA, cdec_daily_SHD_6001_7000)

ggplotly(
  ggplot() + geom_point(data=cdec_daily_SHD[7001:8000,], aes(x=date, y=value_mean_C)))

cdec_daily_SHD_7001_8000 <- cdec_daily_SHD[7001:8000,] %>% 
  filter(value_mean_C > 3.53)

ggplotly(
  ggplot() + geom_point(data=cdec_daily_SHD_7001_8000, aes(x=date, y=value_mean_C)))

cdec_daily_SHD_QA <- rbind(cdec_daily_SHD_QA, cdec_daily_SHD_7001_8000)

ggplotly(
  ggplot() + geom_point(data=cdec_daily_SHD[8001:9000,], aes(x=date, y=value_mean_C)))

cdec_daily_SHD_8001_9000 <- cdec_daily_SHD[8001:9000,] %>% 
  filter(value_mean_C > 3.10)

ggplotly(
  ggplot() + geom_point(data=cdec_daily_SHD_8001_9000, aes(x=date, y=value_mean_C)))

cdec_daily_SHD_QA <- rbind(cdec_daily_SHD_QA, cdec_daily_SHD_8001_9000)

ggplotly(
  ggplot() + geom_point(data=cdec_daily_SHD[9001:10251,], aes(x=date, y=value_mean_C)))

cdec_daily_SHD_9001_10251 <- cdec_daily_SHD[9001:10251,] %>% 
  filter(value_mean_C > 7.65, date != "2016-05-04", date != "2018-02-04", date != "2018-02-05", date != "2019-09-22", date != "2019-08-10", date != "2018-04-27", date != "2018-05-16", date != "2018-05-22", date != "2018-05-23", date != "2018-12-06")

ggplotly(
  ggplot() + geom_point(data=cdec_daily_SHD_9001_10251, aes(x=date, y=value_mean_C)))

cdec_daily_SHD_QA <- rbind(cdec_daily_SHD_QA, cdec_daily_SHD_9001_10251)
# Final review ------------------------------------------------------------

#plot QA'd dataset to confirm all points look good
ggplotly(
  ggplot() +geom_point(data = cdec_daily_SHD_QA, aes(x=date, y=value_mean_C)))

#save QA'd dataset as a .rds file
write_rds(cdec_daily_SHD_QA, path = "data/QA_data/cdec_daily_SHD_QA.rds")

#update the gage_QA_progress
gage_QA_progress <- read_csv("data/data_review/gage_QA_progress.csv")

#note reviewer initials, whether review is complete, and any final notes
gage_QA_progress[gage_QA_progress$site_id=="SHD",6:8] <- c("ADW", "Y", "QA complete")

#save updated dataframe to the .csv
write_csv(gage_QA_progress, path = "data/data_review/gage_QA_progress.csv")
