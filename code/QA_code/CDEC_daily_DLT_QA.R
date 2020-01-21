
# Code description --------------------------------------------------------

# Code to review temperature data from gage CDEC_DLT.

library(tidyverse)
library(lubridate)
library(weathermetrics)
library(plotly)

# Get Data ----------------------------------------------------------------

file_list <- list.files("data/data_review/")

# read in next file:
file_list[[16]]

cdec_daily_DLT <- read_rds(path = paste0("data/data_review/",file_list[[16]]))

# Plot --------------------------------------------------------------------

#plot all data to look for obvious bad data points and gaps
ggplotly(
  ggplot() + geom_point(data=cdec_daily_DLT[,], aes(x=date, y=value_mean_C)))

# now make an interactive plot of first 1000 values
ggplotly(
  ggplot() + geom_point(data=cdec_daily_DLT[1:1000,], aes(x=date, y=value_mean_C)))

# Review, QA, and Repeat --------------------------------------------------
cdec_daily_DLT_1_1000 <- cdec_daily_DLT[22:1000,] %>% 
  filter(date != "1990-06-07", date != "1990-06-06", date != "1990-06-08", date != "1992-11-12")

ggplotly(
  ggplot() + geom_point(data=cdec_daily_DLT_1_1000, aes(x=date, y=value_mean_C)))

cdec_daily_DLT_QA <- cdec_daily_DLT_1_1000

ggplotly(
  ggplot() + geom_point(data=cdec_daily_DLT[1001:2000,], aes(x=date, y=value_mean_C)))

cdec_daily_DLT_1001_2000 <- cdec_daily_DLT[1001:2000,] %>% 
  filter(value_mean_C > 1.54)

cdec_daily_DLT_1001_2000 <- cdec_daily_DLT_1001_2000[-c(930:940),]

cdec_daily_DLT_1001_2000 <- cdec_daily_DLT_1001_2000[-c(908:914),]

cdec_daily_DLT_1001_2000 <- cdec_daily_DLT_1001_2000[-c(587:589),]

cdec_daily_DLT_1001_2000 <- cdec_daily_DLT_1001_2000 %>% 
  filter(date != "1994-08-03", date != "1994-08-02", date != "1994-04-01", date != "1994-04-11", date != "1994-04-10", date != "1994-03-31", date != "1993-07-15", date != "1993-08-17")

ggplotly(
  ggplot() + geom_point(data=cdec_daily_DLT_1001_2000, aes(x=date, y=value_mean_C)))

cdec_daily_DLT_QA <- rbind(cdec_daily_DLT_QA, cdec_daily_DLT_1001_2000)

ggplotly(
  ggplot() + geom_point(data=cdec_daily_DLT[2001:3000,], aes(x=date, y=value_mean_C)))

cdec_daily_DLT_2001_3000 <- cdec_daily_DLT[2001:3000,] %>% 
  filter(value_mean_C > 4.54, date != "1996-06-07", date != "1996-08-07", date != "1997-07-02", date != "1997-07-11")

cdec_daily_DLT_2001_3000 <- cdec_daily_DLT_2001_3000[-c(238:249),]

ggplotly(
  ggplot() + geom_point(data=cdec_daily_DLT_2001_3000, aes(x=date, y=value_mean_C)))

cdec_daily_DLT_QA <- rbind(cdec_daily_DLT_QA, cdec_daily_DLT_2001_3000)

ggplotly(
  ggplot() + geom_point(data=cdec_daily_DLT[3001:4000,], aes(x=date, y=value_mean_C)))

cdec_daily_DLT_3001_4000 <- cdec_daily_DLT[3004:4000,] %>% 
  filter(value_mean_C < 30, date != "1998-07-28", date != "1998-08-07", date != "1998-08-11", date != "1998-08-13", date != "1998-08-14", date != "1998-08-16", date != "1998-08-17")

cdec_daily_DLT_3001_4000 <- cdec_daily_DLT_3001_4000[-c(126:136),]

ggplotly(
  ggplot() + geom_point(data=cdec_daily_DLT_3001_4000, aes(x=date, y=value_mean_C)))

cdec_daily_DLT_QA <- rbind(cdec_daily_DLT_QA, cdec_daily_DLT_3001_4000)

ggplotly(
  ggplot() + geom_point(data=cdec_daily_DLT[4001:5000,], aes(x=date, y=value_mean_C)))

cdec_daily_DLT_4001_5000 <- cdec_daily_DLT[4001:5000,] %>% 
  filter(date != "2003-04-21", date != "2003-04-22", date != "2003-12-29", date != "2003-12-30")

ggplotly(
  ggplot() + geom_point(data=cdec_daily_DLT_4001_5000, aes(x=date, y=value_mean_C)))

cdec_daily_DLT_QA <- rbind(cdec_daily_DLT_QA, cdec_daily_DLT_4001_5000)

ggplotly(
  ggplot() + geom_point(data=cdec_daily_DLT[5001:6000,], aes(x=date, y=value_mean_C)))

cdec_daily_DLT_5001_6000 <- cdec_daily_DLT[5001:6000,] %>% 
  filter(date != "2004-12-27", date != "2006-07-14")

ggplotly(
  ggplot() + geom_point(data=cdec_daily_DLT_5001_6000, aes(x=date, y=value_mean_C)))

cdec_daily_DLT_QA <- rbind(cdec_daily_DLT_QA, cdec_daily_DLT_5001_6000)

ggplotly(
  ggplot() + geom_point(data=cdec_daily_DLT[6001:7000,], aes(x=date, y=value_mean_C)))

cdec_daily_DLT_QA <- rbind(cdec_daily_DLT_QA, cdec_daily_DLT[6001:7000,])

ggplotly(
  ggplot() + geom_point(data=cdec_daily_DLT[7001:8000,], aes(x=date, y=value_mean_C)))

cdec_daily_DLT_7001_8000 <- cdec_daily_DLT[7001:8000,] %>% 
  filter(value_mean_C > 0, date != "2011-07-12")

cdec_daily_DLT_7001_8000 <- cdec_daily_DLT_7001_8000[-c(91:123),]

cdec_daily_DLT_7001_8000 <- cdec_daily_DLT_7001_8000[-c(817:826),]

cdec_daily_DLT_7001_8000 <- cdec_daily_DLT_7001_8000[-c(800:803),]

ggplotly(
  ggplot() + geom_point(data=cdec_daily_DLT_7001_8000, aes(x=date, y=value_mean_C)))

cdec_daily_DLT_QA <- rbind(cdec_daily_DLT_QA, cdec_daily_DLT_7001_8000)

ggplotly(
  ggplot() + geom_point(data=cdec_daily_DLT[8001:9000,], aes(x=date, y=value_mean_C)))

cdec_daily_DLT_8001_9000 <- cdec_daily_DLT[8001:9000,] %>% 
  filter(date != "2012-12-21", date != "2012-12-22")

cdec_daily_DLT_8001_9000 <- cdec_daily_DLT_8001_9000[-c(549:558),]

ggplotly(
  ggplot() + geom_point(data=cdec_daily_DLT_8001_9000, aes(x=date, y=value_mean_C)))

cdec_daily_DLT_QA <- rbind(cdec_daily_DLT_QA, cdec_daily_DLT_8001_9000)

ggplotly(
  ggplot() + geom_point(data=cdec_daily_DLT[9001:10000,], aes(x=date, y=value_mean_C)))

cdec_daily_DLT_9001_10000 <- cdec_daily_DLT[9001:10000,] %>% 
  filter(value_mean_C > 1.41, date != "2016-10-07")

cdec_daily_DLT_9001_10000 <- cdec_daily_DLT_9001_10000[-c(885:889),]

cdec_daily_DLT_9001_10000 <- cdec_daily_DLT_9001_10000[-c(829:842),]

cdec_daily_DLT_9001_10000 <- cdec_daily_DLT_9001_10000[-c(836:841),]

cdec_daily_DLT_9001_10000 <- cdec_daily_DLT_9001_10000 %>% 
  filter(date != "2017-07-05", date != "2017-07-06", date != "2017-07-07", date != "2017-07-12", date != "2017-07-13", date != "2017-07-16", date != "2017-07-18", date != "2017-07-19", date != "2017-07-20", date != "2017-07-21", date != "2017-07-22", date != "2017-07-24")

ggplotly(
  ggplot() + geom_point(data=cdec_daily_DLT_9001_10000, aes(x=date, y=value_mean_C)))

cdec_daily_DLT_QA <- rbind(cdec_daily_DLT_QA, cdec_daily_DLT_9001_10000)

ggplotly(
  ggplot() + geom_point(data=cdec_daily_DLT[10001:10678,], aes(x=date, y=value_mean_C)))

cdec_daily_DLT_10001_10678 <- cdec_daily_DLT[10001:10663,] %>% 
  filter(value_mean_C >2.49)

ggplotly(
  ggplot() + geom_point(data=cdec_daily_DLT_10001_10678, aes(x=date, y=value_mean_C)))

cdec_daily_DLT_QA <- rbind(cdec_daily_DLT_QA, cdec_daily_DLT_10001_10678)


# Final review ------------------------------------------------------------

#plot QA'd dataset to confirm all points look good
ggplotly(
  ggplot() +geom_point(data = cdec_daily_DLT_QA, aes(x=date, y=value_mean_C)))

#save QA'd dataset as a .rds file
write_rds(cdec_daily_DLT_QA, path = "data/QA_data/cdec_daily_DLT_QA.rds")

#update the gage_QA_progress
gage_QA_progress <- read_csv("data/data_review/gage_QA_progress.csv")

#note reviewer initials, whether review is complete, and any final notes
gage_QA_progress[gage_QA_progress$site_id=="DLT",4:6] <- c("ADW", "Y", "QA complete")

#save updated dataframe to the .csv
write_csv(gage_QA_progress, path = "data/data_review/gage_QA_progress.csv")
