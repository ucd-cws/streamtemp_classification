
# Code description --------------------------------------------------------

# Code to review temperature data from gage CDEC_AFD.

library(tidyverse)
library(lubridate)
library(weathermetrics)
library(plotly)

# Get Data ----------------------------------------------------------------

file_list <- list.files("data/data_review/")

# read in next file:
file_list[[8]]

cdec_hourly_AFD <- read_rds(path = paste0("data/data_review/",file_list[[8]]))

# Plot --------------------------------------------------------------------

# now make an interactive plot of first 1000 values
ggplotly(
  ggplot() + geom_point(data=cdec_hourly_AFD[1:1000,], aes(x=datetime, y=value)))

# Review, QA, and Repeat --------------------------------------------------

cdec_hourly_AFD_1_1001 <- cdec_hourly_AFD[1:1000,] %>% 
  filter(value <69.7, value > 40)

ggplotly(
  ggplot() + geom_point(data=cdec_hourly_AFD_1_1001, aes(x=datetime, y=value)))

cdec_hourly_AFD_QA <- cdec_hourly_AFD_1_1001

ggplotly(
  ggplot() + geom_point(data=cdec_hourly_AFD[1001:4000,], aes(x=datetime, y=value)))

cdec_hourly_AFD_1001_4000 <- cdec_hourly_AFD[1001:4000,] %>% 
  filter(value<80, value > 43.5, datetime != "1999-04-15")

ggplotly(
  ggplot() + geom_point(data=cdec_hourly_AFD_1001_4000, aes(x=datetime, y=value)))

cdec_hourly_AFD_QA <- rbind(cdec_hourly_AFD_QA, cdec_hourly_AFD_1001_4000)

ggplotly(
  ggplot() + geom_point(data=cdec_hourly_AFD[4001:10000,], aes(x=datetime, y=value)))

cdec_hourly_AFD_4001_10000 <- cdec_hourly_AFD[4001:10000,] %>% 
  filter(value > 46.4, value < 68.3, datetime != "1999-10-31 08:00:00")

ggplotly(
  ggplot() + geom_point(data=cdec_hourly_AFD_4001_10000, aes(x=datetime, y=value))) #I don't actually think all of these points are good - see Jul through Oct 1999, but I'm not sure it will make much of a difference after they've been averaged out to daily.

cdec_hourly_AFD_QA <- rbind(cdec_hourly_AFD_QA, cdec_hourly_AFD_4001_10000)

ggplotly(
  ggplot() + geom_point(data=cdec_hourly_AFD[10001:30000,], aes(x=datetime, y=value)))

cdec_hourly_AFD_10001_30000 <- cdec_hourly_AFD[10001:30000,] %>% 
  filter(value > 43.9, value < 71.5, datetime != "2000-07-08 18:00:00", datetime != "2000-10-06 18:00:00", datetime != "2000-11-03 22:00:00")

ggplotly(
  ggplot() + geom_point(data=cdec_hourly_AFD_10001_30000, aes(x=datetime, y=value)))

cdec_hourly_AFD_QA <- rbind(cdec_hourly_AFD_QA, cdec_hourly_AFD_10001_30000)

ggplotly(
  ggplot() + geom_point(data=cdec_hourly_AFD[30001:50000,], aes(x=datetime, y=value)))

cdec_hourly_AFD_30001_50000 <- cdec_hourly_AFD[30001:50000,] %>% 
  filter(value > 40.2, value < 70, datetime != "2004-03-11 14:00:00", datetime != "2004-04-10 06:00:00", datetime != "2005-05-08 12:00:00", datetime != "2005-06-24 23:00:00")

ggplotly(
  ggplot() + geom_point(data=cdec_hourly_AFD_30001_50000, aes(x=datetime, y=value)))

cdec_hourly_AFD_QA <- rbind(cdec_hourly_AFD_QA, cdec_hourly_AFD_30001_50000)

ggplotly(
  ggplot() + geom_point(data=cdec_hourly_AFD[50001:70000,], aes(x=datetime, y=value)))

cdec_hourly_AFD_50001_70000 <- cdec_hourly_AFD[50001:70000,] %>% 
  filter(value > 40.9, value < 80, datetime != "2006-03-08 04:00:00")

cdec_hourly_AFD_50001_70000 <- cdec_hourly_AFD_50001_70000[-c(3024:3029),]

ggplotly(
  ggplot() + geom_point(data=cdec_hourly_AFD_50001_70000, aes(x=datetime, y=value)))

cdec_hourly_AFD_QA <- rbind(cdec_hourly_AFD_QA, cdec_hourly_AFD_50001_70000)

ggplotly(
  ggplot() + geom_point(data=cdec_hourly_AFD[70001:90000,], aes(x=datetime, y=value)))

cdec_hourly_AFD_70001_90000 <- cdec_hourly_AFD[70001:90000,] %>% 
  filter(value > 42.5, datetime != "2009-02-18 06:00:00")

ggplotly(
  ggplot() + geom_point(data=cdec_hourly_AFD_70001_90000, aes(x=datetime, y=value)))

cdec_hourly_AFD_QA <- rbind(cdec_hourly_AFD_QA,cdec_hourly_AFD_70001_90000)

#start QA process from here.

# Final review ------------------------------------------------------------

#plot QA'd dataset to confirm all points look good
ggplotly(
  ggplot() +geom_point(data = cdec_daily_AFD_QA, aes(x=datetime, y=value)))

#save QA'd dataset as a .rds file
write_rds(cdec_daily_AFD_QA, path = "data/QA_data/cdec_daily_AFD_QA.rds", compress = "gz")

#update the gage_QA_progress
gage_QA_progress <- read_csv("data/data_review/gage_QA_progress.csv")

#confirm correct row to update by the site_id
gage_QA_progress[103,1]

#note reviewer initials, whether review is complete, and any final notes
gage_QA_progress[103,4:6] <- c("ADW", "Y", "QA complete")

#save updated dataframe to the .csv
write_csv(gage_QA_progress, path = "data/data_review/gage_QA_progress.csv")
