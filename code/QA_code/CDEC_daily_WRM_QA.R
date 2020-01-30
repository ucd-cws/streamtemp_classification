
# Code description --------------------------------------------------------

# Code to review temperature data from gage CDEC_WRM.

library(tidyverse)
library(lubridate)
library(weathermetrics)
library(plotly)

# Get Data ----------------------------------------------------------------

file_list <- list.files("data/data_review/", pattern = ".rds")

# read in next file:
file_list[[63]]

cdec_daily_WRM <- read_rds(path = paste0("data/data_review/",file_list[[63]]))

# Plot --------------------------------------------------------------------

#plot all data to look for obvious bad data points and gaps
ggplotly(
  ggplot() + geom_point(data=cdec_daily_WRM[,], aes(x=date, y=value_mean_C)))

# now make an interactive plot of first 1000 values
ggplotly(
  ggplot() + geom_point(data=cdec_daily_WRM[1:1000,], aes(x=date, y=value_mean_C)))

# Review, QA, and Repeat --------------------------------------------------
cdec_daily_WRM_1_1000 <- cdec_daily_WRM[1:1000,]

cdec_daily_WRM_1_1000 <- cdec_daily_WRM_1_1000[-c(595:617),]

cdec_daily_WRM_1_1000 <- cdec_daily_WRM_1_1000 %>% 
  filter(date != "2012-09-25", date != "2012-09-26", date != "2012-03-22", date != "2012-03-21", date != "2012-03-20")

ggplotly(
  ggplot() + geom_point(data=cdec_daily_WRM_1_1000, aes(x=date, y=value_mean_C)))

cdec_daily_WRM_QA <- cdec_daily_WRM_1_1000

ggplotly(
  ggplot() + geom_point(data=cdec_daily_WRM[1001:2000,], aes(x=date, y=value_mean_C)))

cdec_daily_WRM_1001_2000 <- cdec_daily_WRM[1001:2000,] %>% 
  filter(date != "2013-09-13", date != "2013-09-12", date != "2013-09-11", date != "2013-09-10", date != "2013-09-09")

ggplotly(
  ggplot() + geom_point(data=cdec_daily_WRM_1001_2000, aes(x=date, y=value_mean_C)))

cdec_daily_WRM_QA <- rbind(cdec_daily_WRM_QA, cdec_daily_WRM_1001_2000)

ggplotly(
  ggplot() + geom_point(data=cdec_daily_WRM[2001:3000,], aes(x=date, y=value_mean_C)))

cdec_daily_WRM_QA <- rbind(cdec_daily_WRM_QA, cdec_daily_WRM[2001:3000,])

ggplotly(
  ggplot() + geom_point(data=cdec_daily_WRM[3001:3521,], aes(x=date, y=value_mean_C)))

cdec_daily_WRM_3001_3521 <- cdec_daily_WRM[3001:3521,] %>% 
  filter(date!= "2019-03-07")

ggplotly(
  ggplot() + geom_point(data=cdec_daily_WRM_3001_3521, aes(x=date, y=value_mean_C)))

cdec_daily_WRM_QA <- rbind(cdec_daily_WRM_QA, cdec_daily_WRM_3001_3521)
# Final review ------------------------------------------------------------

#plot QA'd dataset to confirm all points look good
ggplotly(
  ggplot() +geom_point(data = cdec_daily_WRM_QA, aes(x=date, y=value_mean_C)))

#save QA'd dataset as a .rds file
write_rds(cdec_daily_WRM_QA, path = "data/QA_data/cdec_daily_WRM_QA.rds")

#update the gage_QA_progress
gage_QA_progress <- read_csv("data/data_review/gage_QA_progress.csv")

#note reviewer initials, whether review is complete, and any final notes
gage_QA_progress[gage_QA_progress$site_id=="WRM",6:8] <- c("ADW", "Y", "QA complete")

#save updated dataframe to the .csv
write_csv(gage_QA_progress, path = "data/data_review/gage_QA_progress.csv")
