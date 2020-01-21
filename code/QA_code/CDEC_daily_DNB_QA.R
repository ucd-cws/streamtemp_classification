
# Code description --------------------------------------------------------

# Code to review temperature data from gage CDEC_DNB.

library(tidyverse)
library(lubridate)
library(weathermetrics)
library(plotly)

# Get Data ----------------------------------------------------------------

file_list <- list.files("data/data_review/")

# read in next file:
file_list[[17]]

cdec_daily_DNB <- read_rds(path = paste0("data/data_review/",file_list[[17]]))

# Plot --------------------------------------------------------------------

#plot all data to look for obvious bad data points and gaps
ggplotly(
  ggplot() + geom_point(data=cdec_daily_DNB[,], aes(x=date, y=value_mean_C)))

# now make an interactive plot of first 1000 values
ggplotly(
  ggplot() + geom_point(data=cdec_daily_DNB[1:1000,], aes(x=date, y=value_mean_C)))

# Review, QA, and Repeat --------------------------------------------------
cdec_daily_DNB_1_1000 <- cdec_daily_DNB[1:1000,] %>% 
  filter(date != "2005-05-13", date != "2005-05-11", date != "2005-05-12")

ggplotly(
  ggplot() + geom_point(data=cdec_daily_DNB_1_1000, aes(x=date, y=value_mean_C)))

cdec_daily_DNB_QA <- cdec_daily_DNB_1_1000

ggplotly(
  ggplot() + geom_point(data=cdec_daily_DNB[1001:2000,], aes(x=date, y=value_mean_C)))

cdec_daily_DNB_1001_2000 <- cdec_daily_DNB[1001:2000,] %>% 
  filter(value_mean_C > 0)

ggplotly(
  ggplot() + geom_point(data=cdec_daily_DNB_1001_2000, aes(x=date, y=value_mean_C)))

cdec_daily_DNB_QA <- rbind(cdec_daily_DNB_QA, cdec_daily_DNB_1001_2000)

ggplotly(
  ggplot() + geom_point(data=cdec_daily_DNB[2001:3000,], aes(x=date, y=value_mean_C)))

cdec_daily_DNB_QA <- rbind(cdec_daily_DNB_QA, cdec_daily_DNB[2001:3000,])

ggplotly(
  ggplot() + geom_point(data=cdec_daily_DNB[3001:4000,], aes(x=date, y=value_mean_C)))

cdec_daily_DNB_QA <- rbind(cdec_daily_DNB_QA, cdec_daily_DNB[3001:4000,])

ggplotly(
  ggplot() + geom_point(data=cdec_daily_DNB[4001:5000,], aes(x=date, y=value_mean_C)))

cdec_daily_DNB_4001_5000 <- cdec_daily_DNB[4001:5000,] %>% 
  filter(date != "2017-05-10", date != "2017-05-11")

ggplotly(
  ggplot() + geom_point(data=cdec_daily_DNB_4001_5000, aes(x=date, y=value_mean_C)))

cdec_daily_DNB_QA <- rbind(cdec_daily_DNB_QA, cdec_daily_DNB_4001_5000)

ggplotly(
  ggplot() + geom_point(data=cdec_daily_DNB[5001:6000,], aes(x=date, y=value_mean_C)))

cdec_daily_DNB_5001_6000 <- cdec_daily_DNB[5001:6000,] %>% 
  filter(value_mean_C > 0)

ggplotly(
  ggplot() + geom_point(data=cdec_daily_DNB_5001_6000, aes(x=date, y=value_mean_C)))

cdec_daily_DNB_QA <- rbind(cdec_daily_DNB_QA, cdec_daily_DNB_5001_6000)

# Final review ------------------------------------------------------------

#plot QA'd dataset to confirm all points look good
ggplotly(
  ggplot() +geom_point(data = cdec_daily_DNB_QA, aes(x=date, y=value_mean_C)))

#save QA'd dataset as a .rds file
write_rds(cdec_daily_DNB_QA, path = "data/QA_data/cdec_daily_DNB_QA.rds")

#update the gage_QA_progress
gage_QA_progress <- read_csv("data/data_review/gage_QA_progress.csv")

#note reviewer initials, whether review is complete, and any final notes
gage_QA_progress[gage_QA_progress$site_id=="DNB",4:6] <- c("ADW", "Y", "QA complete")

#save updated dataframe to the .csv
write_csv(gage_QA_progress, path = "data/data_review/gage_QA_progress.csv")
