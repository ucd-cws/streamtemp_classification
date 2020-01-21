
# Code description --------------------------------------------------------

# Code to review temperature data from gage CDEC_FWQ.

library(tidyverse)
library(lubridate)
library(weathermetrics)
library(plotly)

# Get Data ----------------------------------------------------------------

file_list <- list.files("data/data_review/")

# read in next file:
file_list[[21]]

cdec_daily_FWQ <- read_rds(path = paste0("data/data_review/",file_list[[21]]))

# Plot --------------------------------------------------------------------

#plot all data to look for obvious bad data points and gaps
ggplotly(
  ggplot() + geom_point(data=cdec_daily_FWQ[,], aes(x=date, y=value_mean_C)))

# now make an interactive plot of first 1000 values
ggplotly(
  ggplot() + geom_point(data=cdec_daily_FWQ[1:1000,], aes(x=date, y=value_mean_C)))

# Review, QA, and Repeat --------------------------------------------------
cdec_daily_FWQ_1_1000 <- cdec_daily_FWQ[1:1000,] %>% 
  filter(value_mean_C > 7.38, value_mean_C < 14.71, date != "2010-06-04", date != "2010-06-13", date != "2010-06-14", date != "2010-06-19", date != "2010-07-08", date != "2011-07-10")

ggplotly(
  ggplot() + geom_point(data=cdec_daily_FWQ_1_1000, aes(x=date, y=value_mean_C)))

cdec_daily_FWQ_QA <- cdec_daily_FWQ_1_1000

ggplotly(
  ggplot() + geom_point(data=cdec_daily_FWQ[1001:2000,], aes(x=date, y=value_mean_C)))

cdec_daily_FWQ_1001_2000 <- cdec_daily_FWQ[1001:2000,] %>% 
  filter(value_mean_C > 2.60)

ggplotly(
  ggplot() + geom_point(data=cdec_daily_FWQ_1001_2000, aes(x=date, y=value_mean_C)))

cdec_daily_FWQ_QA <- rbind(cdec_daily_FWQ_QA, cdec_daily_FWQ_1001_2000)

ggplotly(
  ggplot() + geom_point(data=cdec_daily_FWQ[2001:3000,], aes(x=date, y=value_mean_C)))

cdec_daily_FWQ_2001_3000 <- cdec_daily_FWQ[2001:3000,] %>% 
  filter(date != "2017-08-09", date != "2017-07-09", date != "2017-07-10", date != "2017-07-11", date != "2017-07-12", date != "2017-07-18")

ggplotly(
  ggplot() + geom_point(data=cdec_daily_FWQ_2001_3000, aes(x=date, y=value_mean_C)))

cdec_daily_FWQ_QA <- rbind(cdec_daily_FWQ_QA, cdec_daily_FWQ_2001_3000)

ggplotly(
  ggplot() + geom_point(data=cdec_daily_FWQ[3001:4000,], aes(x=date, y=value_mean_C)))

cdec_daily_FWQ_3001_4000 <- cdec_daily_FWQ[3001:4000,] %>% 
  filter(date != "2018-10-09", date != "2019-04-21")

ggplotly(
  ggplot() + geom_point(data=cdec_daily_FWQ_3001_4000, aes(x=date, y=value_mean_C)))

cdec_daily_FWQ_QA <- rbind(cdec_daily_FWQ_QA, cdec_daily_FWQ_3001_4000)

# Final review ------------------------------------------------------------

#plot QA'd dataset to confirm all points look good
ggplotly(
  ggplot() +geom_point(data = cdec_daily_FWQ_QA, aes(x=date, y=value_mean_C)))

#save QA'd dataset as a .rds file
write_rds(cdec_daily_FWQ_QA, path = "data/QA_data/cdec_daily_FWQ_QA.rds")

#update the gage_QA_progress
gage_QA_progress <- read_csv("data/data_review/gage_QA_progress.csv")

#note reviewer initials, whether review is complete, and any final notes
gage_QA_progress[gage_QA_progress$site_id=="FWQ",4:6] <- c("ADW", "Y", "QA complete")

#save updated dataframe to the .csv
write_csv(gage_QA_progress, path = "data/data_review/gage_QA_progress.csv")
