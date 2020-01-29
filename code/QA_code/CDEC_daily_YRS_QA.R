
# Code description --------------------------------------------------------

# Code to review temperature data from gage CDEC_YRS.

library(tidyverse)
library(lubridate)
library(weathermetrics)
library(plotly)

# Get Data ----------------------------------------------------------------

file_list <- list.files("data/data_review/", pattern = ".rds")

# read in next file:
file_list[[64]]

cdec_daily_YRS <- read_rds(path = paste0("data/data_review/",file_list[[64]]))

# Plot --------------------------------------------------------------------

#plot all data to look for obvious bad data points and gaps
ggplotly(
  ggplot() + geom_point(data=cdec_daily_YRS[,], aes(x=date, y=value_mean_C)))

# now make an interactive plot of first 1000 values
ggplotly(
  ggplot() + geom_point(data=cdec_daily_YRS[1:1000,], aes(x=date, y=value_mean_C)))

# Review, QA, and Repeat --------------------------------------------------
cdec_daily_YRS_1_1000 <- cdec_daily_YRS[1:1000,] %>% 
  filter(date != "2001-05-09", date != "2001-09-11")

ggplotly(
  ggplot() + geom_point(data=cdec_daily_YRS_1_1000, aes(x=date, y=value_mean_C)))

cdec_daily_YRS_QA <- cdec_daily_YRS_1_1000

ggplotly(
  ggplot() + geom_point(data=cdec_daily_YRS[1001:2000,], aes(x=date, y=value_mean_C)))

cdec_daily_YRS_1001_2000 <- cdec_daily_YRS[1001:2000,] %>% 
  filter(date!= "2004-02-02", date != "2004-02-22", date != "2004-03-01", date != "2004-08-26", date != "2004-09-29")

ggplotly(
  ggplot() + geom_point(data=cdec_daily_YRS_1001_2000, aes(x=date, y=value_mean_C)))

cdec_daily_YRS_QA <- rbind(cdec_daily_YRS_QA, cdec_daily_YRS_1001_2000)

ggplotly(
  ggplot() + geom_point(data=cdec_daily_YRS[2001:3000,], aes(x=date, y=value_mean_C)))

cdec_daily_YRS_2001_3000 <- cdec_daily_YRS[2001:3000,] %>% 
  filter(value_mean_C > 6.71, date != "2007-07-26")

ggplotly(
  ggplot() + geom_point(data=cdec_daily_YRS_2001_3000, aes(x=date, y=value_mean_C)))

cdec_daily_YRS_QA <- rbind(cdec_daily_YRS_QA, cdec_daily_YRS_2001_3000)

ggplotly(
  ggplot() + geom_point(data=cdec_daily_YRS[3001:4000,], aes(x=date, y=value_mean_C)))

cdec_daily_YRS_3001_4000 <- cdec_daily_YRS[3001:4000,] %>% 
  filter(value_mean_C > 4.06, date != "2010-07-09", date != "2010-04-10", date != "2010-04-11", date != "2010-04-14")

ggplotly(
  ggplot() + geom_point(data=cdec_daily_YRS_3001_4000, aes(x=date, y=value_mean_C)))

cdec_daily_YRS_QA <- rbind(cdec_daily_YRS_QA, cdec_daily_YRS_3001_4000)

ggplotly(
  ggplot() + geom_point(data=cdec_daily_YRS[4001:5000,], aes(x=date, y=value_mean_C)))

cdec_daily_YRS_4001_5000 <- cdec_daily_YRS[4001:5000,] %>% 
  filter(value_mean_C > 5.88)

ggplotly(
  ggplot() + geom_point(data=cdec_daily_YRS_4001_5000, aes(x=date, y=value_mean_C)))

cdec_daily_YRS_QA <- rbind(cdec_daily_YRS_QA, cdec_daily_YRS_4001_5000)

ggplotly(
  ggplot() + geom_point(data=cdec_daily_YRS[5001:6000,], aes(x=date, y=value_mean_C)))

cdec_daily_YRS_QA <- rbind(cdec_daily_YRS_QA, cdec_daily_YRS[5001:6000,])
# Final review ------------------------------------------------------------

#plot QA'd dataset to confirm all points look good
ggplotly(
  ggplot() +geom_point(data = cdec_daily_YRS_QA, aes(x=date, y=value_mean_C)))

#save QA'd dataset as a .rds file
write_rds(cdec_daily_YRS_QA, path = "data/QA_data/cdec_daily_YRS_QA.rds")

#update the gage_QA_progress
gage_QA_progress <- read_csv("data/data_review/gage_QA_progress.csv")

#note reviewer initials, whether review is complete, and any final notes
gage_QA_progress[gage_QA_progress$site_id=="YRS",6:8] <- c("ADW", "Y", "QA complete")

#save updated dataframe to the .csv
write_csv(gage_QA_progress, path = "data/data_review/gage_QA_progress.csv")
