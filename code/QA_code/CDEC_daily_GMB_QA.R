
# Code description --------------------------------------------------------

# Code to review temperature data from gage CDEC_GMB.

library(tidyverse)
library(lubridate)
library(weathermetrics)
library(plotly)

# Get Data ----------------------------------------------------------------

file_list <- list.files("data/data_review/")

# read in next file:
file_list[[23]]

cdec_daily_GMB <- read_rds(path = paste0("data/data_review/",file_list[[23]]))

# Plot --------------------------------------------------------------------

#plot all data to look for obvious bad data points and gaps
ggplotly(
  ggplot() + geom_point(data=cdec_daily_GMB[,], aes(x=date, y=value_mean_C)))

# now make an interactive plot of first 1000 values
ggplotly(
  ggplot() + geom_point(data=cdec_daily_GMB[1:1000,], aes(x=date, y=value_mean_C)))

# Review, QA, and Repeat --------------------------------------------------
cdec_daily_GMB_QA <- cdec_daily_GMB[1:1000,]

ggplotly(
  ggplot() + geom_point(data=cdec_daily_GMB[1001:2000,], aes(x=date, y=value_mean_C)))

cdec_daily_GMB_1001_2000 <- cdec_daily_GMB[1001:2000,] %>% 
  filter(date != "2006-09-06", date != "2006-09-07", date != "2006-09-08", date != "2006-09-09", date != "2006-09-10")

ggplotly(
  ggplot() + geom_point(data=cdec_daily_GMB_1001_2000, aes(x=date, y=value_mean_C)))

cdec_daily_GMB_QA <- rbind(cdec_daily_GMB_QA, cdec_daily_GMB_1001_2000)

ggplotly(
  ggplot() + geom_point(data=cdec_daily_GMB[2001:3000,], aes(x=date, y=value_mean_C)))

cdec_daily_GMB_QA <- rbind(cdec_daily_GMB_QA, cdec_daily_GMB[2001:3000,])

ggplotly(
  ggplot() + geom_point(data=cdec_daily_GMB[3001:3218,], aes(x=date, y=value_mean_C)))

cdec_daily_GMB_3001_3218 <- cdec_daily_GMB[3001:3217,] %>% 
  filter(date != "2011-02-17")

ggplotly(
  ggplot() + geom_point(data=cdec_daily_GMB_3001_3218, aes(x=date, y=value_mean_C)))

cdec_daily_GMB_QA <- rbind(cdec_daily_GMB_QA, cdec_daily_GMB_3001_3218)

# Final review ------------------------------------------------------------

#plot QA'd dataset to confirm all points look good
ggplotly(
  ggplot() +geom_point(data = cdec_daily_GMB_QA, aes(x=date, y=value_mean_C)))

#save QA'd dataset as a .rds file
write_rds(cdec_daily_GMB_QA, path = "data/QA_data/cdec_daily_GMB_QA.rds")

#update the gage_QA_progress
gage_QA_progress <- read_csv("data/data_review/gage_QA_progress.csv")

#note reviewer initials, whether review is complete, and any final notes
gage_QA_progress[gage_QA_progress$site_id=="GMB",4:6] <- c("ADW", "Y", "QA complete")

#save updated dataframe to the .csv
write_csv(gage_QA_progress, path = "data/data_review/gage_QA_progress.csv")
