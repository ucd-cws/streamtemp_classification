
# Code description --------------------------------------------------------

# Code to review temperature data from gage CDEC_SCQ.

library(tidyverse)
library(lubridate)
library(weathermetrics)
library(plotly)

# Get Data ----------------------------------------------------------------

file_list <- list.files("data/data_review/", pattern = ".rds")

# read in next file:
file_list[[49]]

cdec_daily_SCQ <- read_rds(path = paste0("data/data_review/",file_list[[49]]))

# Plot --------------------------------------------------------------------

#plot all data to look for obvious bad data points and gaps
ggplotly(
  ggplot() + geom_point(data=cdec_daily_SCQ[,], aes(x=date, y=value_mean_C)))

# now make an interactive plot of first 1000 values
ggplotly(
  ggplot() + geom_point(data=cdec_daily_SCQ[1:1000,], aes(x=date, y=value_mean_C)))

# Review, QA, and Repeat --------------------------------------------------
cdec_daily_SCQ_1_1000 <- cdec_daily_SCQ[1:1000,] %>% 
  filter(date != "2011-03-05", date != "2011-03-06", date != "2011-03-07", date != "2011-07-27", date != "2011-07-28", date != "2012-06-11", date != "2012-06-12", date != "2012-06-13", date != "2012-06-14")

ggplotly(
  ggplot() + geom_point(data=cdec_daily_SCQ_1_1000, aes(x=date, y=value_mean_C)))

cdec_daily_SCQ_QA <- cdec_daily_SCQ_1_1000

ggplotly(
  ggplot() + geom_point(data=cdec_daily_SCQ[1001:2000,], aes(x=date, y=value_mean_C)))

cdec_daily_SCQ_QA <- rbind(cdec_daily_SCQ_QA, cdec_daily_SCQ[1001:2000,])

ggplotly(
  ggplot() + geom_point(data=cdec_daily_SCQ[2001:3000,], aes(x=date, y=value_mean_C)))

cdec_daily_SCQ_2001_3000 <- cdec_daily_SCQ[2001:3000,] %>% 
  filter(date != "2017-07-22", date != "2017-07-23", date != "2017-07-24")

ggplotly(
  ggplot() + geom_point(data=cdec_daily_SCQ_2001_3000, aes(x=date, y=value_mean_C)))

ggplotly(
  ggplot() + geom_point(data=cdec_daily_SCQ[3001:3483,], aes(x=date, y=value_mean_C)))

cdec_daily_SCQ_QA <- rbind(cdec_daily_SCQ_QA, cdec_daily_SCQ[3001:3483])

# Final review ------------------------------------------------------------

#plot QA'd dataset to confirm all points look good
ggplotly(
  ggplot() +geom_point(data = cdec_daily_SCQ_QA, aes(x=date, y=value_mean_C)))

#save QA'd dataset as a .rds file
write_rds(cdec_daily_SCQ_QA, path = "data/QA_data/cdec_daily_SCQ_QA.rds")

#update the gage_QA_progress
gage_QA_progress <- read_csv("data/data_review/gage_QA_progress.csv")

#note reviewer initials, whether review is complete, and any final notes
gage_QA_progress[gage_QA_progress$site_id=="SCQ",6:8] <- c("ADW", "Y", "QA complete")

#save updated dataframe to the .csv
write_csv(gage_QA_progress, path = "data/data_review/gage_QA_progress.csv")
