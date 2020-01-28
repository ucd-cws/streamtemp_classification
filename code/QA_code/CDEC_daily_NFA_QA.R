
# Code description --------------------------------------------------------

# Code to review temperature data from gage CDEC_NFA.

library(tidyverse)
library(lubridate)
library(weathermetrics)
library(plotly)

# Get Data ----------------------------------------------------------------

file_list <- list.files("data/data_review/", pattern = ".rds")

# read in next file:
file_list[[41]]

cdec_daily_NFA <- read_rds(path = paste0("data/data_review/",file_list[[41]]))

# Plot --------------------------------------------------------------------

#plot all data to look for obvious bad data points and gaps
ggplotly(
  ggplot() + geom_point(data=cdec_daily_NFA[,], aes(x=date, y=value_mean_C)))

# now make an interactive plot of first 1000 values
ggplotly(
  ggplot() + geom_point(data=cdec_daily_NFA[1:1000,], aes(x=date, y=value_mean_C)))

# Review, QA, and Repeat --------------------------------------------------
cdec_daily_NFA_QA <- cdec_daily_NFA[1:1000,]

ggplotly(
  ggplot() + geom_point(data=cdec_daily_NFA[1001:2000,], aes(x=date, y=value_mean_C)))

cdec_daily_NFA_1001_2000 <- cdec_daily_NFA[1001:2000,] %>% 
  filter(value_mean_C > 5.28, value_mean_C < 31.56, date != "2003-08-07", date != "2004-03-26")

ggplotly(
  ggplot() + geom_point(data=cdec_daily_NFA_1001_2000, aes(x=date, y=value_mean_C)))

cdec_daily_NFA_QA <- rbind(cdec_daily_NFA_QA, cdec_daily_NFA_1001_2000)

ggplotly(
  ggplot() + geom_point(data=cdec_daily_NFA[2001:3000,], aes(x=date, y=value_mean_C)))

cdec_daily_NFA_2001_3000 <- cdec_daily_NFA[2001:3000,] %>% 
  filter(value_mean_C < 24.27, date != "2007-08-21", date != "2006-02-17")

ggplotly(
  ggplot() + geom_point(data=cdec_daily_NFA_2001_3000, aes(x=date, y=value_mean_C)))

cdec_daily_NFA_QA <- rbind(cdec_daily_NFA_QA, cdec_daily_NFA_2001_3000)

ggplotly(
  ggplot() + geom_point(data=cdec_daily_NFA[3001:4000,], aes(x=date, y=value_mean_C)))

cdec_daily_NFA_QA <- rbind(cdec_daily_NFA_QA, cdec_daily_NFA[3001:4000,])

ggplotly(
  ggplot() + geom_point(data=cdec_daily_NFA[4001:5000,], aes(x=date, y=value_mean_C)))

cdec_daily_NFA_QA <- rbind(cdec_daily_NFA_QA, cdec_daily_NFA[4001:5000,])

ggplotly(
  ggplot() + geom_point(data=cdec_daily_NFA[5001:6000,], aes(x=date, y=value_mean_C)))

cdec_daily_NFA_QA <- rbind(cdec_daily_NFA_QA, cdec_daily_NFA[5001:6000,])

ggplotly(
  ggplot() + geom_point(data=cdec_daily_NFA[6001:7099,], aes(x=date, y=value_mean_C)))

cdec_daily_NFA_6001_7099 <- cdec_daily_NFA[6001:7099,] %>% 
  filter(date != "2017-06-14")

cdec_daily_NFA_6001_7099 <- cdec_daily_NFA_6001_7099[-c(175:214),]

ggplotly(
  ggplot() + geom_point(data=cdec_daily_NFA_6001_7099, aes(x=date, y=value_mean_C)))

cdec_daily_NFA_QA <- rbind(cdec_daily_NFA_QA, cdec_daily_NFA_6001_7099)

# Final review ------------------------------------------------------------

#plot QA'd dataset to confirm all points look good
ggplotly(
  ggplot() +geom_point(data = cdec_daily_NFA_QA, aes(x=date, y=value_mean_C)))

#save QA'd dataset as a .rds file
write_rds(cdec_daily_NFA_QA, path = "data/QA_data/cdec_daily_NFA_QA.rds")

#update the gage_QA_progress
gage_QA_progress <- read_csv("data/data_review/gage_QA_progress.csv")

#note reviewer initials, whether review is complete, and any final notes
gage_QA_progress[gage_QA_progress$site_id=="NFA",6:8] <- c("ADW", "Y", "QA complete")

#save updated dataframe to the .csv
write_csv(gage_QA_progress, path = "data/data_review/gage_QA_progress.csv")
