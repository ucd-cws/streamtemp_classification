
# Code description --------------------------------------------------------

# Code to review temperature data from gage CDEC_JLF.

library(tidyverse)
library(lubridate)
library(weathermetrics)
library(plotly)

# Get Data ----------------------------------------------------------------

file_list <- list.files("data/data_review/")

# read in next file:
file_list[[33]]

cdec_daily_JLF <- read_rds(path = paste0("data/data_review/",file_list[[33]]))

# Plot --------------------------------------------------------------------

#plot all data to look for obvious bad data points and gaps
ggplotly(
  ggplot() + geom_point(data=cdec_daily_JLF[,], aes(x=date, y=value_mean_C)))

# now make an interactive plot of first 1000 values
ggplotly(
  ggplot() + geom_point(data=cdec_daily_JLF[1:1000,], aes(x=date, y=value_mean_C)))

# Review, QA, and Repeat --------------------------------------------------
cdec_daily_JLF_1_1000 <- cdec_daily_JLF[1:1000,] %>% 
  filter(date != "1995-01-05", date != "1996-01-19")

ggplotly(
  ggplot() + geom_point(data=cdec_daily_JLF_1_1000, aes(x=date, y=value_mean_C)))

cdec_daily_JLF_QA <- cdec_daily_JLF_1_1000

ggplotly(
  ggplot() + geom_point(data=cdec_daily_JLF[1001:2000,], aes(x=date, y=value_mean_C)))

cdec_daily_JLF_1001_2000 <- cdec_daily_JLF[1001:2000,] %>% 
  filter(value_mean_C > 6.47, value_mean_C < 17.50)

ggplotly(
  ggplot() + geom_point(data=cdec_daily_JLF_1001_2000, aes(x=date, y=value_mean_C)))

cdec_daily_JLF_QA <- rbind(cdec_daily_JLF_QA, cdec_daily_JLF_1001_2000)

ggplotly(
  ggplot() + geom_point(data=cdec_daily_JLF[2001:3000,], aes(x=date, y=value_mean_C)))

cdec_daily_JLF_QA <- rbind(cdec_daily_JLF_QA, cdec_daily_JLF[2001:3000])

ggplotly(
  ggplot() + geom_point(data=cdec_daily_JLF[3001:4000,], aes(x=date, y=value_mean_C)))

cdec_daily_JLF_3001_4000 <- cdec_daily_JLF[3001:4000,] %>% 
  filter(value_mean_C < 16.76)

ggplotly(
  ggplot() + geom_point(data=cdec_daily_JLF_3001_4000, aes(x=date, y=value_mean_C)))

cdec_daily_JLF_QA <- rbind(cdec_daily_JLF_QA, cdec_daily_JLF_3001_4000)

ggplotly(
  ggplot() + geom_point(data=cdec_daily_JLF[4001:5000,], aes(x=date, y=value_mean_C)))

cdec_daily_JLF_4001_5000 <- cdec_daily_JLF[4001:5000,] %>% 
  filter(value_mean_C > 1.32, value_mean_C < 14.78, date != "2007-07-16", date != "2007-07-19")

ggplotly(
  ggplot() + geom_point(data=cdec_daily_JLF_4001_5000, aes(x=date, y=value_mean_C)))

cdec_daily_JLF_QA <- rbind(cdec_daily_JLF_QA, cdec_daily_JLF_4001_5000)

ggplotly(
  ggplot() + geom_point(data=cdec_daily_JLF[5001:6000,], aes(x=date, y=value_mean_C)))

cdec_daily_JLF_5001_6000 <- cdec_daily_JLF[5001:6000,] %>% 
  filter(date != "2008-06-19")

cdec_daily_JLF_QA <- rbind(cdec_daily_JLF_QA, cdec_daily_JLF_5001_6000)

ggplotly(
  ggplot() + geom_point(data=cdec_daily_JLF[6001:7000,], aes(x=date, y=value_mean_C)))

cdec_daily_JLF_QA <- rbind(cdec_daily_JLF_QA, cdec_daily_JLF[6001:7000,])

ggplotly(
  ggplot() + geom_point(data=cdec_daily_JLF[7001:8000,], aes(x=date, y=value_mean_C)))

cdec_daily_JLF_QA <- rbind(cdec_daily_JLF_QA, cdec_daily_JLF[7001:8000,])

ggplotly(
  ggplot() + geom_point(data=cdec_daily_JLF[8001:9172,], aes(x=date, y=value_mean_C)))

cdec_daily_JLF_8001_9172 <- cdec_daily_JLF[8001:9172,] %>% 
  filter(value_mean_C > 4.68, date != "2017-05-17", date != "2017-05-18", date != "2019-02-15")

ggplotly(
  ggplot() + geom_point(data=cdec_daily_JLF_8001_9172, aes(x=date, y=value_mean_C)))

cdec_daily_JLF_QA <- rbind(cdec_daily_JLF_QA, cdec_daily_JLF_8001_9172)

# Final review ------------------------------------------------------------

#plot QA'd dataset to confirm all points look good
ggplotly(
  ggplot() +geom_point(data = cdec_daily_JLF_QA, aes(x=date, y=value_mean_C)))

#save QA'd dataset as a .rds file
write_rds(cdec_daily_JLF_QA, path = "data/QA_data/cdec_daily_JLF_QA.rds")

#update the gage_QA_progress
gage_QA_progress <- read_csv("data/data_review/gage_QA_progress.csv")

#note reviewer initials, whether review is complete, and any final notes
gage_QA_progress[gage_QA_progress$site_id=="JLF",4:6] <- c("ADW", "Y", "QA complete")

#save updated dataframe to the .csv
write_csv(gage_QA_progress, path = "data/data_review/gage_QA_progress.csv")
