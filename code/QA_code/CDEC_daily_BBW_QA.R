
# Code description --------------------------------------------------------

# Code to review temperature data from gage CDEC_BBW.

library(tidyverse)
library(lubridate)
library(weathermetrics)
library(plotly)

# Get Data ----------------------------------------------------------------

file_list <- list.files("data/data_review/")

# read in next file:
file_list[[4]]

cdec_daily_BBW <- read_rds(path = paste0("data/data_review/",file_list[[4]]))

# Plot --------------------------------------------------------------------

#Plot all points to look for any big gaps/errors
ggplotly(
  ggplot() + geom_point(data=cdec_daily_BBW[1:7549,], aes(x=date, y=value_mean_C)))

# now make an interactive plot of first 1000 values
ggplotly(
  ggplot() + geom_point(data=cdec_daily_BBW[1:1000,], aes(x=date, y=value_mean_C))) 

# Review, QA, and Repeat --------------------------------------------------
cdec_daily_BBW_QA <- cdec_daily_BBW[1:1000,]

ggplotly(
  ggplot() + geom_point(data=cdec_daily_BBW[1001:2000,], aes(x=date, y=value_mean_C))) 

cdec_daily_BBW_1001_2000 <- cdec_daily_BBW[1001:2000,] %>% 
  filter(value_mean_C > 0)

ggplotly(
  ggplot() + geom_point(data=cdec_daily_BBW_1001_2000, aes(x=date, y=value_mean_C))) 

cdec_daily_BBW_QA <- rbind(cdec_daily_BBW_QA, cdec_daily_BBW_1001_2000)

ggplotly(
  ggplot() + geom_point(data=cdec_daily_BBW[2001:3000,], aes(x=date, y=value_mean_C))) 

cdec_daily_BBW_QA <- rbind(cdec_daily_BBW_QA, cdec_daily_BBW[2001:3000,])

ggplotly(
  ggplot() + geom_point(data=cdec_daily_BBW[3001:4000,], aes(x=date, y=value_mean_C))) 

cdec_daily_BBW_QA <- rbind(cdec_daily_BBW_QA, cdec_daily_BBW[3001:4000,])

ggplotly(
  ggplot() + geom_point(data=cdec_daily_BBW[4001:5000,], aes(x=date, y=value_mean_C))) 

cdec_daily_BBW_QA <- rbind(cdec_daily_BBW_QA, cdec_daily_BBW[4001:5000,])

ggplotly(
  ggplot() + geom_point(data=cdec_daily_BBW[5001:6000,], aes(x=date, y=value_mean_C))) 

cdec_daily_BBW_QA <- rbind(cdec_daily_BBW_QA, cdec_daily_BBW[5001:6000,])

ggplotly(
  ggplot() + geom_point(data=cdec_daily_BBW[6001:7000,], aes(x=date, y=value_mean_C))) 

cdec_daily_BBW_6001_7000 <- cdec_daily_BBW[6001:7000,] %>% 
  filter(value_mean_C < 20, date != "2017-06-26", date != "2017-06-28")

ggplotly(
  ggplot() + geom_point(data=cdec_daily_BBW_6001_7000, aes(x=date, y=value_mean_C))) 

cdec_daily_BBW_QA <- rbind(cdec_daily_BBW_QA, cdec_daily_BBW_6001_7000)

ggplotly(
  ggplot() + geom_point(data=cdec_daily_BBW[7001:7549,], aes(x=date, y=value_mean_C))) 

cdec_daily_BBW_7001_7549 <- cdec_daily_BBW[7001:7549,]

cdec_daily_BBW_7001_7549 <- cdec_daily_BBW_7001_7549[-c(60:66),]

cdec_daily_BBW_7001_7549 <- cdec_daily_BBW_7001_7549[-c(374:388),]

ggplotly(
  ggplot() + geom_point(data=cdec_daily_BBW_7001_7549, aes(x=date, y=value_mean_C)))

cdec_daily_BBW_QA <- rbind(cdec_daily_BBW_QA, cdec_daily_BBW_7001_7549)
# Final review ------------------------------------------------------------

#plot QA'd dataset to confirm all points look good
ggplotly(
  ggplot() +geom_point(data = cdec_daily_BBW_QA, aes(x=date, y=value_mean_C)))

#save QA'd dataset as a .rds file
write_rds(cdec_daily_BBW_QA, path = "data/QA_data/cdec_daily_BBW_QA.rds")

#update the gage_QA_progress
gage_QA_progress <- read_csv("data/data_review/gage_QA_progress.csv")

#note reviewer initials, whether review is complete, and any final notes
gage_QA_progress[gage_QA_progress$site_id=="BBW",4:6] <- c("ADW", "Y", "QA complete")

#save updated dataframe to the .csv
write_csv(gage_QA_progress, path = "data/data_review/gage_QA_progress.csv")
