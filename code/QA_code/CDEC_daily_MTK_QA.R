
# Code description --------------------------------------------------------

# Code to review temperature data from gage CDEC_MTK.

library(tidyverse)
library(lubridate)
library(weathermetrics)
library(plotly)

# Get Data ----------------------------------------------------------------

file_list <- list.files("data/data_review/", pattern = ".rds")

# read in next file:
file_list[[40]]

cdec_daily_MTK <- read_rds(path = paste0("data/data_review/",file_list[[40]]))

# Plot --------------------------------------------------------------------

#plot all data to look for obvious bad data points and gaps
ggplotly(
  ggplot() + geom_point(data=cdec_daily_MTK[,], aes(x=date, y=value_mean_C)))

# now make an interactive plot of first 1000 values
ggplotly(
  ggplot() + geom_point(data=cdec_daily_MTK[1:1000,], aes(x=date, y=value_mean_C)))

# Review, QA, and Repeat --------------------------------------------------
cdec_daily_MTK_1_1000 <- cdec_daily_MTK[1:1000,] %>% 
  filter(value_mean_C > 0.00, value_mean_C < 20, date != "2004-08-16", date != "2004-08-30", date != "2005-06-29", date != "2005-07-29", date != "2005-08-29", date != "2005-10-03", date != "2006-11-05", date != "2006-11-06")

ggplotly(
  ggplot() + geom_point(data=cdec_daily_MTK_1_1000, aes(x=date, y=value_mean_C)))

cdec_daily_MTK_QA <- cdec_daily_MTK_1_1000

ggplotly(
  ggplot() + geom_point(data=cdec_daily_MTK[1001:2000,], aes(x=date, y=value_mean_C)))

cdec_daily_MTK_QA <- rbind(cdec_daily_MTK_QA, cdec_daily_MTK[1001:2000,])

ggplotly(
  ggplot() + geom_point(data=cdec_daily_MTK[2001:3000,], aes(x=date, y=value_mean_C)))

cdec_daily_MTK_QA <- rbind(cdec_daily_MTK_QA, cdec_daily_MTK[2001:3000,])

ggplotly(
  ggplot() + geom_point(data=cdec_daily_MTK[3001:4000,], aes(x=date, y=value_mean_C)))

cdec_daily_MTK_QA <- rbind(cdec_daily_MTK_QA, cdec_daily_MTK[3001:4000,])

ggplotly(
  ggplot() + geom_point(data=cdec_daily_MTK[4001:5000,], aes(x=date, y=value_mean_C)))

cdec_daily_MTK_4001_5000 <- cdec_daily_MTK[4001:5000,] %>% 
  filter(value_mean_C < 20.91)

ggplotly(
  ggplot() + geom_point(data=cdec_daily_MTK_4001_5000, aes(x=date, y=value_mean_C)))

cdec_daily_MTK_QA <- rbind(cdec_daily_MTK_QA, cdec_daily_MTK_4001_5000)

ggplotly(
  ggplot() + geom_point(data=cdec_daily_MTK[5001:5495,], aes(x=date, y=value_mean_C)))

cdec_daily_MTK_QA <- rbind(cdec_daily_MTK_QA, cdec_daily_MTK[5001:5495,])

# Final review ------------------------------------------------------------

#plot QA'd dataset to confirm all points look good
ggplotly(
  ggplot() +geom_point(data = cdec_daily_MTK_QA, aes(x=date, y=value_mean_C)))

#save QA'd dataset as a .rds file
write_rds(cdec_daily_MTK_QA, path = "data/QA_data/cdec_daily_MTK_QA.rds")

#update the gage_QA_progress
gage_QA_progress <- read_csv("data/data_review/gage_QA_progress.csv")

#note reviewer initials, whether review is complete, and any final notes
gage_QA_progress[gage_QA_progress$site_id=="MTK",6:8] <- c("ADW", "Y", "QA complete")

#save updated dataframe to the .csv
write_csv(gage_QA_progress, path = "data/data_review/gage_QA_progress.csv")
