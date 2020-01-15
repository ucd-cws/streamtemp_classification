
# Code description --------------------------------------------------------

# Code to review temperature data from gage CDEC_BCD.

library(tidyverse)
library(lubridate)
library(weathermetrics)
library(plotly)

# Get Data ----------------------------------------------------------------

file_list <- list.files("data/data_review/")

# read in next file:
file_list[[5]]

cdec_daily_BCD <- read_rds(path = paste0("data/data_review/",file_list[[5]]))

# Plot --------------------------------------------------------------------

#plot all data to look for obvious bad data points and gaps
ggplotly(
  ggplot() + geom_point(data=cdec_daily_BCD[,], aes(x=date, y=value_mean_C)))

# now make an interactive plot of first 1000 values
ggplotly(
  ggplot() + geom_point(data=cdec_daily_BCD[1:1000,], aes(x=date, y=value_mean_C)))

# Review, QA, and Repeat --------------------------------------------------
cdec_daily_BCD_1_1000 <- cdec_daily_BCD[1:1000,]

cdec_daily_BCD_1_1000 <- cdec_daily_BCD_1_1000[-c(146:150),]

ggplotly(
  ggplot() + geom_point(data=cdec_daily_BCD_1_1000, aes(x=date, y=value_mean_C)))

cdec_daily_BCD_QA <- cdec_daily_BCD_1_1000

ggplotly(
  ggplot() + geom_point(data=cdec_daily_BCD[1001:2000,], aes(x=date, y=value_mean_C)))

cdec_daily_BCD_QA <- rbind(cdec_daily_BCD_QA,cdec_daily_BCD[1001:2000,])

ggplotly(
  ggplot() + geom_point(data=cdec_daily_BCD[2001:3000,], aes(x=date, y=value_mean_C)))

cdec_daily_BCD_2001_3000 <- cdec_daily_BCD[2001:3000,]

cdec_daily_BCD_2001_3000 <- cdec_daily_BCD_2001_3000[-c(681:743),]

cdec_daily_BCD_2001_3000 <- filter(cdec_daily_BCD_2001_3000, date != "2004-07-19")

ggplotly(
  ggplot() + geom_point(data=cdec_daily_BCD_2001_3000, aes(x=date, y=value_mean_C)))

cdec_daily_BCD_QA <- rbind(cdec_daily_BCD_QA,cdec_daily_BCD_2001_3000)

ggplotly(
  ggplot() + geom_point(data=cdec_daily_BCD[3001:4000,], aes(x=date, y=value_mean_C)))

cdec_daily_BCD_3001_4000 <- cdec_daily_BCD[3001:4000,] %>% 
  filter(date != "2007-08-07")

ggplotly(
  ggplot() + geom_point(data=cdec_daily_BCD_3001_4000, aes(x=date, y=value_mean_C)))

cdec_daily_BCD_QA <- rbind(cdec_daily_BCD_QA, cdec_daily_BCD_3001_4000)

ggplotly(
  ggplot() + geom_point(data=cdec_daily_BCD[4001:5000,], aes(x=date, y=value_mean_C)))

cdec_daily_BCD_QA <- rbind(cdec_daily_BCD_QA, cdec_daily_BCD[4001:5000,])

ggplotly(
  ggplot() + geom_point(data=cdec_daily_BCD[5001:6000,], aes(x=date, y=value_mean_C)))

cdec_daily_BCD_QA <- rbind(cdec_daily_BCD_QA,cdec_daily_BCD[5001:6000,])

ggplotly(
  ggplot() + geom_point(data=cdec_daily_BCD[6001:7000,], aes(x=date, y=value_mean_C)))

cdec_daily_BCD_6001_7000 <- cdec_daily_BCD[6001:7000,]

cdec_daily_BCD_6001_7000 <- cdec_daily_BCD_6001_7000[-c(253:365),]

ggplotly(
  ggplot() + geom_point(data=cdec_daily_BCD_6001_7000, aes(x=date, y=value_mean_C)))

cdec_daily_BCD_QA <- rbind(cdec_daily_BCD_QA,cdec_daily_BCD_6001_7000)

ggplotly(
  ggplot() + geom_point(data=cdec_daily_BCD[7001:7670,], aes(x=date, y=value_mean_C)))

cdec_daily_BCD_QA <- rbind(cdec_daily_BCD_QA,cdec_daily_BCD[7001:7670,])

# Final review ------------------------------------------------------------

#plot QA'd dataset to confirm all points look good
ggplotly(
  ggplot() +geom_point(data = cdec_daily_BCD_QA, aes(x=date, y=value_mean_C)))

#save QA'd dataset as a .rds file
write_rds(cdec_daily_BCD_QA, path = "data/QA_data/cdec_daily_BCD_QA.rds")

#update the gage_QA_progress
gage_QA_progress <- read_csv("data/data_review/gage_QA_progress.csv")

#note reviewer initials, whether review is complete, and any final notes
gage_QA_progress[gage_QA_progress$site_id=="BCD",4:6] <- c("ADW", "Y", "QA complete")

#save updated dataframe to the .csv
write_csv(gage_QA_progress, path = "data/data_review/gage_QA_progress.csv")
