
# Code description --------------------------------------------------------

# Code to review temperature data from gage CDEC_GRF.

library(tidyverse)
library(lubridate)
library(weathermetrics)
library(plotly)

# Get Data ----------------------------------------------------------------

file_list <- list.files("data/data_review/")

# read in next file:
file_list[[24]]

cdec_daily_GRF <- read_rds(path = paste0("data/data_review/",file_list[[24]]))

# Plot --------------------------------------------------------------------

#plot all data to look for obvious bad data points and gaps
ggplotly(
  ggplot() + geom_point(data=cdec_daily_GRF[,], aes(x=date, y=value_mean_C)))

# now make an interactive plot of first 1000 values
ggplotly(
  ggplot() + geom_point(data=cdec_daily_GRF[1:1000,], aes(x=date, y=value_mean_C)))

# Review, QA, and Repeat --------------------------------------------------
cdec_daily_GRF_QA <- cdec_daily_GRF[1:1000,]

ggplotly(
  ggplot() + geom_point(data=cdec_daily_GRF[1001:2000,], aes(x=date, y=value_mean_C)))

cdec_daily_GRF_1001_2000 <- cdec_daily_GRF[1001:2000,] %>% 
  filter(date != "2009-03-23", date != "2009-03-22", date != "2009-04-25", date != "2009-04-26", date != "2009-04-27", date != "2009-05-24", date != "2009-05-25", date != "2009-06-12", date != "2009-06-13", date != "2009-06-14", date != "2009-06-15", date != "2009-10-01", date != "2009-10-02", date != "2009-10-03", date != "2009-10-04")

ggplotly(
  ggplot() + geom_point(data=cdec_daily_GRF_1001_2000, aes(x=date, y=value_mean_C)))

cdec_daily_GRF_QA <- rbind(cdec_daily_GRF_QA, cdec_daily_GRF_1001_2000)

ggplotly(
  ggplot() + geom_point(data=cdec_daily_GRF[2001:3000,], aes(x=date, y=value_mean_C)))

cdec_daily_GRF_2001_3000 <- cdec_daily_GRF[2001:3000,] %>% 
  filter(date != "2011-09-06", date != "2011-09-07", value_mean_C > 4.60) 

ggplotly(
  ggplot() + geom_point(data=cdec_daily_GRF_2001_3000, aes(x=date, y=value_mean_C)))

cdec_daily_GRF_QA <- rbind(cdec_daily_GRF_QA, cdec_daily_GRF_2001_3000)

ggplotly(
  ggplot() + geom_point(data=cdec_daily_GRF[3001:4000,], aes(x=date, y=value_mean_C)))

cdec_daily_GRF_3001_4000 <- cdec_daily_GRF[3001:4000,] %>% 
  filter(value_mean_C > 0, date != "2014-10-12", date != "2014-10-14")

cdec_daily_GRF_3001_4000 <- cdec_daily_GRF_3001_4000[-c(555:596),]

ggplotly(
  ggplot() + geom_point(data=cdec_daily_GRF_3001_4000, aes(x=date, y=value_mean_C)))

cdec_daily_GRF_QA <- rbind(cdec_daily_GRF_QA, cdec_daily_GRF_3001_4000)

ggplotly(
  ggplot() + geom_point(data=cdec_daily_GRF[4001:5000,], aes(x=date, y=value_mean_C)))

cdec_daily_GRF_QA <- rbind(cdec_daily_GRF_QA, cdec_daily_GRF[4001:5000,])

ggplotly(
  ggplot() + geom_point(data=cdec_daily_GRF[5001:5532,], aes(x=date, y=value_mean_C)))

cdec_daily_GRF_QA <- rbind(cdec_daily_GRF_QA, cdec_daily_GRF[5001:5532,])

# Final review ------------------------------------------------------------

#plot QA'd dataset to confirm all points look good
ggplotly(
  ggplot() +geom_point(data = cdec_daily_GRF_QA, aes(x=date, y=value_mean_C)))

#save QA'd dataset as a .rds file
write_rds(cdec_daily_GRF_QA, path = "data/QA_data/cdec_daily_GRF_QA.rds")

#update the gage_QA_progress
gage_QA_progress <- read_csv("data/data_review/gage_QA_progress.csv")

#note reviewer initials, whether review is complete, and any final notes
gage_QA_progress[gage_QA_progress$site_id=="GRF",4:6] <- c("ADW", "Y", "QA complete")

#save updated dataframe to the .csv
write_csv(gage_QA_progress, path = "data/data_review/gage_QA_progress.csv")
