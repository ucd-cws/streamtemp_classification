
# Code description --------------------------------------------------------

# Code to review temperature data from gage CDEC_BBQ.

library(tidyverse)
library(lubridate)
library(weathermetrics)
library(plotly)

# Get Data ----------------------------------------------------------------

file_list <- list.files("data/data_review/")

# read in next file:
file_list[[10]]

cdec_hourly_BBQ <- read_rds(path = paste0("data/data_review/",file_list[[10]]))

# Plot --------------------------------------------------------------------

# now make an interactive plot of first 1000 values
ggplotly(
  ggplot() + geom_point(data=cdec_hourly_BBQ[1:20000,], aes(x=datetime, y=value)))

# Review, QA, and Repeat --------------------------------------------------
cdec_hourly_BBQ_QA <- cdec_hourly_BBQ[1:20000,]

ggplotly(
  ggplot() + geom_point(data=cdec_hourly_BBQ[20001:40000,], aes(x=datetime, y=value)))

cdec_hourly_BBQ_QA <- cdec_hourly_BBQ[1:40000,]

ggplotly(
  ggplot() + geom_point(data=cdec_hourly_BBQ[40001:60000,], aes(x=datetime, y=value)))

cdec_hourly_BBQ_QA <- cdec_hourly_BBQ[1:60000,]

ggplotly(
  ggplot() + geom_point(data=cdec_hourly_BBQ[60001:80000,], aes(x=datetime, y=value)))

cdec_hourly_BBQ_QA <- cdec_hourly_BBQ[1:80000,]

ggplotly(
  ggplot() + geom_point(data=cdec_hourly_BBQ[80001:100000,], aes(x=datetime, y=value)))

cdec_hourly_BBQ_QA <- cdec_hourly_BBQ[1:100000,]

ggplotly(
  ggplot() + geom_point(data=cdec_hourly_BBQ[100001:120000,], aes(x=datetime, y=value)))

cdec_hourly_BBQ_QA <- cdec_hourly_BBQ[1:120000,]

ggplotly(
  ggplot() + geom_point(data=cdec_hourly_BBQ[120001:140000,], aes(x=datetime, y=value)))

cdec_hourly_BBQ_120001_140000 <- cdec_hourly_BBQ[120001:140000,]

cdec_hourly_BBQ_120001_140000 <- cdec_hourly_BBQ_120001_140000[-c(18779:19265),]

cdec_hourly_BBQ_120001_140000 <- cdec_hourly_BBQ_120001_140000[-c(19203:19453),]

ggplotly(
  ggplot() + geom_point(data=cdec_hourly_BBQ_120001_140000, aes(x=datetime, y=value)))

cdec_hourly_BBQ_QA <- rbind(cdec_hourly_BBQ_QA, cdec_hourly_BBQ_120001_140000)

ggplotly(
  ggplot() + geom_point(data=cdec_hourly_BBQ[140001:160000,], aes(x=datetime, y=value)))

#too many air temperature days in this plot; convert all temps to daily mean first, then QA

# Final review ------------------------------------------------------------

#plot QA'd dataset to confirm all points look good
ggplotly(
  ggplot() +geom_point(data = cdec_hourly_BBQ_QA, aes(x=datetime, y=value)))

#save QA'd dataset as a .rds file
write_rds(cdec_hourly_BBQ_QA, path = "data/QA_data/cdec_hourly_BBQ_QA.rds", compress = "gz")

#update the gage_QA_progress
gage_QA_progress <- read_csv("data/data_review/gage_QA_progress.csv")

#note reviewer initials, whether review is complete, and any final notes
gage_QA_progress[gage_QA_progress$site_id=="BBQ",4:6] <- c("ADW", "N", "too many air temp days in dataset, need to convert to daily mean first")

#save updated dataframe to the .csv
write_csv(gage_QA_progress, path = "data/data_review/gage_QA_progress.csv")
