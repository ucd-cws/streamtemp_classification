
# Code description --------------------------------------------------------

# Code to review temperature data from gage usgs_11447650.

library(tidyverse)
library(lubridate)
library(weathermetrics)
library(plotly)

# Get Data ----------------------------------------------------------------

file_list <- list.files("data/data_review/")

# read in next file:
file_list[[88]]

usgs_daily_11447650 <- read_rds(path = paste0("data/data_review/",file_list[[88]]))


# Plot --------------------------------------------------------------------

# now make an interactive plot of first 1000 values
ggplotly(
  ggplot() + geom_point(data=usgs_daily_11447650[1:1000,], aes(x=Date, y=X_00010_00003)))

# Review, QA, and Repeat --------------------------------------------------

usgs_daily_11447650_QA <- usgs_daily_11447650[1:1000,]

ggplotly(
  ggplot() + geom_point(data=usgs_daily_11447650[1001:2000,], aes(x=Date, y=X_00010_00003)))

usgs_daily_11447650_QA <- usgs_daily_11447650[1:2000,]

ggplotly(
  ggplot() + geom_point(data=usgs_daily_11447650[2001:3000,], aes(x=Date, y=X_00010_00003)))

usgs_daily_11447650_QA <- usgs_daily_11447650[1:3000,]

ggplotly(
  ggplot() + geom_point(data=usgs_daily_11447650[3001:4000,], aes(x=Date, y=X_00010_00003)))

usgs_daily_11447650_QA <- usgs_daily_11447650[1:4000,]

ggplotly(
  ggplot() + geom_point(data=usgs_daily_11447650[4001:5000,], aes(x=Date, y=X_00010_00003)))

usgs_daily_11447650_QA <- usgs_daily_11447650[1:5000,]

ggplotly(
  ggplot() + geom_point(data=usgs_daily_11447650[5001:6000,], aes(x=Date, y=X_00010_00003)))

usgs_daily_11447650_QA <- usgs_daily_11447650[1:6000,]

ggplotly(
  ggplot() + geom_point(data=usgs_daily_11447650[6001:7000,], aes(x=Date, y=X_00010_00003)))

usgs_daily_11447650_QA <- usgs_daily_11447650[1:7000,]

ggplotly(
  ggplot() + geom_point(data=usgs_daily_11447650[7001:8000,], aes(x=Date, y=X_00010_00003)))

usgs_daily_11447650_QA <- usgs_daily_11447650[1:8000,]

ggplotly(
  ggplot() + geom_point(data=usgs_daily_11447650[8001:9035,], aes(x=Date, y=X_00010_00003)))

usgs_daily_11447650_QA <- usgs_daily_11447650[1:9035,]

# Final review ------------------------------------------------------------

#plot QA'd dataset to confirm all points look good
ggplotly(
  ggplot() +geom_point(data = usgs_daily_11447650_QA, aes(x=Date, y=X_00010_00003)))

# Data is not continuous; however, we still have enough data for the analysis, so we will keep the site.

# #save QA'd dataset as a .rds file
write_rds(usgs_daily_11447650_QA, path = "data/QA_data/usgs_daily_11447650_QA.rds")

#update the gage_QA_progress
gage_QA_progress <- read_csv("data/data_review/gage_QA_progress.csv")

#confirm correct row to update by the site_id
gage_QA_progress[194,1]

#note reviewer initials, whether review is complete, and any final notes
gage_QA_progress[194,4:6] <- c("ADW", "Y", "QA complete")

#save updated dataframe to the .csv
write_csv(gage_QA_progress, path = "data/data_review/gage_QA_progress.csv")
