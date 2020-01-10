
# Code description --------------------------------------------------------

# Code to review temperature data from gage usgs_11467000.

library(tidyverse)
library(lubridate)
library(weathermetrics)
library(plotly)

# Get Data ----------------------------------------------------------------

file_list <- list.files("data/data_review/")

# read in next file:
file_list[[89]]

usgs_daily_11467000 <- read_rds(path = paste0("data/data_review/",file_list[[89]]))


# Plot --------------------------------------------------------------------

# now make an interactive plot of first 1000 values
ggplotly(
  ggplot() + geom_point(data=usgs_daily_11467000[1:1000,], aes(x=Date, y=X_00010_00003)))

# Review, QA, and Repeat --------------------------------------------------

usgs_daily_11467000_QA <- usgs_daily_11467000[1:1000,]

ggplotly(
  ggplot() + geom_point(data=usgs_daily_11467000[1001:2000,], aes(x=Date, y=X_00010_00003)))

usgs_daily_11467000_QA <- usgs_daily_11467000[1:2000,]

ggplotly(
  ggplot() + geom_point(data=usgs_daily_11467000[2001:3000,], aes(x=Date, y=X_00010_00003)))

usgs_daily_11467000_QA <- usgs_daily_11467000[1:3000,]

ggplotly(
  ggplot() + geom_point(data=usgs_daily_11467000[3001:4150,], aes(x=Date, y=X_00010_00003)))

usgs_daily_11467000_QA <- usgs_daily_11467000[1:4150,]

# Final review ------------------------------------------------------------

#plot QA'd dataset to confirm all points look good
ggplotly(
  ggplot() +geom_point(data = usgs_daily_11467000_QA, aes(x=Date, y=X_00010_00003)))

# Data is not continuous; gap exists from 09-2010 until 10-2016; however, we still have enough data for the analysis, so we will keep the site.

# #save QA'd dataset as a .rds file
write_rds(usgs_daily_11467000_QA, path = "data/QA_data/usgs_daily_11467000_QA.rds")

#update the gage_QA_progress
gage_QA_progress <- read_csv("data/data_review/gage_QA_progress.csv")

#confirm correct row to update by the site_id
gage_QA_progress[210,1]

#note reviewer initials, whether review is complete, and any final notes
gage_QA_progress[210,4:6] <- c("ADW", "Y", "QA complete")

#save updated dataframe to the .csv
write_csv(gage_QA_progress, path = "data/data_review/gage_QA_progress.csv")
