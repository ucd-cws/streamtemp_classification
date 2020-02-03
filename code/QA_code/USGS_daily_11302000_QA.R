
# Code description --------------------------------------------------------

# Code to review temperature data from gage USGS_11302000.

library(tidyverse)
library(lubridate)
library(weathermetrics)
library(plotly)

# Get Data ----------------------------------------------------------------

file_list <- list.files("data/data_review/", pattern = ".rds")

# read in next file:
file_list[[82]]

usgs_daily_11302000 <- read_rds(path = paste0("data/data_review/",file_list[[82]]))

# Plot --------------------------------------------------------------------

#plot all data to look for obvious bad data points and gaps
ggplotly(
  ggplot() + geom_point(data=usgs_daily_11302000[,], aes(x=date, y=value_mean_C)))

#Insufficient data for complete analysis. Site dropped from study.

ggplotly(
  ggplot() + geom_point(data=usgs_daily_11302000[1:1000,], aes(x=date, y=value_mean_C)))

# Review, QA, and Repeat --------------------------------------------------




# Final review ------------------------------------------------------------

#plot QA'd dataset to confirm all points look good
ggplotly(
  ggplot() +geom_point(data = usgs_daily_11302000_QA, aes(x=date, y=value_mean_C)))

#save QA'd dataset as a .rds file
write_rds(usgs_daily_11302000_QA, path = "data/QA_data/usgs_daily_11302000_QA.rds")

#update the gage_QA_progress
gage_QA_progress <- read_csv("data/data_review/gage_QA_progress.csv")

#note reviewer initials, whether review is complete, and any final notes
gage_QA_progress[gage_QA_progress$site_id=="11302000",6:8] <- c("ADW", "Y", "Insufficient data for complete analysis. Site dropped from study.")

#save updated dataframe to the .csv
write_csv(gage_QA_progress, path = "data/data_review/gage_QA_progress.csv")

gage_QA_progress %>% 
  count(completed_Y_N)
