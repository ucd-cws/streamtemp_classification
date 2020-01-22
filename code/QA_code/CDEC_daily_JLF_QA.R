
# Code description --------------------------------------------------------

# Code to review temperature data from gage CDEC_ING.

library(tidyverse)
library(lubridate)
library(weathermetrics)
library(plotly)

# Get Data ----------------------------------------------------------------

file_list <- list.files("data/data_review/")

# read in next file:
file_list[[32]]

cdec_daily_ING <- read_rds(path = paste0("data/data_review/",file_list[[32]]))

# Plot --------------------------------------------------------------------

#plot all data to look for obvious bad data points and gaps
ggplotly(
  ggplot() + geom_point(data=cdec_daily_ING[,], aes(x=date, y=value_mean_C)))

# now make an interactive plot of first 1000 values
ggplotly(
  ggplot() + geom_point(data=cdec_daily_ING[1:1000,], aes(x=date, y=value_mean_C)))

# Review, QA, and Repeat --------------------------------------------------

#not enought data to include in the analysis - only four complete years. No data file saved.

# Final review ------------------------------------------------------------

#plot QA'd dataset to confirm all points look good
ggplotly(
  ggplot() +geom_point(data = cdec_daily_ING_QA, aes(x=date, y=value_mean_C)))

#save QA'd dataset as a .rds file
write_rds(cdec_daily_ING_QA, path = "data/QA_data/cdec_daily_ING_QA.rds")

#update the gage_QA_progress
gage_QA_progress <- read_csv("data/data_review/gage_QA_progress.csv")

#note reviewer initials, whether review is complete, and any final notes
gage_QA_progress[gage_QA_progress$site_id=="ING",4:6] <- c("ADW", "Y", "Insufficient data available for analysis. No data file saved, gage dropped from study.")

#save updated dataframe to the .csv
write_csv(gage_QA_progress, path = "data/data_review/gage_QA_progress.csv")
