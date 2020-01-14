
# Code description --------------------------------------------------------

# Code to review temperature data from gage usgs_11523000.

library(tidyverse)
library(lubridate)
library(weathermetrics)
library(plotly)

# Get Data ----------------------------------------------------------------

file_list <- list.files("data/data_review/")

# read in next file:
file_list[[90]]

usgs_daily_11523000 <- read_rds(path = paste0("data/data_review/",file_list[[90]]))


# Plot --------------------------------------------------------------------

# now make an interactive plot of first 1000 values
ggplotly(
  ggplot() + geom_point(data=usgs_daily_11523000[1:1110,], aes(x=Date, y=X_00010_00003)))

# Review, QA, and Repeat --------------------------------------------------

#data unusable, almost a full gap until October 2016; commenting out all data export code to avoid accidentally including this site in the analysis

# Final review ------------------------------------------------------------

#plot QA'd dataset to confirm all points look good
# ggplotly(
#   ggplot() +geom_point(data = usgs_daily_11523000_QA, aes(x=datetime, y=value)))
# 
# #save QA'd dataset as a .rds file
# write_rds(usgs_daily_11523000_QA, path = "data/QA_data/usgs_daily_11523000_QA.rds")

#update the gage_QA_progress
gage_QA_progress <- read_csv("data/data_review/gage_QA_progress.csv")

#note reviewer initials, whether review is complete, and any final notes
gage_QA_progress[gage_QA_progress$site_id=="11523000",4:6] <- c("ADW", "Y", "data unusable, site dropped from analysis")

#save updated dataframe to the .csv
write_csv(gage_QA_progress, path = "data/data_review/gage_QA_progress.csv")
