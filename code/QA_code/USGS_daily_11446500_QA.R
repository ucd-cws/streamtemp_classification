
# Code description --------------------------------------------------------

# Code to review temperature data from gage usgs_11446500.

library(tidyverse)
library(lubridate)
library(weathermetrics)
library(plotly)

# Get Data ----------------------------------------------------------------

file_list <- list.files("data/data_review/")

# read in next file:
file_list[[87]]

usgs_daily_11446500 <- read_rds(path = paste0("data/data_review/",file_list[[87]]))


# Plot --------------------------------------------------------------------

# now make an interactive plot of first 1000 values
ggplotly(
  ggplot() + geom_point(data=usgs_daily_11446500[1:1881,], aes(x=Date, y=X_00010_00003)))

# Review, QA, and Repeat --------------------------------------------------

#Can't use gage - datagap extends from 1974 to 2016, not enough data for analysis. Gage dropped from study

# Final review ------------------------------------------------------------

#plot QA'd dataset to confirm all points look good
ggplotly(
  ggplot() +geom_point(data = usgs_daily_11446500_QA, aes(x=Date, y=X_00010_00003)))

# Data is not continuous; however, we still have enough data for the analysis, so we will keep the site.

# #save QA'd dataset as a .rds file
write_rds(usgs_daily_11446500_QA, path = "data/QA_data/usgs_daily_11446500_QA.rds")

#update the gage_QA_progress
gage_QA_progress <- read_csv("data/data_review/gage_QA_progress.csv")

#confirm correct row to update by the site_id
gage_QA_progress[194,1]

#note reviewer initials, whether review is complete, and any final notes
gage_QA_progress[194,4:6] <- c("ADW", "Y", "QA complete")

#save updated dataframe to the .csv
write_csv(gage_QA_progress, path = "data/data_review/gage_QA_progress.csv")
