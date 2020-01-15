
# Code description --------------------------------------------------------

# Code to review temperature data from gage CDEC_BBW.

library(tidyverse)
library(lubridate)
library(weathermetrics)
library(plotly)

# Get Data ----------------------------------------------------------------

file_list <- list.files("data/data_review/")

# read in next file:
file_list[[4]]

cdec_daily_BBW <- read_rds(path = paste0("data/data_review/",file_list[[4]]))

# Plot --------------------------------------------------------------------

#Plot all points to look for any big gaps/errors

ggplot() + geom_point(data=cdec_daily_BBW[1:15040,], aes(x=datetime, y=value_mean_C))

# now make an interactive plot of first 1000 values
ggplotly(
  ggplot() + geom_point(data=cdec_daily_BBW[1:1000,], aes(x=datetime, y=value_mean_C))) 

#there's something weird with this data - there are two points for each water day. Will review the code to convert everything to daily and then rerun and review.

# Review, QA, and Repeat --------------------------------------------------

# Final review ------------------------------------------------------------

#plot QA'd dataset to confirm all points look good
ggplotly(
  ggplot() +geom_point(data = cdec_daily_BBW_QA, aes(x=datetime, y=value_mean_C)))

#save QA'd dataset as a .rds file
write_rds(cdec_daily_BBW_QA, path = "data/QA_data/cdec_daily_BBW_QA.rds", compress = "gz")

#update the gage_QA_progress
gage_QA_progress <- read_csv("data/data_review/gage_QA_progress.csv")

#note reviewer initials, whether review is complete, and any final notes
gage_QA_progress[gage_QA_progress$site_id=="BBQ",4:6] <- c("ADW", "N", "too many air temp days in dataset, need to convert to daily mean first")

#save updated dataframe to the .csv
write_csv(gage_QA_progress, path = "data/data_review/gage_QA_progress.csv")
