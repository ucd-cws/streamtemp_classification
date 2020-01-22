
# Code description --------------------------------------------------------

# Code to review temperature data from gage CDEC_HIQ.

library(tidyverse)
library(lubridate)
library(weathermetrics)
library(plotly)

# Get Data ----------------------------------------------------------------

file_list <- list.files("data/data_review/")

# read in next file:
file_list[[29]]

cdec_daily_HIQ <- read_rds(path = paste0("data/data_review/",file_list[[29]]))

# Plot --------------------------------------------------------------------

#plot all data to look for obvious bad data points and gaps
ggplotly(
  ggplot() + geom_point(data=cdec_daily_HIQ[,], aes(x=date, y=value_mean_C)))

# now make an interactive plot of first 1000 values
ggplotly(
  ggplot() + geom_point(data=cdec_daily_HIQ[1:1000,], aes(x=date, y=value_mean_C)))

# Review, QA, and Repeat --------------------------------------------------

# Temperature is erratic; gage is located just downstream of a dam outlet and shows huge fluctuations with stage (stage data plotted on CDEC). Will keep all data as is, but make a note in the table.

cdec_daily_HIQ_QA <- cdec_daily_HIQ[,]

# Final review ------------------------------------------------------------

#plot QA'd dataset to confirm all points look good
ggplotly(
  ggplot() +geom_point(data = cdec_daily_HIQ_QA, aes(x=date, y=value_mean_C)))

#save QA'd dataset as a .rds file
write_rds(cdec_daily_HIQ_QA, path = "data/QA_data/cdec_daily_HIQ_QA.rds")

#update the gage_QA_progress
gage_QA_progress <- read_csv("data/data_review/gage_QA_progress.csv")

#note reviewer initials, whether review is complete, and any final notes
gage_QA_progress[gage_QA_progress$site_id=="HIQ",4:6] <- c("ADW", "Y", "QA complete; no changes made, temp highly influenced by dam operations")

#save updated dataframe to the .csv
write_csv(gage_QA_progress, path = "data/data_review/gage_QA_progress.csv")
