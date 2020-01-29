
# Code description --------------------------------------------------------

# Code to review temperature data from gage CDEC_ROR.

library(tidyverse)
library(lubridate)
library(weathermetrics)
library(plotly)

# Get Data ----------------------------------------------------------------

file_list <- list.files("data/data_review/", pattern = ".rds")

# read in next file:
file_list[[48]]

cdec_daily_ROR <- read_rds(path = paste0("data/data_review/",file_list[[48]]))

# Plot --------------------------------------------------------------------

#plot all data to look for obvious bad data points and gaps
ggplotly(
  ggplot() + geom_point(data=cdec_daily_ROR[,], aes(x=date, y=value_mean_C)))

# now make an interactive plot of first 1000 values
ggplotly(
  ggplot() + geom_point(data=cdec_daily_ROR[1:1000,], aes(x=date, y=value_mean_C)))

# Review, QA, and Repeat --------------------------------------------------
cdec_daily_ROR_1_1000 <- cdec_daily_ROR[1:1000,] %>% 
  filter(value_mean_C > 0.00, date != "2008-05-29", date != "2008-10-04", date != "2009-09-11", date != "2009-09-16")

ggplotly(
  ggplot() + geom_point(data=cdec_daily_ROR_1_1000, aes(x=date, y=value_mean_C)))

cdec_daily_ROR_QA <- cdec_daily_ROR_1_1000

ggplotly(
  ggplot() + geom_point(data=cdec_daily_ROR[1001:2000,], aes(x=date, y=value_mean_C)))

cdec_daily_ROR_QA <- rbind(cdec_daily_ROR_QA, cdec_daily_ROR[1001:2000,])

ggplotly(
  ggplot() + geom_point(data=cdec_daily_ROR[2001:3000,], aes(x=date, y=value_mean_C)))

cdec_daily_ROR_QA <- rbind(cdec_daily_ROR_QA, cdec_daily_ROR[2001:3000,])

ggplotly(
  ggplot() + geom_point(data=cdec_daily_ROR[3001:4157,], aes(x=date, y=value_mean_C)))

cdec_daily_ROR_QA <- rbind(cdec_daily_ROR_QA, cdec_daily_ROR[3001:4157,])
# Final review ------------------------------------------------------------

#plot QA'd dataset to confirm all points look good
ggplotly(
  ggplot() +geom_point(data = cdec_daily_ROR_QA, aes(x=date, y=value_mean_C)))

#save QA'd dataset as a .rds file
write_rds(cdec_daily_ROR_QA, path = "data/QA_data/cdec_daily_ROR_QA.rds")

#update the gage_QA_progress
gage_QA_progress <- read_csv("data/data_review/gage_QA_progress.csv")

#note reviewer initials, whether review is complete, and any final notes
gage_QA_progress[gage_QA_progress$site_id=="ROR",6:8] <- c("ADW", "Y", "QA complete")

#save updated dataframe to the .csv
write_csv(gage_QA_progress, path = "data/data_review/gage_QA_progress.csv")
