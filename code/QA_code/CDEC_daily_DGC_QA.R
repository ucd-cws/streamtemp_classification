
# Code description --------------------------------------------------------

# Code to review temperature data from gage CDEC_DGC.

library(tidyverse)
library(lubridate)
library(weathermetrics)
library(plotly)

# Get Data ----------------------------------------------------------------

file_list <- list.files("data/data_review/")

# read in next file:
file_list[[15]]

cdec_daily_DGC <- read_rds(path = paste0("data/data_review/",file_list[[15]]))

# Plot --------------------------------------------------------------------

#plot all data to look for obvious bad data points and gaps
ggplotly(
  ggplot() + geom_point(data=cdec_daily_DGC[,], aes(x=date, y=value_mean_C)))

# now make an interactive plot of first 1000 values
ggplotly(
  ggplot() + geom_point(data=cdec_daily_DGC[1:1000,], aes(x=date, y=value_mean_C)))

# Review, QA, and Repeat --------------------------------------------------
cdec_daily_DGC_1_1000 <- cdec_daily_DGC[1:1000,] %>% 
  filter(date != "1996-01-27")

ggplotly(
  ggplot() + geom_point(data=cdec_daily_DGC_1_1000, aes(x=date, y=value_mean_C)))

cdec_daily_DGC_QA <- cdec_daily_DGC_1_1000

ggplotly(
  ggplot() + geom_point(data=cdec_daily_DGC[1001:2000,], aes(x=date, y=value_mean_C)))

cdec_daily_DGC_1001_2000 <- cdec_daily_DGC[1001:2000,] %>% 
  filter(value_mean_C > 3.10)

cdec_daily_DGC_1001_2000 <- cdec_daily_DGC_1001_2000[-c(319:355),]

ggplotly(
  ggplot() + geom_point(data=cdec_daily_DGC_1001_2000, aes(x=date, y=value_mean_C)))

cdec_daily_DGC_QA <- rbind(cdec_daily_DGC_QA, cdec_daily_DGC_1001_2000)

ggplotly(
  ggplot() + geom_point(data=cdec_daily_DGC[2001:3000,], aes(x=date, y=value_mean_C)))

cdec_daily_DGC_2001_3000 <- cdec_daily_DGC[2001:3000,] %>% 
  filter(date != "2001-09-11")

ggplotly(
  ggplot() + geom_point(data=cdec_daily_DGC_2001_3000, aes(x=date, y=value_mean_C)))

cdec_daily_DGC_QA <- rbind(cdec_daily_DGC_QA, cdec_daily_DGC_2001_3000)

ggplotly(
  ggplot() + geom_point(data=cdec_daily_DGC[3001:4000,], aes(x=date, y=value_mean_C)))

cdec_daily_DGC_3001_4000 <- cdec_daily_DGC[3001:4000,] %>% 
  filter(value_mean_C > 1.62, date != "2001-12-14", date != "2002-12-31", date != "2003-04-29")

ggplotly(
  ggplot() + geom_point(data=cdec_daily_DGC_3001_4000, aes(x=date, y=value_mean_C)))

cdec_daily_DGC_QA <- rbind(cdec_daily_DGC_QA, cdec_daily_DGC_3001_4000)
# Final review ------------------------------------------------------------

#plot QA'd dataset to confirm all points look good
ggplotly(
  ggplot() +geom_point(data = cdec_daily_DGC_QA, aes(x=date, y=value_mean_C)))

#save QA'd dataset as a .rds file
write_rds(cdec_daily_DGC_QA, path = "data/QA_data/cdec_daily_DGC_QA.rds")

#update the gage_QA_progress
gage_QA_progress <- read_csv("data/data_review/gage_QA_progress.csv")

#note reviewer initials, whether review is complete, and any final notes
gage_QA_progress[gage_QA_progress$site_id=="DGC",4:6] <- c("ADW", "Y", "QA complete")

#save updated dataframe to the .csv
write_csv(gage_QA_progress, path = "data/data_review/gage_QA_progress.csv")
