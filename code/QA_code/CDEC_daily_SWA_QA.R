
# Code description --------------------------------------------------------

# Code to review temperature data from gage CDEC_SWA.

library(tidyverse)
library(lubridate)
library(weathermetrics)
library(plotly)

# Get Data ----------------------------------------------------------------

file_list <- list.files("data/data_review/")

# read in next file:
file_list[[7]]

cdec_daily_SWA <- read_rds(path = paste0("data/data_review/",file_list[[7]]))


# Plot --------------------------------------------------------------------

# now make an interactive plot of first 1000 values
ggplotly(
  ggplot() + geom_point(data=cdec_daily_SWA[1:1000,], aes(x=datetime, y=value)))

# Review, QA, and Repeat --------------------------------------------------

cdec_daily_SWA_1_1000 <- cdec_daily_SWA[1:1000,] %>% 
  filter(value > 33.0)

ggplotly(
  ggplot() + geom_point(data=cdec_daily_SWA_1_1000, aes(x=datetime, y=value)))

cdec_daily_SWA_QA <- cdec_daily_SWA_1_1000

ggplotly(
  ggplot() + geom_point(data=cdec_daily_SWA[1001:2000,], aes(x=datetime, y=value)))

cdec_daily_SWA_1001_2000 <- cdec_daily_SWA[1001:2000,] %>% 
  filter(value > 38.2)

ggplotly(
  ggplot() + geom_point(data=cdec_daily_SWA_1001_2000, aes(x=datetime, y=value)))

cdec_daily_SWA_QA <- rbind(cdec_daily_SWA_QA,cdec_daily_SWA_1001_2000)

ggplotly(
  ggplot() + geom_point(data=cdec_daily_SWA[2001:3000,], aes(x=datetime, y=value)))

cdec_daily_SWA_2001_3000 <- cdec_daily_SWA[2001:3000,] %>% 
  filter(value > 29.5, value < 100, datetime != "2016-09-08")

ggplotly(
  ggplot() + geom_point(data=cdec_daily_SWA_2001_3000, aes(x=datetime, y=value)))

cdec_daily_SWA_QA <- rbind(cdec_daily_SWA_QA, cdec_daily_SWA_2001_3000)

ggplotly(
  ggplot() + geom_point(data=cdec_daily_SWA[3001:3168,], aes(x=datetime, y=value)))

cdec_daily_SWA_3001_3168 <- cdec_daily_SWA[3001:3168,] %>% 
  filter(value > 29.3)

ggplotly(
  ggplot() + geom_point(data=cdec_daily_SWA_3001_3168, aes(x=datetime, y=value)))

cdec_daily_SWA_QA <- rbind(cdec_daily_SWA_QA, cdec_daily_SWA_3001_3168)

# Final review ------------------------------------------------------------

#plot QA'd dataset to confirm all points look good
ggplotly(
  ggplot() +geom_point(data = cdec_daily_SWA_QA, aes(x=datetime, y=value)))

#save QA'd dataset as a .rds file
write_rds(cdec_daily_SWA_QA, path = "data/QA_data/cdec_daily_SWA_QA.rds")

#update the gage_QA_progress
gage_QA_progress <- read_csv("data/data_review/gage_QA_progress.csv")

#note reviewer initials, whether review is complete, and any final notes
gage_QA_progress[gage_QA_progress$site_id=="SWA",4:6] <- c("ADW", "Y", "QA complete")

#save updated dataframe to the .csv
write_csv(gage_QA_progress, path = "data/data_review/gage_QA_progress.csv")
