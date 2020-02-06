
# Code description --------------------------------------------------------

# Code to review temperature data from gage CDEC_MRB.

library(tidyverse)
library(lubridate)
library(weathermetrics)
library(plotly)

# Get Data ----------------------------------------------------------------

file_list <- list.files("data/data_review/")

# read in next file:
file_list[[38]]

cdec_daily_MRB <- read_rds(path = paste0("data/data_review/",file_list[[38]]))

# Plot --------------------------------------------------------------------

# now make an interactive plot of first 1000 values
ggplotly(
  ggplot() + geom_point(data=cdec_daily_MRB[1:1000,], aes(x=date, y=value_mean_C)))

# Review, QA, and Repeat --------------------------------------------------

cdec_daily_MRB_QA <- cdec_daily_MRB[1:1000,]

ggplotly(
  ggplot() + geom_point(data=cdec_daily_MRB[1001:2000,], aes(x=date, y=value_mean_C)))

cdec_daily_MRB_QA <- rbind(cdec_daily_MRB_QA, cdec_daily_MRB[1001:2000,])

ggplotly(
  ggplot() + geom_point(data=cdec_daily_MRB[2001:2500,], aes(x=date, y=value_mean_C)))

cdec_daily_MRB_2001_2500 <- cdec_daily_MRB[2001:2500,] %>% 
  filter(date != "2015-11-14", date != "2015-11-15", date != "2015-11-16", date != "2015-11-17")

ggplotly(
  ggplot() + geom_point(data=cdec_daily_MRB_2001_2500, aes(x=date, y=value_mean_C)))

cdec_daily_MRB_QA <- rbind(cdec_daily_MRB_QA, cdec_daily_MRB_2001_2500)

ggplotly(
  ggplot() + geom_point(data=cdec_daily_MRB[2501:3520,], aes(x=date, y=value_mean_C)))

cdec_daily_MRB_2501_3520 <- cdec_daily_MRB[2501:3520,] %>% 
  filter(value_mean_C < 30) 

cdec_daily_MRB_2501_3520 <- cdec_daily_MRB_2501_3520[-c(779:786),]
 
ggplotly(
  ggplot() + geom_point(data=cdec_daily_MRB_2501_3520, aes(x=date, y=value_mean_C)))

cdec_daily_MRB_QA <- rbind(cdec_daily_MRB_QA, cdec_daily_MRB_2501_3520)

# Final review ------------------------------------------------------------

#plot QA'd dataset to confirm all points look good
ggplotly(
  ggplot() +geom_point(data = cdec_daily_MRB_QA, aes(x=date, y=value_mean_C)))

#save QA'd dataset as a .rds file
write_rds(cdec_daily_MRB_QA, path = "data/QA_data/cdec_daily_MRB_QA.rds")

#update the gage_QA_progress
gage_QA_progress <- read_csv("data/data_review/gage_QA_progress.csv")

#note reviewer initials, whether review is complete, and any final notes
gage_QA_progress[gage_QA_progress$site_id=="MRB",4:6] <- c("ADW", "Y", "QA complete")

#save updated dataframe to the .csv
write_csv(gage_QA_progress, path = "data/data_review/gage_QA_progress.csv")
