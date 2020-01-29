
# Code description --------------------------------------------------------

# Code to review temperature data from gage CDEC_NTR.

library(tidyverse)
library(lubridate)
library(weathermetrics)
library(plotly)

# Get Data ----------------------------------------------------------------

file_list <- list.files("data/data_review/", pattern = ".rds")

# read in next file:
file_list[[43]]

cdec_daily_NTR <- read_rds(path = paste0("data/data_review/",file_list[[43]]))

# Plot --------------------------------------------------------------------

#plot all data to look for obvious bad data points and gaps
ggplotly(
  ggplot() + geom_point(data=cdec_daily_NTR[,], aes(x=date, y=value_mean_C)))

# now make an interactive plot of first 1000 values
ggplotly(
  ggplot() + geom_point(data=cdec_daily_NTR[1:1000,], aes(x=date, y=value_mean_C)))

# Review, QA, and Repeat --------------------------------------------------
cdec_daily_NTR_1_1000 <- cdec_daily_NTR[1:1000,] %>% 
  filter(value_mean_C > 0.25, date != "2003-06-03", date != "2003-06-07", date != "2003-06-26", date != "2003-07-02", date != "2003-07-04", date != "2003-07-05", date != "2003-07-16", date != "2003-07-24")

ggplotly(
  ggplot() + geom_point(data=cdec_daily_NTR_1_1000, aes(x=date, y=value_mean_C)))

cdec_daily_NTR_QA <- cdec_daily_NTR_1_1000

ggplotly(
  ggplot() + geom_point(data=cdec_daily_NTR[1001:2000,], aes(x=date, y=value_mean_C)))

cdec_daily_NTR_1001_2000 <- cdec_daily_NTR[1001:2000,] %>% 
  filter(value_mean_C > 1.14)

cdec_daily_NTR_1001_2000 <- cdec_daily_NTR_1001_2000[-c(209:226),]

cdec_daily_NTR_1001_2000 <- cdec_daily_NTR_1001_2000 %>% 
  filter(date != "2005-06-08", date != "2005-05-08", date != "2005-03-29", date != "2004-04-10", date != "2003-09-12", date != "2003-09-13", date != "2003-10-08", date != "2003-09-15")

ggplotly(
  ggplot() + geom_point(data=cdec_daily_NTR_1001_2000, aes(x=date, y=value_mean_C)))

cdec_daily_NTR_QA <- rbind(cdec_daily_NTR_QA, cdec_daily_NTR_1001_2000)

ggplotly(
  ggplot() + geom_point(data=cdec_daily_NTR[2001:3000,], aes(x=date, y=value_mean_C)))

cdec_daily_NTR_QA <- rbind(cdec_daily_NTR_QA, cdec_daily_NTR[2001:3000,])

ggplotly(
  ggplot() + geom_point(data=cdec_daily_NTR[3001:4000,], aes(x=date, y=value_mean_C)))

cdec_daily_NTR_QA <- rbind(cdec_daily_NTR_QA, cdec_daily_NTR[3001:4000,])

ggplotly(
  ggplot() + geom_point(data=cdec_daily_NTR[4001:5022,], aes(x=date, y=value_mean_C)))

cdec_daily_NTR_4001_5022 <- cdec_daily_NTR[4001:5019,]

ggplotly(
  ggplot() + geom_point(data=cdec_daily_NTR_4001_5022, aes(x=date, y=value_mean_C)))

cdec_daily_NTR_QA <- rbind(cdec_daily_NTR_QA,cdec_daily_NTR_4001_5022)
# Final review ------------------------------------------------------------

#plot QA'd dataset to confirm all points look good
ggplotly(
  ggplot() +geom_point(data = cdec_daily_NTR_QA, aes(x=date, y=value_mean_C)))

#save QA'd dataset as a .rds file
write_rds(cdec_daily_NTR_QA, path = "data/QA_data/cdec_daily_NTR_QA.rds")

#update the gage_QA_progress
gage_QA_progress <- read_csv("data/data_review/gage_QA_progress.csv")

#note reviewer initials, whether review is complete, and any final notes
gage_QA_progress[gage_QA_progress$site_id=="NTR",6:8] <- c("ADW", "Y", "QA complete")

#save updated dataframe to the .csv
write_csv(gage_QA_progress, path = "data/data_review/gage_QA_progress.csv")
