
# Code description --------------------------------------------------------

# Code to review temperature data from gage CDEC_BWC.

library(tidyverse)
library(lubridate)
library(weathermetrics)
library(plotly)

# Get Data ----------------------------------------------------------------

file_list <- list.files("data/data_review/")

# read in next file:
file_list[[10]]

cdec_daily_BWC <- read_rds(path = paste0("data/data_review/",file_list[[10]]))

# Plot --------------------------------------------------------------------

#plot all data to look for obvious bad data points and gaps
ggplotly(
  ggplot() + geom_point(data=cdec_daily_BWC[,], aes(x=date, y=value_mean_C)))

# now make an interactive plot of first 1000 values
ggplotly(
  ggplot() + geom_point(data=cdec_daily_BWC[1:1000,], aes(x=date, y=value_mean_C)))

# Review, QA, and Repeat --------------------------------------------------

cdec_daily_BWC_1_1000 <- cdec_daily_BWC[1:1000,] %>% 
  filter(value_mean_C > 3.94, date != "2000-07-27")

ggplotly(
  ggplot() + geom_point(data=cdec_daily_BWC_1_1000, aes(x=date, y=value_mean_C)))

cdec_daily_BWC_QA <- cdec_daily_BWC_1_1000

ggplotly(
  ggplot() + geom_point(data=cdec_daily_BWC[1001:2000,], aes(x=date, y=value_mean_C)))

cdec_daily_BWC_1001_2000 <- cdec_daily_BWC[1001:2000,] %>% 
  filter(value_mean_C < 26.15, date != "2003-10-22", date != "2003-10-31", date != "2003-11-01", date != "2003-11-03", date != "2003-11-07")

ggplotly(
  ggplot() + geom_point(data=cdec_daily_BWC_1001_2000, aes(x=date, y=value_mean_C)))

cdec_daily_BWC_QA <- rbind(cdec_daily_BWC_QA, cdec_daily_BWC_1001_2000)

ggplotly(
  ggplot() + geom_point(data=cdec_daily_BWC[2001:3000,], aes(x=date, y=value_mean_C)))

cdec_daily_BWC_2001_3000 <- cdec_daily_BWC[2001:3000,]

cdec_daily_BWC_2001_3000 <- cdec_daily_BWC_2001_3000[-c(988:1000),]

ggplotly(
  ggplot() + geom_point(data=cdec_daily_BWC_2001_3000, aes(x=date, y=value_mean_C)))

cdec_daily_BWC_QA <- rbind(cdec_daily_BWC_QA, cdec_daily_BWC_2001_3000)

ggplotly(
  ggplot() + geom_point(data=cdec_daily_BWC[3001:4000,], aes(x=date, y=value_mean_C)))

cdec_daily_BWC_QA <- rbind(cdec_daily_BWC_QA, cdec_daily_BWC[3001:4000,])

ggplotly(
  ggplot() + geom_point(data=cdec_daily_BWC[4001:4934,], aes(x=date, y=value_mean_C)))

cdec_daily_BWC_4001_4934 <- cdec_daily_BWC[4001:4934,] %>% 
  filter(date != "2012-02-22", date != "2012-02-23")

cdec_daily_BWC_4001_4934 <- cdec_daily_BWC_4001_4934[-c(631:637),]

cdec_daily_BWC_4001_4934 <- cdec_daily_BWC_4001_4934[-c(917:925),]

ggplotly(
  ggplot() + geom_point(data=cdec_daily_BWC_4001_4934, aes(x=date, y=value_mean_C)))

cdec_daily_BWC_QA <- rbind(cdec_daily_BWC_QA, cdec_daily_BWC_4001_4934)
# Final review ------------------------------------------------------------

#plot QA'd dataset to confirm all points look good
ggplotly(
  ggplot() +geom_point(data = cdec_daily_BWC_QA, aes(x=date, y=value_mean_C)))

#save QA'd dataset as a .rds file
write_rds(cdec_daily_BWC_QA, path = "data/QA_data/cdec_daily_BWC_QA.rds")

#update the gage_QA_progress
gage_QA_progress <- read_csv("data/data_review/gage_QA_progress.csv")

#note reviewer initials, whether review is complete, and any final notes
gage_QA_progress[gage_QA_progress$site_id=="BWC",4:6] <- c("ADW", "Y", "QA complete")

#save updated dataframe to the .csv
write_csv(gage_QA_progress, path = "data/data_review/gage_QA_progress.csv")
