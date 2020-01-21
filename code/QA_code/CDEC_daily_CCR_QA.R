
# Code description --------------------------------------------------------

# Code to review temperature data from gage CDEC_CCR.

library(tidyverse)
library(lubridate)
library(weathermetrics)
library(plotly)

# Get Data ----------------------------------------------------------------

file_list <- list.files("data/data_review/")

# read in next file:
file_list[[11]]

cdec_daily_CCR <- read_rds(path = paste0("data/data_review/",file_list[[11]]))

# Plot --------------------------------------------------------------------

#plot all data to look for obvious bad data points and gaps
ggplotly(
  ggplot() + geom_point(data=cdec_daily_CCR[,], aes(x=date, y=value_mean_C)))

# now make an interactive plot of first 1000 values
ggplotly(
  ggplot() + geom_point(data=cdec_daily_CCR[1:1000,], aes(x=date, y=value_mean_C)))

# Review, QA, and Repeat --------------------------------------------------

cdec_daily_CCR_1_1000 <- cdec_daily_CCR[1:1000,] %>% 
  filter(value_mean_C > 8.06, date != "1998-07-21", date != "1997-08-23", date != "1997-09-15", date != "1997-09-18", date != "1997-09-20", date != "1997-09-23", date != "1997-09-27", date != "1997-10-02", date != "1997-10-07", date != "1997-10-23", date != "1997-10-25", date != "1997-10-24", date != "1997-11-05", date != "1997-11-09", date != "1997-11-08", date != "1997-11-10", date != "1997-11-14" , date != "1997-11-26", date != "1997-12-10", date != "1997-12-29", date != "1998-01-10", date != "1998-01-23", date != "1998-01-24", date != "1999-01-09", date != "1998-11-17")

cdec_daily_CCR_1_1000 <- cdec_daily_CCR_1_1000[-c(1:6),]

ggplotly(
  ggplot() + geom_point(data=cdec_daily_CCR_1_1000, aes(x=date, y=value_mean_C)))

cdec_daily_CCR_QA <- cdec_daily_CCR_1_1000

ggplotly(
  ggplot() + geom_point(data=cdec_daily_CCR[1001:2000,], aes(x=date, y=value_mean_C)))

cdec_daily_CCR_1001_2000 <- cdec_daily_CCR[1001:2000,] %>% 
  filter(date != "2000-10-13", date != "2000-10-20", date != "2001-05-09", date != "2001-05-25", date != "2001-06-14", date != "2002-03-04", date != "2002-06-05", date != "2000-07-27", date != "2000-05-17", date != "2000-05-18", date != "2000-05-19")

ggplotly(
  ggplot() + geom_point(data=cdec_daily_CCR_1001_2000, aes(x=date, y=value_mean_C)))

cdec_daily_CCR_QA <- rbind(cdec_daily_CCR_QA, cdec_daily_CCR_1001_2000)

ggplotly(
  ggplot() + geom_point(data=cdec_daily_CCR[2001:3000,], aes(x=date, y=value_mean_C)))

cdec_daily_CCR_2001_3000 <- cdec_daily_CCR[2001:3000,] %>% 
  filter(date != "2003-10-30", date != "2004-11-30", date != "2005-01-30")

ggplotly(
  ggplot() + geom_point(data=cdec_daily_CCR_2001_3000, aes(x=date, y=value_mean_C)))

cdec_daily_CCR_QA <- rbind(cdec_daily_CCR_QA, cdec_daily_CCR_2001_3000)

ggplotly(
  ggplot() + geom_point(data=cdec_daily_CCR[3001:4000,], aes(x=date, y=value_mean_C)))

cdec_daily_CCR_3001_4000 <- cdec_daily_CCR[3001:4000,] %>% 
  filter(date != "2007-05-27", date != "2007-06-04", date != "2007-06-05", date != "2007-06-13", date != "2006-05-23", date != "2006-05-27")

ggplotly(
  ggplot() + geom_point(data=cdec_daily_CCR_3001_4000, aes(x=date, y=value_mean_C)))

cdec_daily_CCR_QA <- rbind(cdec_daily_CCR_QA, cdec_daily_CCR_3001_4000)

ggplotly(
  ggplot() + geom_point(data=cdec_daily_CCR[4001:5000,], aes(x=date, y=value_mean_C)))

cdec_daily_CCR_4001_5000 <- cdec_daily_CCR[4001:5000,]

cdec_daily_CCR_4001_5000 <- cdec_daily_CCR_4001_5000[-c(673:677),]

ggplotly(
  ggplot() + geom_point(data=cdec_daily_CCR_4001_5000, aes(x=date, y=value_mean_C)))

cdec_daily_CCR_QA <- rbind(cdec_daily_CCR_QA, cdec_daily_CCR_4001_5000)

ggplotly(
  ggplot() + geom_point(data=cdec_daily_CCR[5001:6000,], aes(x=date, y=value_mean_C)))

cdec_daily_CCR_QA <- rbind(cdec_daily_CCR_QA, cdec_daily_CCR[5001:6000,])

ggplotly(
  ggplot() + geom_point(data=cdec_daily_CCR[6001:7000,], aes(x=date, y=value_mean_C)))

cdec_daily_CCR_6001_7000 <- cdec_daily_CCR[6001:7000,] %>% 
  filter(value_mean_C > 8.51, date != "2014-12-03", date != "2014-12-04", date != "2015-06-04", date != "2015-09-16", date != "2016-10-03", date != "2015-04-07")

ggplotly(
  ggplot() + geom_point(data=cdec_daily_CCR_6001_7000, aes(x=date, y=value_mean_C)))

cdec_daily_CCR_QA <- rbind(cdec_daily_CCR_QA, cdec_daily_CCR_6001_7000)

ggplotly(
  ggplot() + geom_point(data=cdec_daily_CCR[7001:8086,], aes(x=date, y=value_mean_C)))

cdec_daily_CCR_7001_8086 <- cdec_daily_CCR[7001:8086,] %>% 
  filter(value_mean_C > 8.0, date != "2018-06-12", date != "2018-07-30", date != "2018-07-31", date != "2019-02-13", date != "2019-02-14", date != "2019-09-25", date != "2019-09-30", date != "2019-09-29", date != "2019-10-01")

ggplotly(
  ggplot() + geom_point(data=cdec_daily_CCR_7001_8086, aes(x=date, y=value_mean_C)))

cdec_daily_CCR_QA <- rbind(cdec_daily_CCR_QA, cdec_daily_CCR_7001_8086)


# Final review ------------------------------------------------------------

#plot QA'd dataset to confirm all points look good
ggplotly(
  ggplot() +geom_point(data = cdec_daily_CCR_QA, aes(x=date, y=value_mean_C)))

#save QA'd dataset as a .rds file
write_rds(cdec_daily_CCR_QA, path = "data/QA_data/cdec_daily_CCR_QA.rds")

#update the gage_QA_progress
gage_QA_progress <- read_csv("data/data_review/gage_QA_progress.csv")

#note reviewer initials, whether review is complete, and any final notes
gage_QA_progress[gage_QA_progress$site_id=="CCR",4:6] <- c("ADW", "Y", "QA complete")

#save updated dataframe to the .csv
write_csv(gage_QA_progress, path = "data/data_review/gage_QA_progress.csv")
