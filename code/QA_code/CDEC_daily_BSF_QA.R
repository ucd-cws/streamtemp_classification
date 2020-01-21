
# Code description --------------------------------------------------------

# Code to review temperature data from gage CDEC_BSF.

library(tidyverse)
library(lubridate)
library(weathermetrics)
library(plotly)

# Get Data ----------------------------------------------------------------

file_list <- list.files("data/data_review/")

# read in next file:
file_list[[9]]

cdec_daily_BSF <- read_rds(path = paste0("data/data_review/",file_list[[9]]))

# Plot --------------------------------------------------------------------

#plot all data to look for obvious bad data points and gaps
ggplotly(
  ggplot() + geom_point(data=cdec_daily_BSF[,], aes(x=date, y=value_mean_C)))

# now make an interactive plot of first 1000 values
ggplotly(
  ggplot() + geom_point(data=cdec_daily_BSF[1:1000,], aes(x=date, y=value_mean_C)))

# Review, QA, and Repeat --------------------------------------------------
cdec_daily_BSF_1_1000 <- cdec_daily_BSF[1:1000,] %>% 
  filter(value_mean_C > 5.5)

ggplotly(
  ggplot() + geom_point(data=cdec_daily_BSF_1_1000, aes(x=date, y=value_mean_C)))

cdec_daily_BSF_QA <- cdec_daily_BSF_1_1000

ggplotly(
  ggplot() + geom_point(data=cdec_daily_BSF[1001:2000,], aes(x=date, y=value_mean_C)))

cdec_daily_BSF_1001_2000 <- cdec_daily_BSF[1001:2000,] %>% 
  filter(value_mean_C > 7.09)

ggplotly(
  ggplot() + geom_point(data=cdec_daily_BSF_1001_2000, aes(x=date, y=value_mean_C)))

cdec_daily_BSF_QA <- rbind(cdec_daily_BSF_QA,cdec_daily_BSF_1001_2000)

ggplotly(
  ggplot() + geom_point(data=cdec_daily_BSF[2001:3000,], aes(x=date, y=value_mean_C)))

cdec_daily_BSF_2001_3000 <- cdec_daily_BSF[2001:3000,] %>% 
  filter(value_mean_C > 6.77)

cdec_daily_BSF_2001_3000 <- cdec_daily_BSF_2001_3000[-c(50:68),]

cdec_daily_BSF_2001_3000 <- cdec_daily_BSF_2001_3000[-c(191:204),]

cdec_daily_BSF_2001_3000 <- cdec_daily_BSF_2001_3000 %>% 
  filter(date != "1996-08-11", date != "1998-01-04", date != "1998-01-05", date != "1998-01-06", date != "1998-01-07", date != "1998-01-08")

ggplotly(
  ggplot() + geom_point(data=cdec_daily_BSF_2001_3000, aes(x=date, y=value_mean_C)))

cdec_daily_BSF_QA <- rbind(cdec_daily_BSF_QA, cdec_daily_BSF_2001_3000)

ggplotly(
  ggplot() + geom_point(data=cdec_daily_BSF[3001:4000,], aes(x=date, y=value_mean_C)))

cdec_daily_BSF_QA <- rbind(cdec_daily_BSF_QA, cdec_daily_BSF[3001:4000,])

ggplotly(
  ggplot() + geom_point(data=cdec_daily_BSF[4001:5000,], aes(x=date, y=value_mean_C)))

cdec_daily_BSF_4001_5000 <- cdec_daily_BSF[4001:5000,] %>% 
  filter(value_mean_C > 7.70, date != "2003-05-17", date != "2003-08-05")

ggplotly(
  ggplot() + geom_point(data=cdec_daily_BSF_4001_5000, aes(x=date, y=value_mean_C)))

cdec_daily_BSF_QA <- rbind(cdec_daily_BSF_QA, cdec_daily_BSF_4001_5000)

ggplotly(
  ggplot() + geom_point(data=cdec_daily_BSF[5001:6000,], aes(x=date, y=value_mean_C)))

cdec_daily_BSF_5001_6000 <- cdec_daily_BSF[5001:6000,] %>% 
  filter(value_mean_C > 7.71, date != "2004-07-09", date != "2004-07-21", date != "2004-07-22", date != "2004-07-24", date != "2005-06-11", date != "2005-06-12", date != "2005-07-02", date != "2005-10-02", date != "2006-06-25", date != "2006-09-17", date != "2006-09-21")

cdec_daily_BSF_5001_6000 <- cdec_daily_BSF_5001_6000[-c(146:148),]

cdec_daily_BSF_5001_6000 <- cdec_daily_BSF_5001_6000[-c(373:388),]

cdec_daily_BSF_5001_6000 <- cdec_daily_BSF_5001_6000[-c(386:388),]

ggplotly(
  ggplot() + geom_point(data=cdec_daily_BSF_5001_6000, aes(x=date, y=value_mean_C)))

cdec_daily_BSF_QA <- rbind(cdec_daily_BSF_QA, cdec_daily_BSF_5001_6000)

ggplotly(
  ggplot() + geom_point(data=cdec_daily_BSF[6001:7000,], aes(x=date, y=value_mean_C)))

cdec_daily_BSF_6001_7000 <- cdec_daily_BSF[6001:7000,] %>% 
  filter(value_mean_C > 0, date != "2007-07-10", date !="2007-07-11")

ggplotly(
  ggplot() + geom_point(data=cdec_daily_BSF_6001_7000, aes(x=date, y=value_mean_C)))

cdec_daily_BSF_QA <- rbind(cdec_daily_BSF_QA, cdec_daily_BSF_6001_7000)

ggplotly(
  ggplot() + geom_point(data=cdec_daily_BSF[7001:8000,], aes(x=date, y=value_mean_C)))

cdec_daily_BSF_7001_8000 <- cdec_daily_BSF[7001:8000,] %>% 
  filter(value_mean_C > 3.17)

ggplotly(
  ggplot() + geom_point(data=cdec_daily_BSF_7001_8000, aes(x=date, y=value_mean_C)))

cdec_daily_BSF_QA <- rbind(cdec_daily_BSF_QA, cdec_daily_BSF_7001_8000)

ggplotly(
  ggplot() + geom_point(data=cdec_daily_BSF[8001:9000,], aes(x=date, y=value_mean_C)))

cdec_daily_BSF_8001_9000 <- cdec_daily_BSF[8001:9000,] %>% 
  filter(value_mean_C > 6.88, date != "2012-09-12", date != "2013-09-24")

ggplotly(
  ggplot() + geom_point(data=cdec_daily_BSF_8001_9000, aes(x=date, y=value_mean_C)))

cdec_daily_BSF_QA <- rbind(cdec_daily_BSF_QA, cdec_daily_BSF_8001_9000)

ggplotly(
  ggplot() + geom_point(data=cdec_daily_BSF[9001:10000,], aes(x=date, y=value_mean_C)))

cdec_daily_BSF_9001_10000 <- cdec_daily_BSF[9001:10000,] %>% 
  filter(value_mean_C > 7.52)
  
cdec_daily_BSF_9001_10000 <- cdec_daily_BSF_9001_10000[-c(609:619),]

ggplotly(
  ggplot() + geom_point(data=cdec_daily_BSF_9001_10000, aes(x=date, y=value_mean_C)))

cdec_daily_BSF_QA <- rbind(cdec_daily_BSF_QA, cdec_daily_BSF_9001_10000)

ggplotly(
  ggplot() + geom_point(data=cdec_daily_BSF[10001:10500,], aes(x=date, y=value_mean_C)))

cdec_daily_BSF_10001_10500 <- cdec_daily_BSF[10001:10500,] %>% 
  filter(value_mean_C > 4.85)

ggplotly(
  ggplot() + geom_point(data=cdec_daily_BSF_10001_10500, aes(x=date, y=value_mean_C)))

cdec_daily_BSF_QA <- rbind(cdec_daily_BSF_QA, cdec_daily_BSF_9001_10000)
# Final review ------------------------------------------------------------

#plot QA'd dataset to confirm all points look good
ggplotly(
  ggplot() +geom_point(data = cdec_daily_BSF_QA, aes(x=date, y=value_mean_C)))

#save QA'd dataset as a .rds file
write_rds(cdec_daily_BSF_QA, path = "data/QA_data/cdec_daily_BSF_QA.rds")

#update the gage_QA_progress
gage_QA_progress <- read_csv("data/data_review/gage_QA_progress.csv")

#note reviewer initials, whether review is complete, and any final notes
gage_QA_progress[gage_QA_progress$site_id=="BSF",4:6] <- c("ADW", "Y", "QA complete")

#save updated dataframe to the .csv
write_csv(gage_QA_progress, path = "data/data_review/gage_QA_progress.csv")
