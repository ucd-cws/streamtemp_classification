
# Code description --------------------------------------------------------

# Code to review temperature data from gage CDEC_LWS.

library(tidyverse)
library(lubridate)
library(weathermetrics)


# Get Data ----------------------------------------------------------------

file_list <- list.files("data/data_review/")

# read in next file:
file_list[[36]]

cdec_daily_LWS <- read_rds(path = paste0("data/data_review/",file_list[[36]]))

# Plot --------------------------------------------------------------------

library(plotly)

ggplotly(
  ggplot() + geom_point(data=cdec_daily_LWS[,], aes(x=date, y=value_mean_C)))

# now make an interactive plot of first 1000 values
ggplotly(
  ggplot() + geom_point(data=cdec_daily_LWS[1:1000,], aes(x=date, y=value_mean_C)))

# Review, QA, and Repeat --------------------------------------------------

cdec_daily_LWS_1_1000 <- cdec_daily_LWS[1:1000,] %>% 
  filter(value_mean_C < 16.44, value_mean_C > 0.94, date != "1990-07-27", date != "1991-05-02", date != "1991-05-03", date != "1993-01-14", date != "1991-11-02", date!= "1992-08-25")

ggplotly(
  ggplot() + geom_point(data=cdec_daily_LWS_1_1000, aes(x=date, y=value_mean_C)))

cdec_daily_LWS_QA <- cdec_daily_LWS_1_1000

ggplotly(
  ggplot() + geom_point(data=cdec_daily_LWS[1001:2000,], aes(x=date, y=value_mean_C)))

cdec_daily_LWS_1001_2000 <- cdec_daily_LWS[1001:2000,] %>% 
  filter(value_mean_C > 5.39, date != "1996-01-12", date != "1996-01-13", date != "1996-02-19", date != "1996-03-01")
  
ggplotly(
  ggplot() + geom_point(data=cdec_daily_LWS_1001_2000, aes(x=date, y=value_mean_C)))

cdec_daily_LWS_QA <- rbind(cdec_daily_LWS_QA, cdec_daily_LWS_1001_2000)

ggplotly(
  ggplot() + geom_point(data=cdec_daily_LWS[2001:3000,], aes(x=date, y=value_mean_C)))

cdec_daily_LWS_2001_3000 <- cdec_daily_LWS[2001:3000,] %>% 
  filter(value_mean_C > 0.78, date != "1996-07-25", date != "1997-05-23", date != "1997-07-01", date != "1997-12-03", date != "1998-08-11")

ggplotly(
  ggplot() + geom_point(data=cdec_daily_LWS_2001_3000, aes(x=date, y=value_mean_C)))

ggplotly(
  ggplot() + geom_point(data=cdec_daily_LWS[3001:4000,], aes(x=date, y=value_mean_C)))

cdec_daily_LWS_QA <- rbind(cdec_daily_LWS_QA, cdec_daily_LWS[3001:4000,])

ggplotly(
  ggplot() + geom_point(data=cdec_daily_LWS[4001:5000,], aes(x=date, y=value_mean_C)))

cdec_daily_LWS_QA <- rbind(cdec_daily_LWS_QA, cdec_daily_LWS[4001:5000,])

ggplotly(
  ggplot() + geom_point(data=cdec_daily_LWS[5001:6000,], aes(x=date, y=value_mean_C)))

cdec_daily_LWS_QA <- rbind(cdec_daily_LWS_QA, cdec_daily_LWS[5001:6000,])

ggplotly(
  ggplot() + geom_point(data=cdec_daily_LWS[6001:7000,], aes(x=date, y=value_mean_C)))

cdec_daily_LWS_QA <- rbind(cdec_daily_LWS_QA, cdec_daily_LWS[6001:7000,])

ggplotly(
  ggplot() + geom_point(data=cdec_daily_LWS[7001:8000,], aes(x=date, y=value_mean_C)))

cdec_daily_LWS_QA <- rbind(cdec_daily_LWS_QA, cdec_daily_LWS[7001:8000,])

ggplotly(
  ggplot() + geom_point(data=cdec_daily_LWS[8001:9000,], aes(x=date, y=value_mean_C)))

cdec_daily_LWS_QA <- rbind(cdec_daily_LWS_QA, cdec_daily_LWS[8001:9000,])

ggplotly(
  ggplot() + geom_point(data=cdec_daily_LWS[9001:9911,], aes(x=date, y=value_mean_C)))

cdec_daily_LWS_QA <- rbind(cdec_daily_LWS_QA, cdec_daily_LWS[9001:9911,])

# Final review ------------------------------------------------------------

#plot QA'd dataset to confirm all points look good
ggplotly(
  ggplot() +geom_point(data = cdec_daily_LWS_QA, aes(x=date, y=value_mean_C)))

#save QA'd dataset as a .rds file
write_rds(cdec_daily_LWS_QA, path = "data/QA_data/cdec_daily_LWS_QA.rds")

#update the gage_QA_progress
gage_QA_progress <- read_csv("data/data_review/gage_QA_progress.csv")

#note reviewer initials, whether review is complete, and any final notes
gage_QA_progress[gage_QA_progress$site_id=="LWS",6:8] <- c("ADW", "Y", "QA complete")

#save updated dataframe to the .csv
write_csv(gage_QA_progress, path = "data/data_review/gage_QA_progress.csv")
