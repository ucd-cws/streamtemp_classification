
# Code description --------------------------------------------------------

# Code to review temperature data from gage CDEC_SDP.

library(tidyverse)
library(lubridate)
library(weathermetrics)
library(plotly)

# Get Data ----------------------------------------------------------------

file_list <- list.files("data/data_review/")

# read in next file:
file_list[[50]]

cdec_daily_SDP <- read_rds(path = paste0("data/data_review/",file_list[[50]]))

# Plot --------------------------------------------------------------------

ggplotly(
  ggplot() + geom_point(data=cdec_daily_SDP[,], aes(x=date, y=value_mean_C)))

# now make an interactive plot of first 1000 values
ggplotly(
  ggplot() + geom_point(data=cdec_daily_SDP[1:1000,], aes(x=date, y=value_mean_C)))

# Review, QA, and Repeat --------------------------------------------------

cdec_daily_SDP_1_1000 <- cdec_daily_SDP[1:1000,] %>% 
  filter( value_mean_C > fahrenheit.to.celsius(32.3))

ggplotly(
  ggplot() + geom_point(data=cdec_daily_SDP_1_1000, aes(x=date, y=value_mean_C)))

cdec_daily_SDP_QA <- cdec_daily_SDP_1_1000

ggplotly(
  ggplot() + geom_point(data=cdec_daily_SDP[1001:2000,], aes(x=date, y=value_mean_C)))

cdec_daily_SDP_1001_2000 <- cdec_daily_SDP[1001:2000,] %>% 
  filter(value_mean_C > fahrenheit.to.celsius(25.5), date != "2013-09-17", date != "2014-03-26")

ggplotly(
  ggplot() + geom_point(data=cdec_daily_SDP_1001_2000, aes(x=date, y=value_mean_C)))

cdec_daily_SDP_QA <- rbind(cdec_daily_SDP_QA, cdec_daily_SDP_1001_2000)

ggplotly(
  ggplot() + geom_point(data=cdec_daily_SDP[2001:3537,], aes(x=date, y=value_mean_C)))

cdec_daily_SDP_2001_3537 <- cdec_daily_SDP[2001:3537,] %>% 
  filter(value_mean_C > fahrenheit.to.celsius(42.2))

ggplotly(
  ggplot() + geom_point(data=cdec_daily_SDP_2001_3537, aes(x=date, y=value_mean_C)))

cdec_daily_SDP_QA <- rbind(cdec_daily_SDP_QA, cdec_daily_SDP_2001_3537)


# Final review ------------------------------------------------------------

#plot QA'd dataset to confirm all points look good
ggplotly(
  ggplot() +geom_point(data = cdec_daily_SDP_QA, aes(x=date, y=value_mean_C)))

#save QA'd dataset as a .rds file
write_rds(cdec_daily_SDP_QA, path = "data/QA_data/cdec_daily_SDP_QA.rds")

#update the gage_QA_progress
gage_QA_progress <- read_csv("data/data_review/gage_QA_progress.csv")

#note reviewer initials, whether review is complete, and any final notes
gage_QA_progress[gage_QA_progress$site_id=="SDP",4:6] <- c("ADW", "Y", "QA complete")

#save updated dataframe to the .csv
write_csv(gage_QA_progress, path = "data/data_review/gage_QA_progress.csv")
