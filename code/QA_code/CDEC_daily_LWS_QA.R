
# Code description --------------------------------------------------------

# Code to review temperature data from gage CDEC_LWS.

library(tidyverse)
library(lubridate)
library(weathermetrics)


# Get Data ----------------------------------------------------------------

file_list <- list.files("data/data_review/")

# read in next file:
file_list[[3]]

cdec_daily_LWS <- read_rds(path = paste0("data/data_review/",file_list[[3]]))

# Plot --------------------------------------------------------------------

library(plotly)

# now make an interactive plot of first 1000 values
ggplotly(
  ggplot() + geom_point(data=cdec_daily_LWS[1:1000,], aes(x=datetime, y=value)))

# Review, QA, and Repeat --------------------------------------------------

cdec_daily_LWS_1_1000 <- cdec_daily_LWS[1:1000,] %>% 
  filter(value < 78.7, value > 33.7)

ggplotly(
  ggplot() + geom_point(data=cdec_daily_LWS_1_1000, aes(x=datetime, y=value)))

check_temps <- c(44.3, 64.0, 61.6, 43.7, 45.5)

fahrenheit.to.celsius(check_temps)

cdec_daily_LWS_1_1000 <- cdec_daily_LWS_1_1000 %>% 
  filter(datetime != "1990-07-27")

ggplotly(
  ggplot() + geom_point(data=cdec_daily_LWS_1_1000, aes(x=datetime, y=value)))

cdec_daily_LWS_QA <- cdec_daily_LWS_1_1000

ggplotly(
  ggplot() + geom_point(data=cdec_daily_LWS[1001:2000,], aes(x=datetime, y=value)))

cdec_daily_LWS_1001_2000 <- cdec_daily_LWS[1001:2000,] %>% 
  filter(value > 32.0)
  
ggplotly(
  ggplot() + geom_point(data=cdec_daily_LWS_1001_2000, aes(x=datetime, y=value)))

check_temps <- c(35.9) 

fahrenheit.to.celsius(check_temps)

cdec_daily_LWS_QA <- rbind(cdec_daily_LWS_QA, cdec_daily_LWS_1001_2000)
 
ggplotly(
  ggplot() + geom_point(data=cdec_daily_LWS[2001:3000,], aes(x=datetime, y=value)))

cdec_daily_LWS_2001_3000 <- cdec_daily_LWS[2001:3000,] %>% 
  filter(value<300, datetime != "1997-05-22", datetime != "1997-05-23", datetime != "1997-07-01", datetime != "1997-07-02", datetime != "1998-08-11")

ggplotly(
  ggplot() + geom_point(data=cdec_daily_LWS_2001_3000, aes(x=datetime, y=value)))

cdec_daily_LWS_QA <- rbind(cdec_daily_LWS_QA, cdec_daily_LWS_2001_3000)

ggplotly(
  ggplot() + geom_point(data=cdec_daily_LWS[3001:4000,], aes(x=datetime, y=value)))

fahrenheit.to.celsius(54.2)

cdec_daily_LWS_QA <- rbind(cdec_daily_LWS_QA, cdec_daily_LWS[3001:4000,])

ggplotly(
  ggplot() + geom_point(data=cdec_daily_LWS[4001:5000,], aes(x=datetime, y=value)))

cdec_daily_LWS_QA <- rbind(cdec_daily_LWS_QA, cdec_daily_LWS[4001:5000,])

ggplotly(
  ggplot() + geom_point(data=cdec_daily_LWS[5001:6000,], aes(x=datetime, y=value)))

cdec_daily_LWS_QA <- rbind(cdec_daily_LWS_QA, cdec_daily_LWS[5001:6000,])

ggplotly(
  ggplot() + geom_point(data=cdec_daily_LWS[6001:7000,], aes(x=datetime, y=value)))

cdec_daily_LWS_6001_7000 <- cdec_daily_LWS[6001:7000,] %>% 
  filter(value < 4000)

ggplotly(
  ggplot() + geom_point(data=cdec_daily_LWS_6001_7000, aes(x=datetime, y=value)))

cdec_daily_LWS_QA <- rbind(cdec_daily_LWS_QA, cdec_daily_LWS_6001_7000)

ggplotly(
  ggplot() + geom_point(data=cdec_daily_LWS[7001:8000,], aes(x=datetime, y=value)))

cdec_daily_LWS_QA <- rbind(cdec_daily_LWS_QA, cdec_daily_LWS[7001:8000,])

ggplotly(
  ggplot() + geom_point(data=cdec_daily_LWS[8001:9000,], aes(x=datetime, y=value)))

cdec_daily_LWS_QA <- rbind(cdec_daily_LWS_QA, cdec_daily_LWS[8001:9000,])

ggplotly(
  ggplot() + geom_point(data=cdec_daily_LWS[9001:9928,], aes(x=datetime, y=value)))

check_temps <- c(58.7, 56.9)

fahrenheit.to.celsius(check_temps)

cdec_daily_LWS_QA <- rbind(cdec_daily_LWS_QA, cdec_daily_LWS[9001:9928,])

# Final review ------------------------------------------------------------

#plot QA'd dataset to confirm all points look good
ggplotly(
  ggplot() +geom_point(data = cdec_daily_LWS_QA, aes(x=datetime, y=value)))

check_temps <- c(64.0, 61.6)

fahrenheit.to.celsius(check_temps)

#save QA'd dataset as a .rds file
write_rds(cdec_daily_LWS_QA, path = "data/QA_data/cdec_daily_LWS_QA.rds")

#update the gage_QA_progress
gage_QA_progress <- read_csv("data/data_review/gage_QA_progress.csv")

#confirm correct row to update by the site_id
gage_QA_progress[56,1]

#note reviewer initials, whether review is complete, and any final notes
gage_QA_progress[56,4:6] <- c("ADW", "Y", "QA complete")

#save updated dataframe to the .csv
write_csv(gage_QA_progress, path = "data/data_review/gage_QA_progress.csv")
