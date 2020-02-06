
# Code description --------------------------------------------------------

# Code to review temperature data from gage CDEC_AFD.

library(tidyverse)
library(lubridate)
library(weathermetrics)
library(plotly)

# Get Data ----------------------------------------------------------------

file_list <- list.files("data/data_review/")

# read in next file:
file_list[[1]]

cdec_daily_AFD <- read_rds(path = paste0("data/data_review/",file_list[[1]]))

# Plot --------------------------------------------------------------------

ggplotly(
  ggplot() + geom_point(data=cdec_daily_AFD[,], aes(x=date, y=value_mean_C)))


# now make an interactive plot of first 1000 values
ggplotly(
  ggplot() + geom_point(data=cdec_daily_AFD[1:1000,], aes(x=date, y=value_mean_C)))

# Review, QA, and Repeat --------------------------------------------------

cdec_daily_AFD_1_1001 <- cdec_daily_AFD[1:1000,] %>% 
  filter(date != "1998-11-03", date != "1999-03-15", date != "1999-03-28")

ggplotly(
  ggplot() + geom_point(data=cdec_daily_AFD_1_1001, aes(x=date, y=value_mean_C)))

cdec_daily_AFD_QA <- cdec_daily_AFD_1_1001

ggplotly(
  ggplot() + geom_point(data=cdec_daily_AFD[1001:2000,], aes(x=date, y=value_mean_C)))

cdec_daily_AFD_1001_2000 <- cdec_daily_AFD[1001:2000,]

cdec_daily_AFD_1001_2000 <- cdec_daily_AFD_1001_2000[-c(63:77),]

cdec_daily_AFD_1001_2000 <- cdec_daily_AFD_1001_2000[-c(397:422),]

ggplotly(
  ggplot() + geom_point(data=cdec_daily_AFD_1001_2000, aes(x=date, y=value_mean_C)))

cdec_daily_AFD_QA <- rbind(cdec_daily_AFD_QA, cdec_daily_AFD_1001_2000)

ggplotly(
  ggplot() + geom_point(data=cdec_daily_AFD[2001:3000,], aes(x=date, y=value_mean_C)))

cdec_daily_AFD_QA <- rbind(cdec_daily_AFD_QA, cdec_daily_AFD[2001:3000,])

ggplotly(
  ggplot() + geom_point(data=cdec_daily_AFD[3001:4000,], aes(x=date, y=value_mean_C)))

cdec_daily_AFD_QA <- rbind(cdec_daily_AFD_QA, cdec_daily_AFD[3001:4000,])

ggplotly(
  ggplot() + geom_point(data=cdec_daily_AFD[4001:5000,], aes(x=date, y=value_mean_C)))

cdec_daily_AFD_QA <- rbind(cdec_daily_AFD_QA, cdec_daily_AFD[4001:5000,])

ggplotly(
  ggplot() + geom_point(data=cdec_daily_AFD[5001:6000,], aes(x=date, y=value_mean_C)))

cdec_daily_AFD_QA <- rbind(cdec_daily_AFD_QA, cdec_daily_AFD[5001:6000,])

ggplotly(
  ggplot() + geom_point(data=cdec_daily_AFD[6001:7000,], aes(x=date, y=value_mean_C)))

cdec_daily_AFD_QA <- rbind(cdec_daily_AFD_QA, cdec_daily_AFD[6001:7000,])

ggplotly(
  ggplot() + geom_point(data=cdec_daily_AFD[7001:8000,], aes(x=date, y=value_mean_C)))

cdec_daily_AFD_QA <- rbind(cdec_daily_AFD_QA, cdec_daily_AFD[7001:7407,])

# Final review ------------------------------------------------------------

#plot QA'd dataset to confirm all points look good
ggplotly(
  ggplot() +geom_point(data = cdec_daily_AFD_QA, aes(x=date, y=value_mean_C)))

#save QA'd dataset as a .rds file
write_rds(cdec_daily_AFD_QA, path = "data/QA_data/cdec_daily_AFD_QA.rds")

#update the gage_QA_progress
gage_QA_progress <- read_csv("data/data_review/gage_QA_progress.csv")

#note reviewer initials, whether review is complete, and any final notes
gage_QA_progress[gage_QA_progress$site_id=="AFD",6:8] <- c("ADW", "Y", "QA complete")

#save updated dataframe to the .csv
write_csv(gage_QA_progress, path = "data/data_review/gage_QA_progress.csv")
