
# Code description --------------------------------------------------------

# Code to review temperature data from gage CDEC_BBQ.

library(tidyverse)
library(lubridate)
library(weathermetrics)
library(plotly)

# Get Data ----------------------------------------------------------------

file_list <- list.files("data/data_review/")

# read in next file:
file_list[[3]]

cdec_daily_BBQ <- read_rds(path = paste0("data/data_review/",file_list[[3]]))

# Plot --------------------------------------------------------------------

# now make an interactive plot of first 1000 values
ggplotly(
  ggplot() + geom_point(data=cdec_daily_BBQ[1:1000,], aes(x=date, y=value_mean_C)))

# Review, QA, and Repeat --------------------------------------------------
cdec_daily_BBQ_1_1000 <- cdec_daily_BBQ[1:1000,]

cdec_daily_BBQ_1_1000 <- cdec_daily_BBQ_1_1000[-c(799:805),]

ggplotly(
  ggplot() + geom_point(data=cdec_daily_BBQ_1_1000, aes(x=date, y=value_mean_C)))

cdec_daily_BBQ_QA <- cdec_daily_BBQ_1_1000

ggplotly(
  ggplot() + geom_point(data=cdec_daily_BBQ[1001:2000,], aes(x=date, y=value_mean_C)))

cdec_daily_BBQ_1001_2000 <- cdec_daily_BBQ[1001:2000,]

cdec_daily_BBQ_1001_2000 <- cdec_daily_BBQ_1001_2000[-c(452:536),]

cdec_daily_BBQ_1001_2000 <- cdec_daily_BBQ_1001_2000[-c(632:673),]

cdec_daily_BBQ_1001_2000 <- filter(cdec_daily_BBQ_1001_2000,date != "2014-10-14")

ggplotly(
  ggplot() + geom_point(data=cdec_daily_BBQ_1001_2000, aes(x=date, y=value_mean_C)))

cdec_daily_BBQ_QA <- rbind(cdec_daily_BBQ_QA,cdec_daily_BBQ_1001_2000)

ggplotly(
  ggplot() + geom_point(data=cdec_daily_BBQ[2001:3000,], aes(x=date, y=value_mean_C)))

cdec_daily_BBQ_2001_3000 <- cdec_daily_BBQ[2001:3000,]

cdec_daily_BBQ_2001_3000 <- cdec_daily_BBQ_2001_3000[-c(90:158),]

cdec_daily_BBQ_2001_3000 <- filter(cdec_daily_BBQ_2001_3000,date != "2017-06-08", date != "2016-07-23", date != "2016-08-02", date != "2016-08-16", date != "2016-08-28", date != "2016-09-10", date != "2016-09-12")

cdec_daily_BBQ_2001_3000 <- cdec_daily_BBQ_2001_3000[-c(646:648),]

cdec_daily_BBQ_2001_3000 <- cdec_daily_BBQ_2001_3000[-c(668:672),]

ggplotly(
  ggplot() + geom_point(data=cdec_daily_BBQ_2001_3000, aes(x=date, y=value_mean_C)))

cdec_daily_BBQ_QA <- rbind(cdec_daily_BBQ_QA,cdec_daily_BBQ_2001_3000)

ggplotly(
  ggplot() + geom_point(data=cdec_daily_BBQ[3001:3521,], aes(x=date, y=value_mean_C)))

cdec_daily_BBQ_3001_3521 <- cdec_daily_BBQ[3001:3521,] %>% 
  filter(date != "2018-07-31", date != "2018-08-01", date != "2018-08-12", date != "2018-08-13", date != "2018-08-24")

cdec_daily_BBQ_3001_3521 <- cdec_daily_BBQ_3001_3521[-c(125:129),]

cdec_daily_BBQ_3001_3521 <- cdec_daily_BBQ_3001_3521[-c(374:377),]

ggplotly(
  ggplot() + geom_point(data=cdec_daily_BBQ_3001_3521, aes(x=date, y=value_mean_C)))

cdec_daily_BBQ_QA <- rbind(cdec_daily_BBQ_QA,cdec_daily_BBQ_3001_3521)

# Final review ------------------------------------------------------------

#plot QA'd dataset to confirm all points look good
ggplotly(
  ggplot() +geom_point(data = cdec_daily_BBQ_QA, aes(x=date, y=value_mean_C)))

#save QA'd dataset as a .rds file
write_rds(cdec_daily_BBQ_QA, path = "data/QA_data/cdec_daily_BBQ_QA.rds")

#update the gage_QA_progress
gage_QA_progress <- read_csv("data/data_review/gage_QA_progress.csv")

#note reviewer initials, whether review is complete, and any final notes
gage_QA_progress[gage_QA_progress$site_id=="BBQ",4:6] <- c("ADW", "Y", "QA complete")

#save updated dataframe to the .csv
write_csv(gage_QA_progress, path = "data/data_review/gage_QA_progress.csv")
