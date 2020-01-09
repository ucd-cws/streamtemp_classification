
# Code description --------------------------------------------------------

# Code to review temperature data from gage CDEC_SJS.

library(tidyverse)
library(lubridate)
library(weathermetrics)
library(plotly)

# Get Data ----------------------------------------------------------------

file_list <- list.files("data/data_review/")

# read in next file:
file_list[[6]]

cdec_daily_SJS <- read_rds(path = paste0("data/data_review/",file_list[[6]]))

# Plot --------------------------------------------------------------------

# now make an interactive plot of first 1000 values
ggplotly(
  ggplot() + geom_point(data=cdec_daily_SJS[1:1000,], aes(x=datetime, y=value)))

# Review, QA, and Repeat --------------------------------------------------

cdec_daily_SJS_QA <- cdec_daily_SJS[1:1000,]

ggplotly(
  ggplot() + geom_point(data=cdec_daily_SJS[1001:2000,], aes(x=datetime, y=value)))

cdec_daily_SJS_1001_2000 <- cdec_daily_SJS[1001:2000,] 

cdec_daily_SJS_1001_2000 <- cdec_daily_SJS_1001_2000[-c(36:50),]

ggplotly(
  ggplot() + geom_point(data=cdec_daily_SJS_1001_2000, aes(x=datetime, y=value)))

cdec_daily_SJS_QA <- rbind(cdec_daily_SJS_QA, cdec_daily_SJS_1001_2000)

ggplotly(
  ggplot() + geom_point(data=cdec_daily_SJS[2001:3000,], aes(x=datetime, y=value)))

cdec_daily_SJS_2001_3000 <- cdec_daily_SJS[2001:3000,]

cdec_daily_SJS_2001_3000 <- cdec_daily_SJS_2001_3000[-c(478:577),]

ggplotly(
  ggplot() + geom_point(data=cdec_daily_SJS_2001_3000, aes(x=datetime, y=value)))

cdec_daily_SJS_QA <- rbind(cdec_daily_SJS_QA,cdec_daily_SJS_2001_3000)

ggplotly(
  ggplot() + geom_point(data=cdec_daily_SJS[3001:4000,], aes(x=datetime, y=value)))

cdec_daily_SJS_3001_4000 <- cdec_daily_SJS[3001:4000,] %>% 
  filter(value < 100, value > 45.1, datetime != "2013-02-24")

ggplotly(
  ggplot() + geom_point(data=cdec_daily_SJS_3001_4000, aes(x=datetime, y=value)))

cdec_daily_SJS_QA <- rbind(cdec_daily_SJS_QA, cdec_daily_SJS_3001_4000)

ggplotly(
  ggplot() + geom_point(data=cdec_daily_SJS[4001:5000,], aes(x=datetime, y=value)))

cdec_daily_SJS_4001_5000 <- cdec_daily_SJS[4001:5000,] %>%
  filter(value < 100, datetime != "2016-02-07")

ggplotly(
  ggplot() + geom_point(data=cdec_daily_SJS_4001_5000, aes(x=datetime, y=value)))


cdec_daily_SJS_QA <- rbind(cdec_daily_SJS_QA, cdec_daily_SJS_4001_5000)

ggplotly(
  ggplot() + geom_point(data=cdec_daily_SJS[5001:6164,], aes(x=datetime, y=value)))

cdec_daily_SJS_5001_6164 <- cdec_daily_SJS[5001:6164,] %>% 
  filter(value < 93.3, value > 32, datetime != "2019-04-30", datetime != "2019-04-25")

ggplotly(
  ggplot() + geom_point(data=cdec_daily_SJS_5001_6164, aes(x=datetime, y=value)))

cdec_daily_SJS_QA <- rbind(cdec_daily_SJS_QA, cdec_daily_SJS_5001_6164)



# Final review ------------------------------------------------------------

#plot QA'd dataset to confirm all points look good
ggplotly(
  ggplot() +geom_point(data = cdec_daily_SJS_QA, aes(x=datetime, y=value)))

#save QA'd dataset as a .rds file
write_rds(cdec_daily_SJS_QA, path = "data/QA_data/cdec_daily_SJS_QA.rds")

#update the gage_QA_progress
gage_QA_progress <- read_csv("data/data_review/gage_QA_progress.csv")

#confirm correct row to update by the site_id
gage_QA_progress[95,1]

#note reviewer initials, whether review is complete, and any final notes
gage_QA_progress[95,4:6] <- c("ADW", "Y", "QA complete")

#save updated dataframe to the .csv
write_csv(gage_QA_progress, path = "data/data_review/gage_QA_progress.csv")
