
# Code description --------------------------------------------------------

# Code to review temperature data from gage CDEC_SJS.

library(tidyverse)
library(lubridate)
library(weathermetrics)
library(plotly)

# Get Data ----------------------------------------------------------------

file_list <- list.files("data/data_review/")

# read in next file:
file_list[[53]]

cdec_daily_SJS <- read_rds(path = paste0("data/data_review/",file_list[[53]]))

# Plot --------------------------------------------------------------------

ggplotly(
  ggplot() + geom_point(data=cdec_daily_SJS[,], aes(x=date, y=value_mean_C)))

# now make an interactive plot of first 1000 value_mean_Cs
ggplotly(
  ggplot() + geom_point(data=cdec_daily_SJS[1:1000,], aes(x=date, y=value_mean_C)))

# Review, QA, and Repeat --------------------------------------------------

cdec_daily_SJS_QA <- cdec_daily_SJS[1:1000,]

ggplotly(
  ggplot() + geom_point(data=cdec_daily_SJS[1001:2000,], aes(x=date, y=value_mean_C)))

cdec_daily_SJS_1001_2000 <- cdec_daily_SJS[1001:2000,] 

cdec_daily_SJS_1001_2000 <- cdec_daily_SJS_1001_2000[-c(36:50),]

ggplotly(
  ggplot() + geom_point(data=cdec_daily_SJS_1001_2000, aes(x=date, y=value_mean_C)))

cdec_daily_SJS_QA <- rbind(cdec_daily_SJS_QA, cdec_daily_SJS_1001_2000)

ggplotly(
  ggplot() + geom_point(data=cdec_daily_SJS[2001:3000,], aes(x=date, y=value_mean_C)))

cdec_daily_SJS_2001_3000 <- cdec_daily_SJS[2001:3000,]

cdec_daily_SJS_2001_3000 <- cdec_daily_SJS_2001_3000[-c(478:577),]

ggplotly(
  ggplot() + geom_point(data=cdec_daily_SJS_2001_3000, aes(x=date, y=value_mean_C)))

cdec_daily_SJS_QA <- rbind(cdec_daily_SJS_QA,cdec_daily_SJS_2001_3000)

ggplotly(
  ggplot() + geom_point(data=cdec_daily_SJS[3001:4000,], aes(x=date, y=value_mean_C)))

cdec_daily_SJS_3001_4000 <- cdec_daily_SJS[3001:4000,] %>% 
  filter(value_mean_C > fahrenheit.to.celsius(45.1), date != "2013-02-24")

ggplotly(
  ggplot() + geom_point(data=cdec_daily_SJS_3001_4000, aes(x=date, y=value_mean_C)))

cdec_daily_SJS_QA <- rbind(cdec_daily_SJS_QA, cdec_daily_SJS_3001_4000)

ggplotly(
  ggplot() + geom_point(data=cdec_daily_SJS[4001:5000,], aes(x=date, y=value_mean_C)))

cdec_daily_SJS_4001_5000 <- cdec_daily_SJS[4001:5000,] %>%
  filter(date != "2016-02-07")

ggplotly(
  ggplot() + geom_point(data=cdec_daily_SJS_4001_5000, aes(x=date, y=value_mean_C)))


cdec_daily_SJS_QA <- rbind(cdec_daily_SJS_QA, cdec_daily_SJS_4001_5000)

ggplotly(
  ggplot() + geom_point(data=cdec_daily_SJS[5001:6164,], aes(x=date, y=value_mean_C)))

cdec_daily_SJS_5001_6164 <- cdec_daily_SJS[5001:6164,] %>% 
  filter(value_mean_C < fahrenheit.to.celsius(93.3), value_mean_C > fahrenheit.to.celsius(32), date != "2019-04-30", date != "2019-04-25", date!= "2017-08-15")

ggplotly(
  ggplot() + geom_point(data=cdec_daily_SJS_5001_6164, aes(x=date, y=value_mean_C)))

cdec_daily_SJS_QA <- rbind(cdec_daily_SJS_QA, cdec_daily_SJS_5001_6164)



# Final review ------------------------------------------------------------

#plot QA'd dataset to confirm all points look good
ggplotly(
  ggplot() +geom_point(data = cdec_daily_SJS_QA, aes(x=date, y=value_mean_C)))

#save QA'd dataset as a .rds file
write_rds(cdec_daily_SJS_QA, path = "data/QA_data/cdec_daily_SJS_QA.rds")

#update the gage_QA_progress
gage_QA_progress <- read_csv("data/data_review/gage_QA_progress.csv")

#note reviewer initials, whether review is complete, and any final notes
gage_QA_progress[gage_QA_progress$site_id=="SJS",4:6] <- c("ADW", "Y", "QA complete")

#save updated dataframe to the .csv
write_csv(gage_QA_progress, path = "data/data_review/gage_QA_progress.csv")
