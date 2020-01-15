
# Code description --------------------------------------------------------

# Code to review temperature data from gage CDEC_BIC.

library(tidyverse)
library(lubridate)
library(weathermetrics)
library(plotly)

# Get Data ----------------------------------------------------------------

file_list <- list.files("data/data_review/")

# read in next file:
file_list[[7]]

cdec_daily_BIC <- read_rds(path = paste0("data/data_review/",file_list[[7]]))

# Plot --------------------------------------------------------------------

#plot all data to look for obvious bad data points and gaps
ggplotly(
  ggplot() + geom_point(data=cdec_daily_BIC[,], aes(x=date, y=value_mean_C)))

# now make an interactive plot of first 1000 values
ggplotly(
  ggplot() + geom_point(data=cdec_daily_BIC[1:1000,], aes(x=date, y=value_mean_C)))

# Review, QA, and Repeat --------------------------------------------------
cdec_daily_BIC_1_1000 <- cdec_daily_BIC[1:1000,] %>% 
  filter(value_mean_C > 0, value_mean_C < 30, date != "2001-06-06", date != "2001-06-07")

ggplotly(
  ggplot() + geom_point(data=cdec_daily_BIC_1_1000, aes(x=date, y=value_mean_C)))

cdec_daily_BIC_QA <- cdec_daily_BIC_1_1000

ggplotly(
  ggplot() + geom_point(data=cdec_daily_BIC[1001:2000,], aes(x=date, y=value_mean_C)))

cdec_daily_BIC_1001_2000 <- cdec_daily_BIC[1001:2000,] %>% 
  filter(value_mean_C > 3.00)

ggplotly(
  ggplot() + geom_point(data=cdec_daily_BIC_1001_2000, aes(x=date, y=value_mean_C)))

cdec_daily_BIC_QA <- rbind(cdec_daily_BIC_QA, cdec_daily_BIC_1001_2000)

ggplotly(
  ggplot() + geom_point(data=cdec_daily_BIC[2001:3000,], aes(x=date, y=value_mean_C)))

cdec_daily_BIC_2001_3000 <- cdec_daily_BIC[2001:3000,] %>% 
  filter(value_mean_C < 29.0)

ggplotly(
  ggplot() + geom_point(data=cdec_daily_BIC_2001_3000, aes(x=date, y=value_mean_C)))

cdec_daily_BIC_QA <- rbind(cdec_daily_BIC_QA, cdec_daily_BIC_2001_3000)

ggplotly(
  ggplot() + geom_point(data=cdec_daily_BIC[3001:4000,], aes(x=date, y=value_mean_C)))

cdec_daily_BIC_QA <- rbind(cdec_daily_BIC_QA, cdec_daily_BIC[3001:4000,])

ggplotly(
  ggplot() + geom_point(data=cdec_daily_BIC[4001:5000,], aes(x=date, y=value_mean_C)))

cdec_daily_BIC_4001_5000 <- cdec_daily_BIC[4001:5000,] %>% 
  filter(value_mean_C > 0)

ggplotly(
  ggplot() + geom_point(data=cdec_daily_BIC_4001_5000, aes(x=date, y=value_mean_C)))

cdec_daily_BIC_QA <- rbind(cdec_daily_BIC_QA,cdec_daily_BIC_4001_5000)

ggplotly(
  ggplot() + geom_point(data=cdec_daily_BIC[5001:5838,], aes(x=date, y=value_mean_C)))

cdec_daily_BIC_QA <- rbind(cdec_daily_BIC_QA, cdec_daily_BIC[5001:5838,])

# Final review ------------------------------------------------------------

#plot QA'd dataset to confirm all points look good
ggplotly(
  ggplot() +geom_point(data = cdec_daily_BIC_QA, aes(x=date, y=value_mean_C)))

#save QA'd dataset as a .rds file
write_rds(cdec_daily_BIC_QA, path = "data/QA_data/cdec_daily_BIC_QA.rds")

#update the gage_QA_progress
gage_QA_progress <- read_csv("data/data_review/gage_QA_progress.csv")

#note reviewer initials, whether review is complete, and any final notes
gage_QA_progress[gage_QA_progress$site_id=="BIC",4:6] <- c("ADW", "Y", "QA complete")

#save updated dataframe to the .csv
write_csv(gage_QA_progress, path = "data/data_review/gage_QA_progress.csv")
