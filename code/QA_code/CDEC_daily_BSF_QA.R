
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

##start QAing dataset from here


# Final review ------------------------------------------------------------

#plot QA'd dataset to confirm all points look good
ggplotly(
  ggplot() +geom_point(data = cdec_daily_BSF_QA, aes(x=date, y=value_mean_C)))

#save QA'd dataset as a .rds file
write_rds(cdec_daily_BSF_QA, path = "data/QA_data/cdec_daily_BSF_QA.rds")

#update the gage_QA_progress
gage_QA_progress <- read_csv("data/data_review/gage_QA_progress.csv")

#note reviewer initials, whether review is complete, and any final notes
gage_QA_progress[gage_QA_progress$site_id=="BSF",4:6] <- c("ADW", "N", "In progress, see notes in code")

#save updated dataframe to the .csv
write_csv(gage_QA_progress, path = "data/data_review/gage_QA_progress.csv")
