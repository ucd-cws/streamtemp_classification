
# Code description --------------------------------------------------------

# Code to review temperature data from gage CDEC_CRS.

library(tidyverse)
library(lubridate)


# Get Data ----------------------------------------------------------------

file_list <- list.files("data/data_review/")

# read in next file:
file_list[[14]]

cdec_daily_CRS <- read_rds(path = paste0("data/data_review/",file_list[[14]]))

# Plot --------------------------------------------------------------------

library(plotly)

ggplotly(
  ggplot() + geom_point(data=cdec_daily_CRS[,], aes(x=date, y=value_mean_C)))

# now make an interactive plot of first 1000 value_mean_C_mean_Cs
ggplotly(
  ggplot() + geom_point(data=cdec_daily_CRS[1:1000,], aes(x=date, y=value_mean_C)))


# Review, QA, and Repeat --------------------------------------------------

cdec_daily_CRS_1_1000 <- cdec_daily_CRS[1:1000,] %>%
  filter(date != "2000-10-31")

ggplotly(
  ggplot() + geom_point(data=cdec_daily_CRS_1_1000[1:1000,], aes(x=date, y=value_mean_C)))

cdec_daily_CRS_QA <- cdec_daily_CRS_1_1000

# make an interactive plot of points 1001-2000
ggplotly(
  ggplot() +geom_point(data = cdec_daily_CRS[1001:2000,], aes(x=date, y=value_mean_C)))

#points look good, bind to QA'd dataframe
cdec_daily_CRS_QA <- rbind(cdec_daily_CRS_QA, cdec_daily_CRS[1001:2000,])

#make an interactive plot of points 2001-3000
ggplotly(
  ggplot() +geom_point(data = cdec_daily_CRS[2001:3000,], aes(x=date, y=value_mean_C)))

cdec_daily_CRS_2001_3000 <- cdec_daily_CRS[2001:3000,] %>% 
  filter(value_mean_C > 2.67)

ggplotly(
  ggplot() +geom_point(data = cdec_daily_CRS_2001_3000, aes(x=date, y=value_mean_C)))

cdec_daily_CRS_QA <- rbind(cdec_daily_CRS_QA, cdec_daily_CRS_2001_3000)

#make an interactive plot of points 3001-4000
ggplotly(
  ggplot() +geom_point(data = cdec_daily_CRS[3001:4000,], aes(x=date, y=value_mean_C)))

cdec_daily_CRS_QA <- rbind(cdec_daily_CRS_QA, cdec_daily_CRS[3001:4000,])

#make an interactive plot of points 4001-5000
ggplotly(
  ggplot() +geom_point(data = cdec_daily_CRS[4001:5000,], aes(x=date, y=value_mean_C)))

cdec_daily_CRS_4001_5000 <- cdec_daily_CRS[4001:5000,] %>% 
  filter(value_mean_C > 6.61)

ggplotly(
  ggplot() +geom_point(data = cdec_daily_CRS_4001_5000, aes(x=date, y=value_mean_C)))

cdec_daily_CRS_QA <- rbind(cdec_daily_CRS_QA, cdec_daily_CRS_4001_5000)

#make an interactive plot of 5001-6486
ggplotly(
  ggplot() +geom_point(data = cdec_daily_CRS[5001:6486,], aes(x=date, y=value_mean_C)))

cdec_daily_CRS_5001_6486 <- cdec_daily_CRS[5001:6486,] %>% 
  filter(value_mean_C > 1.56)

ggplotly(
  ggplot() +geom_point(data = cdec_daily_CRS_5001_6486, aes(x=date, y=value_mean_C)))

cdec_daily_CRS_QA <- rbind(cdec_daily_CRS_QA, cdec_daily_CRS_5001_6486)

#plot QA'd dataset to confirm all points look good
ggplotly(
  ggplot() +geom_point(data = cdec_daily_CRS_QA, aes(x=date, y=value_mean_C)))

#save QA'd dataset as a .rds file
write_rds(cdec_daily_CRS_QA, path = "data/QA_data/cdec_daily_CRS_QA.rds")

#update the gage_QA_progress
gage_QA_progress <- read_csv("data/data_review/gage_QA_progress.csv")

#note reviewer initials, whether review is complete, and any final notes
gage_QA_progress[gage_QA_progress$site_id=="CRS",4:6] <- c("ADW", "Y", "QA complete")

#save updated dataframe to the .csv
write_csv(gage_QA_progress, path = "data/data_review/gage_QA_progress.csv")
