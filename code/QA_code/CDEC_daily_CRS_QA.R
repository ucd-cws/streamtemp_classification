
# Code description --------------------------------------------------------

# Code to review temperature data from gage CDEC_CRS.

library(tidyverse)
library(lubridate)


# Get Data ----------------------------------------------------------------

file_list <- list.files("data/data_review/")

# read in next file:
file_list[[1]]

cdec_daily_CRS <- read_rds(path = paste0("data/data_review/",file_list[[1]]))

# Plot --------------------------------------------------------------------

library(plotly)

# now make an interactive plot of first 1000 values
ggplotly(
  ggplot() + geom_point(data=cdec_daily_CRS[1:1000,], aes(x=datetime, y=value)))


# Review, QA, and Repeat --------------------------------------------------

cdec_daily_CRS_1_1000 <- cdec_daily_CRS[1:1000,] %>%
  filter(datetime != "2000-10-31")

ggplotly(
  ggplot() + geom_point(data=cdec_daily_CRS_1_1000[1:1000,], aes(x=datetime, y=value)))

cdec_daily_CRS_QA <- cdec_daily_CRS_1_1000

# make an interactive plot of points 1001-2000
ggplotly(
  ggplot() +geom_point(data = cdec_daily_CRS[1001:2000,], aes(x=datetime, y=value)))

#points look good, bind to QA'd dataframe
cdec_daily_CRS_QA <- rbind(cdec_daily_CRS_QA, cdec_daily_CRS[1001:2000,])

#make an interactive plot of points 2001-3000
ggplotly(
  ggplot() +geom_point(data = cdec_daily_CRS[2001:3000,], aes(x=datetime, y=value)))

cdec_daily_CRS_2001_3000 <- cdec_daily_CRS[2001:3000,] %>% 
  filter(value > 37)

ggplotly(
  ggplot() +geom_point(data = cdec_daily_CRS_2001_3000, aes(x=datetime, y=value)))

cdec_daily_CRS_QA <- rbind(cdec_daily_CRS_QA, cdec_daily_CRS_2001_3000)

#make an interactive plot of points 3001-4000
ggplotly(
  ggplot() +geom_point(data = cdec_daily_CRS[3001:4000,], aes(x=datetime, y=value)))

cdec_daily_CRS_QA <- rbind(cdec_daily_CRS_QA, cdec_daily_CRS[3001:4000,])

#make an interactive plot of points 4001-5000
ggplotly(
  ggplot() +geom_point(data = cdec_daily_CRS[4001:5000,], aes(x=datetime, y=value)))

cdec_daily_CRS_4001_5000 <- cdec_daily_CRS[4001:5000,] %>% 
  filter(value > 43.9)

ggplotly(
  ggplot() +geom_point(data = cdec_daily_CRS_4001_5000, aes(x=datetime, y=value)))

cdec_daily_CRS_QA <- rbind(cdec_daily_CRS_QA, cdec_daily_CRS_4001_5000)

#make an interactive plot of 5001-6486
ggplotly(
  ggplot() +geom_point(data = cdec_daily_CRS[5001:6486,], aes(x=datetime, y=value)))

cdec_daily_CRS_5001_6486 <- cdec_daily_CRS[5001:6486,] %>% 
  filter(value > 34.8)

ggplotly(
  ggplot() +geom_point(data = cdec_daily_CRS_5001_6486, aes(x=datetime, y=value)))

cdec_daily_CRS_QA <- rbind(cdec_daily_CRS_QA, cdec_daily_CRS_5001_6486)

#plot QA'd dataset to confirm all points look good
ggplotly(
  ggplot() +geom_point(data = cdec_daily_CRS_QA, aes(x=datetime, y=value)))

#save QA'd dataset as a .rds file
write_rds(cdec_daily_CRS_QA, path = "data/QA_data/cdec_daily_CRS_QA.rds")

#update the gage_QA_progress
gage_QA_progress <- read_csv("data/data_review/gage_QA_progress.csv")

#confirm correct row to update by the site_id
gage_QA_progress[20,1]

#note reviewer initials, whether review is complete, and any final notes
gage_QA_progress[20,4:6] <- c("ADW", "Y", "QA complete")

#save updated dataframe to the .csv
write_csv(gage_QA_progress, path = "data/data_review/gage_QA_progress.csv")
