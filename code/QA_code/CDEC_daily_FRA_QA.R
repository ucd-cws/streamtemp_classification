
# Code description --------------------------------------------------------

# Code to review temperature data from gage CDEC_FRA.

library(tidyverse)
library(lubridate)


# Get Data ----------------------------------------------------------------

file_list <- list.files("data/data_review/")

# read in next file:
file_list[[2]]

cdec_daily_FRA <- read_rds(path = paste0("data/data_review/",file_list[[2]]))

# Plot --------------------------------------------------------------------

library(plotly)

# now make an interactive plot of first 1000 values
ggplotly(
  ggplot() + geom_point(data=cdec_daily_FRA[1:1000,], aes(x=datetime, y=value)))

# Review, QA, and Repeat --------------------------------------------------

cdec_daily_FRA_QA <- cdec_daily_FRA[1:1000,]

# make an interactive plot of points 1001-2000
ggplotly(
  ggplot() +geom_point(data = cdec_daily_FRA[1001:2000,], aes(x=datetime, y=value)))

cdec_daily_FRA_1001_2000 <- cdec_daily_FRA[1001:2000,] %>% 
  filter(value > 44.6)

ggplotly(
  ggplot() +geom_point(data = cdec_daily_FRA_1001_2000, aes(x=datetime, y=value)))

cdec_daily_FRA_QA <- rbind(cdec_daily_FRA_QA, cdec_daily_FRA_1001_2000)

ggplotly(
  ggplot() +geom_point(data = cdec_daily_FRA[2001:3000,], aes(x=datetime, y=value)))

cdec_daily_FRA_QA <- rbind(cdec_daily_FRA_QA, cdec_daily_FRA[2001:3000,])

ggplotly(
  ggplot() +geom_point(data = cdec_daily_FRA[3001:4000,], aes(x=datetime, y=value)))

cdec_daily_FRA_QA <- rbind(cdec_daily_FRA_QA, cdec_daily_FRA[3001:4000,])

ggplotly(
  ggplot() +geom_point(data = cdec_daily_FRA[4001:5000,], aes(x=datetime, y=value)))

cdec_daily_FRA_QA <- rbind(cdec_daily_FRA_QA, cdec_daily_FRA[4001:5000,])

ggplotly(
  ggplot() +geom_point(data = cdec_daily_FRA[5001:6447,], aes(x=datetime, y=value)))

cdec_daily_FRA_QA <- rbind(cdec_daily_FRA_QA, cdec_daily_FRA[5001:6447,])

# Final review ------------------------------------------------------------

#plot QA'd dataset to confirm all points look good
ggplotly(
  ggplot() +geom_point(data = cdec_daily_FRA_QA, aes(x=datetime, y=value)))

#save QA'd dataset as a .rds file
write_rds(cdec_daily_FRA_QA, path = "data/QA_data/cdec_daily_FRA_QA.rds")

#update the gage_QA_progress
gage_QA_progress <- read_csv("data/data_review/gage_QA_progress.csv")

#confirm correct row to update by the site_id
gage_QA_progress[30,1]

#note reviewer initials, whether review is complete, and any final notes
gage_QA_progress[30,4:6] <- c("ADW", "Y", "QA complete")

#save updated dataframe to the .csv
write_csv(gage_QA_progress, path = "data/data_review/gage_QA_progress.csv")
