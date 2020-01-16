
# Code description --------------------------------------------------------

# Code to review temperature data from gage CDEC_BAS.

library(tidyverse)
library(lubridate)
library(weathermetrics)
library(plotly)

# Get Data ----------------------------------------------------------------

file_list <- list.files("data/data_review/")

# read in next file:
file_list[[2]]

cdec_daily_BAS <- read_rds(path = paste0("data/data_review/",file_list[[2]]))

# Plot --------------------------------------------------------------------

# now make an interactive plot of first 1000 values
ggplotly(
  ggplot() + geom_point(data=cdec_daily_BAS[1:1000,], aes(x=date, y=value_mean_C)))


# Review, QA, and Repeat --------------------------------------------------

cdec_daily_BAS_QA <- cdec_daily_BAS[1:1000,]

ggplotly(
  ggplot() + geom_point(data=cdec_daily_BAS[1001:2000,], aes(x=date, y=value_mean_C)))

cdec_daily_BAS_1001_2000 <- cdec_daily_BAS[1001:2000,]

cdec_daily_BAS_1001_2000 <- cdec_daily_BAS_1001_2000[-c(602:608),]

ggplotly(
  ggplot() + geom_point(data=cdec_daily_BAS_1001_2000, aes(x=date, y=value_mean_C)))

cdec_daily_BAS_QA <- rbind(cdec_daily_BAS_QA,cdec_daily_BAS_1001_2000)

ggplotly(
  ggplot() + geom_point(data=cdec_daily_BAS[2001:3000,], aes(x=date, y=value_mean_C)))

cdec_daily_BAS_QA <- rbind(cdec_daily_BAS_QA,cdec_daily_BAS[2001:3000,])

ggplotly(
  ggplot() + geom_point(data=cdec_daily_BAS[3001:3267,], aes(x=date, y=value_mean_C)))

cdec_daily_BAS_3001_3267 <- cdec_daily_BAS[3001:3267,] %>% 
  filter(date != "2019-02-13")

ggplotly(
  ggplot() + geom_point(data=cdec_daily_BAS_3001_3267, aes(x=date, y=value_mean_C)))

cdec_daily_BAS_QA <- rbind(cdec_daily_BAS_QA,cdec_daily_BAS[3001:3267,])

# Final review ------------------------------------------------------------

#plot QA'd dataset to confirm all points look good
ggplotly(
  ggplot() +geom_point(data = cdec_daily_BAS_QA, aes(x=date, y=value_mean_C)))

#save QA'd dataset as a .rds file
write_rds(cdec_daily_BAS_QA, path = "data/QA_data/cdec_daily_BAS_QA.rds")

#update the gage_QA_progress
gage_QA_progress <- read_csv("data/data_review/gage_QA_progress.csv")

#note reviewer initials, whether review is complete, and any final notes
gage_QA_progress[gage_QA_progress$site_id=="BAS",4:6] <- c("ADW", "Y", "QA complete")

#save updated dataframe to the .csv
write_csv(gage_QA_progress, path = "data/data_review/gage_QA_progress.csv")
