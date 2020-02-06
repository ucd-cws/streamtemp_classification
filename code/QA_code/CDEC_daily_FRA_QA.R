
# Code description --------------------------------------------------------

# Code to review temperature data from gage CDEC_FRA.

library(tidyverse)
library(lubridate)

# Get Data ----------------------------------------------------------------

siteName <- "FRA"

file_list <- list.files("data/data_review/")

# Get index number of file name of interest:
(siteFile <- which(grepl(siteName, file_list)))

# read in the file
cdec_daily_FRA <- read_rds(path = paste0("data/data_review/",
                                         file_list[[siteFile]]))

# Plot --------------------------------------------------------------------

library(plotly)

# now make an interactive plot of first 1000 values
ggplotly(
  ggplot() + geom_point(data=cdec_daily_FRA[1:1000,], aes(x=date, y=value_mean_C)))

# Review, QA, and Repeat --------------------------------------------------

cdec_daily_FRA_QA_1300 <- cdec_daily_FRA[1300:1700,]

# make an interactive plot of points 1001-2000
ggplotly(
  ggplot() +geom_point(data = cdec_daily_FRA_QA_1300, aes(x=date, y=value_mean_C)))

# filter for just OCT
cdec_daily_FRA_QA_1300 <- cdec_daily_FRA %>% filter(month=="October" & year==2005)

ggplotly(
  ggplot() +geom_point(data = cdec_daily_FRA_QA_1300, aes(x=date, y=value_mean_C)))

# values below 10.61 look weird, drop them
cdec_daily_FRA_QA_1300 <- filter(cdec_daily_FRA_QA_1300, value_mean_C>10.61)

ggplotly(
  ggplot() +geom_point(data = cdec_daily_FRA_QA_1300, aes(x=date, y=value_mean_C)))


# rebind with main set, first drop section from main data
cdec_daily_FRA_QA <- cdec_daily_FRA %>% 
  filter(!month=="October" & !year==2005)

# then rebind with clean
cdec_daily_FRA_QA <- rbind(cdec_daily_FRA_QA, cdec_daily_FRA_QA_1300)

ggplotly(
  ggplot() +geom_point(data = cdec_daily_FRA_QA, aes(x=date, y=value_mean_C)))

# Final review ------------------------------------------------------------

#plot QA'd dataset to confirm all points look good
ggplotly(
  ggplot() +geom_point(data = cdec_daily_FRA_QA, aes(x=date, y=value_mean_C)))

#save QA'd dataset as a .rds file
write_rds(cdec_daily_FRA_QA, path = "data/QA_data/cdec_daily_FRA_QA.rds")

#update the gage_QA_progress
gage_QA_progress <- read_csv("data/data_review/gage_QA_progress.csv")

#note reviewer initials, whether review is complete, and any final notes
gage_QA_progress[gage_QA_progress$site_id=="FRA",4:6] <- c("RAP", "Y", "QA complete")

#save updated dataframe to the .csv
write_csv(gage_QA_progress, path = "data/data_review/gage_QA_progress.csv")
