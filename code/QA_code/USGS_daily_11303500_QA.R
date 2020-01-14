
# Code description --------------------------------------------------------

# Code to review temperature data from gage usgs_11303500.

library(tidyverse)
library(lubridate)
library(weathermetrics)
library(plotly)

# Get Data ----------------------------------------------------------------

file_list <- list.files("data/data_review/")

# read in next file:
file_list[[84]]

# read in and assign name
usgs_daily_11303500 <- read_rds(path = paste0("data/data_review/",file_list[[84]]))

# fix names using dataRetrieval package
names(usgs_daily_11303500)
names(dataRetrieval::renameNWISColumns(usgs_daily_11303500)) # quick test to see change
# now assign new names
usgs_daily_11303500 <- renameNWISColumns(usgs_daily_11303500)
names(usgs_daily_11303500)


# Check for Error Flags in Data -------------------------------------------

# usgs uses flags in data...check with table()
table(usgs_daily_11303500$Wtemp_cd)

# filter out all values that aren't "A"
usgs_daily_11303500_filt <- usgs_daily_11303500 %>% filter(Wtemp_cd=="A")


# Plot --------------------------------------------------------------------

#plot full dataset to see what it looks like and quick QA for large gaps

ggplot() + geom_point(data=usgs_daily_11303500_filt, 
                      aes(x=Date, y=Wtemp))

# now make an interactive plot of first 1000 values
ggplotly(
  ggplot() + geom_point(data=usgs_daily_11303500_filt[1:1000,], aes(x=Date, y=Wtemp)))

# Review, QA, and Repeat --------------------------------------------------

usgs_daily_11303500_QA <- usgs_daily_11303500_filt[1:1000,]

ggplotly(
  ggplot() + geom_point(data=usgs_daily_11303500_filt[1001:2000,], aes(x=Date, y=Wtemp)))

usgs_daily_11303500_QA <- usgs_daily_11303500_filt[1:2000,]

ggplotly(
  ggplot() + geom_point(data=usgs_daily_11303500_filt[2001:3000,], aes(x=Date, y=Wtemp)))

usgs_daily_11303500_QA <- usgs_daily_11303500_filt[1:3000]

ggplotly(
  ggplot() + geom_point(data=usgs_daily_11303500_filt[3001:4000,], aes(x=Date, y=Wtemp)))

usgs_daily_11303500_QA <- usgs_daily_11303500_filt[1:4000,]

ggplotly(
  ggplot() + geom_point(data=usgs_daily_11303500_filt[4001:5000,], aes(x=Date, y=Wtemp)))

usgs_daily_11303500_QA <- usgs_daily_11303500_filt[1:5000,]

ggplotly(
  ggplot() + geom_point(data=usgs_daily_11303500_filt[5001:5804,], aes(x=Date, y=Wtemp)))

usgs_daily_11303500_QA <- usgs_daily_11303500_filt[1:5804,]

# Final review ------------------------------------------------------------

# plot QA'd dataset to confirm all points look good
ggplotly(
  ggplot() +geom_point(data = usgs_daily_11303500_QA, aes(x=Date, y=Wtemp)))

#save QA'd dataset as a .rds file
write_rds(usgs_daily_11303500_QA, path = "data/QA_data/usgs_daily_11303500_QA.rds")

#update the gage_QA_progress
gage_QA_progress <- read_csv("data/data_review/gage_QA_progress.csv")

#note reviewer initials, whether review is complete, and any final notes
gage_QA_progress[gage_QA_progress$site_id=="11303500",4:6] <- c("ADW", "Y", "QA complete")

#save updated dataframe to the .csv
write_csv(gage_QA_progress, path = "data/data_review/gage_QA_progress.csv")
