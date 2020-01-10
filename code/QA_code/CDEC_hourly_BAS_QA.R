
# Code description --------------------------------------------------------

# Code to review temperature data from gage CDEC_BAS.

library(tidyverse)
library(lubridate)
library(weathermetrics)
library(plotly)

# Get Data ----------------------------------------------------------------

file_list <- list.files("data/data_review/")

# read in next file:
file_list[[9]]

cdec_hourly_BAS <- read_rds(path = paste0("data/data_review/",file_list[[9]]))

# Plot --------------------------------------------------------------------

# now make an interactive plot of first 1000 values
ggplotly(
  ggplot() + geom_point(data=cdec_hourly_BAS[1:20000,], aes(x=datetime, y=value)))


# Review, QA, and Repeat --------------------------------------------------

cdec_hourly_BAS_1_20000 <- cdec_hourly_BAS[1:20000,] %>% 
  filter(value > 20)

ggplotly(
  ggplot() + geom_point(data=cdec_hourly_BAS_1_20000, aes(x=datetime, y=value)))

cdec_hourly_BAS_QA <- cdec_hourly_BAS_1_20000

ggplotly(
  ggplot() + geom_point(data=cdec_hourly_BAS[20001:40000,], aes(x=datetime, y=value)))

cdec_hourly_BAS_QA <- rbind(cdec_hourly_BAS_QA, cdec_hourly_BAS[20001:40000,])

ggplotly(
  ggplot() + geom_point(data=cdec_hourly_BAS[40001:60000,], aes(x=datetime, y=value)))

cdec_hourly_BAS_40001_60000 <- cdec_hourly_BAS[40001:60000,] %>% 
  filter(value > 5.0)

ggplotly(
  ggplot() + geom_point(data=cdec_hourly_BAS_40001_60000, aes(x=datetime, y=value)))

cdec_hourly_BAS_QA <- rbind(cdec_hourly_BAS_QA, cdec_hourly_BAS_40001_60000)

ggplotly(
  ggplot() + geom_point(data=cdec_hourly_BAS[60001:80000,], aes(x=datetime, y=value)))

cdec_hourly_BAS_QA <- rbind(cdec_hourly_BAS_QA, cdec_hourly_BAS[60001:80000,])

ggplotly(
  ggplot() + geom_point(data=cdec_hourly_BAS[80001:100000,], aes(x=datetime, y=value)))

cdec_hourly_BAS_QA <- rbind(cdec_hourly_BAS_QA,cdec_hourly_BAS[80001:100000,])

ggplotly(
  ggplot() + geom_point(data=cdec_hourly_BAS[100001:120000,], aes(x=datetime, y=value)))

cdec_hourly_BAS_100001_120000 <- cdec_hourly_BAS[100001:120000,] %>% 
  filter(value > 5.0)

ggplotly(
  ggplot() + geom_point(data=cdec_hourly_BAS_100001_120000, aes(x=datetime, y=value)))

cdec_hourly_BAS_QA <- rbind(cdec_hourly_BAS_QA, cdec_hourly_BAS_100001_120000)

ggplotly(
  ggplot() + geom_point(data=cdec_hourly_BAS[120001:140000,], aes(x=datetime, y=value)))

cdec_hourly_BAS_120001_140000 <- cdec_hourly_BAS[120001:140000,] %>% 
  filter(value > 5.0)

ggplotly(
  ggplot() + geom_point(data=cdec_hourly_BAS_120001_140000, aes(x=datetime, y=value)))

cdec_hourly_BAS_QA <- rbind(cdec_hourly_BAS_QA, cdec_hourly_BAS_120001_140000)

ggplotly(
  ggplot() + geom_point(data=cdec_hourly_BAS[140001:160000,], aes(x=datetime, y=value)))

cdec_hourly_BAS_QA <- rbind(cdec_hourly_BAS_QA, cdec_hourly_BAS[140001:160000,])

ggplotly(
  ggplot() + geom_point(data=cdec_hourly_BAS[160001:180000,], aes(x=datetime, y=value)))

cdec_hourly_BAS_QA <- rbind(cdec_hourly_BAS_QA, cdec_hourly_BAS[160001:180000,])

ggplotly(
  ggplot() + geom_point(data=cdec_hourly_BAS[180001:200000,], aes(x=datetime, y=value)))

cdec_hourly_BAS_180001_200000 <- cdec_hourly_BAS[180001:200000,] %>% 
  filter(value > 5.0)

ggplotly(
  ggplot() + geom_point(data=cdec_hourly_BAS_180001_200000, aes(x=datetime, y=value)))

cdec_hourly_BAS_QA <- rbind(cdec_hourly_BAS_QA, cdec_hourly_BAS_180001_200000)

ggplotly(
  ggplot() + geom_point(data=cdec_hourly_BAS[200001:220000,], aes(x=datetime, y=value)))

cdec_hourly_BAS_QA <- rbind(cdec_hourly_BAS_QA, cdec_hourly_BAS[200001:220000,])

ggplotly(
  ggplot() + geom_point(data=cdec_hourly_BAS[220001:240000,], aes(x=datetime, y=value)))

cdec_hourly_BAS_QA <- rbind(cdec_hourly_BAS_QA, cdec_hourly_BAS[220001:240000,])

ggplotly(
  ggplot() + geom_point(data=cdec_hourly_BAS[240001:260000,], aes(x=datetime, y=value)))

cdec_hourly_BAS_QA <- rbind(cdec_hourly_BAS_QA, cdec_hourly_BAS[240001:260000,])

ggplotly(
  ggplot() + geom_point(data=cdec_hourly_BAS[260001:280000,], aes(x=datetime, y=value)))

cdec_hourly_BAS_QA <- rbind(cdec_hourly_BAS_QA, cdec_hourly_BAS[260001:280000,])

ggplotly(
  ggplot() + geom_point(data=cdec_hourly_BAS[280001:304323,], aes(x=datetime, y=value)))

cdec_hourly_BAS_QA <- rbind(cdec_hourly_BAS_QA, cdec_hourly_BAS[280001:304323,])



# Final review ------------------------------------------------------------

#plot QA'd dataset to confirm all points look good
ggplotly(
  ggplot() +geom_point(data = cdec_hourly_BAS_QA, aes(x=datetime, y=value)))

#save QA'd dataset as a .rds file
write_rds(cdec_hourly_BAS_QA, path = "data/QA_data/cdec_hourly_BAS_QA.rds", compress = "gz")

#update the gage_QA_progress
gage_QA_progress <- read_csv("data/data_review/gage_QA_progress.csv")

#confirm correct row to update by the site_id
gage_QA_progress[3,1]

#note reviewer initials, whether review is complete, and any final notes
gage_QA_progress[3,4:6] <- c("ADW", "Y", "QA complete")

#save updated dataframe to the .csv
write_csv(gage_QA_progress, path = "data/data_review/gage_QA_progress.csv")
