
# Code description --------------------------------------------------------

# This code takes the sub-hourly data from the north alcove spring in Big Springs Creek and transforms it into the daily recorded needed to create the compiled modeling dataset.


# Libraries ---------------------------------------------------------------

library(tidyverse)
library(plotly)
library(lubridate)

# Import data -------------------------------------------------------------

north_alcove_spring_raw <- read_csv("data/Shasta/Shasta_raw/BSC_spring.csv", col_names = c("datetime", "deg_C"))

gage_QA_progress <- read_csv("data/data_review/gage_QA_progress.csv")


# Convert to daily data ---------------------------------------------------
north_alcove_spring_raw <- north_alcove_spring_raw %>% 
  mutate(date_time2 = mdy_hm(datetime),
         julian_day = yday(date_time2), date = date(date_time2), month=month(date_time2),
         month_day=day(date_time2), year=year(date_time2)) %>% 
  filter(!is.na(deg_C))

north_alcove_spring_raw_daily <- north_alcove_spring_raw %>% 
  group_by(date, julian_day) %>% 
  summarize(value_mean_C = mean(deg_C))

# Visualize ---------------------------------------------------------------

ggplotly(
  ggplot() + geom_line(data=north_alcove_spring_raw_daily[,], aes(x=date, y=value_mean_C))
  )

# Save file ---------------------------------------------------------------

BSC_spring_daily_QA <- north_alcove_spring_raw_daily %>% 
  mutate(station_id = "BSC_spring")

write_rds(BSC_spring_daily_QA, path = "data/QA_data/Shasta_BSC_spring_daily_QA.rds")

#update gage_QA_progress table
#note reviewer initials, whether review is complete, and any final notes
gage_QA_progress[gage_QA_progress$site_id=="BSC_spring",6:8] <- c("ADW", "Y", "QA complete")

write_csv(gage_QA_progress, path = "data/data_review/gage_QA_progress.csv")
