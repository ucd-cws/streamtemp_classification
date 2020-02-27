
# Code description --------------------------------------------------------

# This code processes raw data to the daily average record for sites in the Shasta River and Parks Creek, following the same steps as in 01_BSC_raw_to_QA_daily.


# Libraries ---------------------------------------------------------------

library(tidyverse)
library(lubridate)
library(plotly)

# Import gage_progress_QA table -------------------------------------------

gage_QA_progress <- read_csv("data/data_review/gage_QA_progress.csv")

# Import data -------------------------------------------------------------

SR_PC_raw_2008 <- read_csv("data/Shasta/Shasta_raw/Upper_Shasta_and_Parks_2008.csv", col_names = c("datetime", "julian_day", "SRabvPC", "SRabvBSC", "PC_mouth"), col_types = cols(SRabvBSC = col_double(), PC_mouth = col_double()), skip = 1)

SR_PC_raw_2008 <- SR_PC_raw_2008 %>% 
  select(datetime, SRabvPC, SRabvBSC,PC_mouth) %>%
  mutate(date_time2 = mdy_hm(datetime),
         julian_day = yday(date_time2), month=month(date_time2), month_day=day(date_time2), year=year(date_time2))
  
SR_PC_raw_2009 <- read_csv("data/Shasta/Shasta_raw/Upper_Shasta_and_Parks_2009.csv", col_names = c("datetime", "julian_day", "SR_S_bound", "SRabvPC", "SRabvBSC", "PC_mouth"), col_types = cols(SR_S_bound = col_double(), SRabvBSC = col_double(), PC_mouth = col_double()), skip = 1)

SR_PC_raw_2009 <- SR_PC_raw_2009 %>% 
  select(datetime, SRabvPC, SRabvBSC,PC_mouth) %>%
  mutate(date_time2 = mdy_hm(datetime),
         julian_day = yday(date_time2), month=month(date_time2), month_day=day(date_time2), year=year(date_time2))

SR_PC_raw_2010 <- read_csv("data/Shasta/Shasta_raw/Upper_Shasta_and_Parks_2010.csv", col_names = c("datetime", "julian_day", "SR_S_bound", "SRabvPC", "SRabvBSC", "PC_mouth"), col_types = cols(SR_S_bound = col_double(), SRabvBSC = col_double(), PC_mouth = col_double()), skip = 1)

SR_PC_raw_2010 <- SR_PC_raw_2010 %>% 
  select(datetime, SRabvPC, SRabvBSC,PC_mouth) %>%
  mutate(date_time2 = mdy_hm(datetime),
         julian_day = yday(date_time2), month=month(date_time2), month_day=day(date_time2), year=year(date_time2))

SR_PC_raw_2011 <- read_csv("data/Shasta/Shasta_raw/Upper_Shasta_and_Parks_2011.csv", col_names = c("datetime", "julian_day", "SR_S_bound", "SRabvPC", "SRabvBSC", "PC_mouth"), col_types = cols(SR_S_bound = col_double(), SRabvBSC = col_double(), PC_mouth = col_double()), skip = 1)

SR_PC_raw_2011 <- SR_PC_raw_2011 %>% 
  select(datetime, SRabvPC, SRabvBSC,PC_mouth) %>%
  mutate(date_time2 = mdy_hm(datetime),
         julian_day = yday(date_time2), month=month(date_time2), month_day=day(date_time2), year=year(date_time2))

SR_PC_raw_2012 <- read_csv("data/Shasta/Shasta_raw/Upper_Shasta_and_Parks_2012.csv", col_names = c("datetime", "julian_day", "SRabvPC", "SRabvBSC", "PC_mouth"), col_types = cols(SRabvBSC = col_double(), PC_mouth = col_double()), skip = 1)

SR_PC_raw_2012 <- SR_PC_raw_2012 %>% 
  select(datetime, SRabvPC, SRabvBSC,PC_mouth) %>%
  mutate(date_time2 = mdy_hm(datetime),
         julian_day = yday(date_time2), month=month(date_time2), month_day=day(date_time2), year=year(date_time2))

SR_PC_raw_2013 <- read_csv("data/Shasta/Shasta_raw/Upper_Shasta_and_Parks_2013.csv", col_names = c("datetime", "julian_day", "SRabvPC", "SRabvBSC", "PC_mouth"), col_types = cols(SRabvBSC = col_double(), PC_mouth = col_double()), skip = 1)

SR_PC_raw_2013 <- SR_PC_raw_2013 %>% 
  select(datetime, SRabvPC, SRabvBSC,PC_mouth) %>%
  mutate(date_time2 = mdy_hm(datetime),
         julian_day = yday(date_time2), month=month(date_time2), month_day=day(date_time2), year=year(date_time2))

SR_PC_raw_2014 <- read_csv("data/Shasta/Shasta_raw/Upper_Shasta_and_Parks_2014.csv", col_names = c("datetime", "julian_day", "SRabvPC", "SRabvBSC", "PC_mouth"), col_types = cols(SRabvBSC = col_double(), PC_mouth = col_double()), skip = 1)

SR_PC_raw_2014 <- SR_PC_raw_2014 %>% 
  select(datetime, SRabvPC, SRabvBSC,PC_mouth) %>%
  mutate(date_time2 = mdy_hm(datetime),
         julian_day = yday(date_time2), month=month(date_time2), month_day=day(date_time2), year=year(date_time2))

SR_PC_raw_2015 <- read_csv("data/Shasta/Shasta_raw/Upper_Shasta_and_Parks_2015.csv", col_names = c("datetime", "julian_day", "SRabvPC", "SRabvBSC", "PC_mouth"), col_types = cols(SRabvBSC = col_double(), PC_mouth = col_double()), skip = 1)

SR_PC_raw_2015 <- SR_PC_raw_2015 %>% 
  select(datetime, SRabvPC, SRabvBSC,PC_mouth) %>%
  mutate(date_time2 = mdy_hm(datetime),
         julian_day = yday(date_time2), month=month(date_time2), month_day=day(date_time2), year=year(date_time2))

SRabvPC_raw_2016_2019 <- read_csv("data/Shasta/Shasta_raw/SR_abv_PC_2016_2019.csv", col_names = c("datetime", "temp_C"), skip = 1)

SRabvPC_raw_2016_2019 <- SRabvPC_raw_2016_2019 %>% 
  mutate(date_time2 = mdy_hm(datetime),
         julian_day = yday(date_time2), month=month(date_time2), month_day=day(date_time2), year=year(date_time2))

SRabvBSC_raw_2016_2019 <- read_csv("data/Shasta/Shasta_raw/SR_abv_BSC_2016_2019.csv", col_names = c("datetime", "temp_C"), skip = 1)

SRabvBSC_raw_2016_2019 <- SRabvBSC_raw_2016_2019 %>% 
  mutate(date_time2 = mdy_hm(datetime),
         julian_day = yday(date_time2), month=month(date_time2), month_day=day(date_time2), year=year(date_time2))

PCmouth_raw_2016_2019 <- read_csv("data/Shasta/Shasta_raw/PC_mouth_2016_2019.csv", col_names = c("datetime", "temp_C"), skip = 1)

PCmouth_raw_2016_2019 <- PCmouth_raw_2016_2019 %>% 
  mutate(date_time2 = mdy_hm(datetime),
         julian_day = yday(date_time2), month=month(date_time2), month_day=day(date_time2), year=year(date_time2))

# Build raw datasets for SRabvPC ------------------------------------------

SRabvPC_2008 <- SR_PC_raw_2008 %>% 
  select(date_time2, julian_day, month, month_day, year, SRabvPC) %>% 
  filter(!is.na(SRabvPC)) %>% 
  rename("temp_C" = SRabvPC)

SRabvPC_2009 <- SR_PC_raw_2009 %>% 
  select(date_time2, julian_day, month, month_day, year, SRabvPC) %>% 
  filter(!is.na(SRabvPC)) %>% 
  rename("temp_C" = SRabvPC)

SRabvPC_master <- rbind(SRabvPC_2008, SRabvPC_2009)

SRabvPC_2010 <- SR_PC_raw_2010 %>% 
  select(date_time2, julian_day, month, month_day, year, SRabvPC) %>% 
  filter(!is.na(SRabvPC)) %>% 
  rename("temp_C" = SRabvPC)

SRabvPC_master <- rbind(SRabvPC_master, SRabvPC_2010)

SRabvPC_2011 <- SR_PC_raw_2011 %>% 
  select(date_time2, julian_day, month, month_day, year, SRabvPC) %>% 
  filter(!is.na(SRabvPC)) %>% 
  rename("temp_C" = SRabvPC)

SRabvPC_master <- rbind(SRabvPC_master, SRabvPC_2011)

SRabvPC_2012 <- SR_PC_raw_2012 %>% 
  select(date_time2, julian_day, month, month_day, year, SRabvPC) %>% 
  filter(!is.na(SRabvPC)) %>% 
  rename("temp_C" = SRabvPC)

SRabvPC_master <- rbind(SRabvPC_master, SRabvPC_2012)

SRabvPC_2013 <- SR_PC_raw_2013 %>% 
  select(date_time2, julian_day, month, month_day, year, SRabvPC) %>% 
  filter(!is.na(SRabvPC)) %>% 
  rename("temp_C" = SRabvPC)

SRabvPC_master <- rbind(SRabvPC_master, SRabvPC_2013)

SRabvPC_2014 <- SR_PC_raw_2014 %>% 
  select(date_time2, julian_day, month, month_day, year, SRabvPC) %>% 
  filter(!is.na(SRabvPC)) %>% 
  rename("temp_C" = SRabvPC)

SRabvPC_master <- rbind(SRabvPC_master, SRabvPC_2014)

SRabvPC_2015 <- SR_PC_raw_2015 %>% 
  select(date_time2, julian_day, month, month_day, year, SRabvPC) %>% 
  filter(!is.na(SRabvPC)) %>% 
  rename("temp_C" = SRabvPC)

SRabvPC_master <- rbind(SRabvPC_master, SRabvPC_2015)

SRabvPC_2016_2019 <- SRabvPC_raw_2016_2019 %>% 
  select(date_time2, julian_day, month, month_day, year, temp_C) %>% 
  filter(!is.na(temp_C))

SRabvPC_master <- rbind(SRabvPC_master, SRabvPC_2016_2019)

SRabvPC_master <- SRabvPC_master %>% 
  mutate(date = date(date_time2)) %>% 
  select(julian_day, date, month, month_day, year, temp_C)


# Create daily, plot, and QA SRabvPC --------------------------------------

SRabvPC_daily <- SRabvPC_master %>% 
  group_by(date, julian_day) %>% 
  summarize(value_mean_C = mean(temp_C)) %>% 
  mutate(station_id = "SRabvPC")

ggplotly(
  ggplot() + geom_point(data=SRabvPC_daily[,], aes(x=date, y=value_mean_C)))

SRabvPC_daily_QA <- SRabvPC_daily %>% 
  filter(date != "2017-05-23", date != "2017-05-24")

SRabvPC_daily_QA <- SRabvPC_daily_QA[-c(2691:2905),]

ggplotly(
  ggplot() + geom_point(data=SRabvPC_daily_QA[,], aes(x=date, y=value_mean_C)))

write_rds(SRabvPC_daily_QA, path = "data/QA_data/Shasta_SRabvPC_daily_QA.rds")

#update gage_QA_progress table
#note reviewer initials, whether review is complete, and any final notes
gage_QA_progress[gage_QA_progress$site_id=="SR_abv_Parks",6:8] <- c("ADW", "Y", "QA complete")


# Build raw dataset for SRabvBSC ------------------------------------------

SRabvBSC_2008 <- SR_PC_raw_2008 %>% 
  select(date_time2, julian_day, month, month_day, year, SRabvBSC) %>% 
  filter(!is.na(SRabvBSC)) %>% 
  rename("temp_C" = SRabvBSC)

SRabvBSC_2009 <- SR_PC_raw_2009 %>% 
  select(date_time2, julian_day, month, month_day, year, SRabvBSC) %>% 
  filter(!is.na(SRabvBSC)) %>% 
  rename("temp_C" = SRabvBSC)

SRabvBSC_master <- rbind(SRabvBSC_2008, SRabvBSC_2009)

SRabvBSC_2010 <- SR_PC_raw_2010 %>% 
  select(date_time2, julian_day, month, month_day, year, SRabvBSC) %>% 
  filter(!is.na(SRabvBSC)) %>% 
  rename("temp_C" = SRabvBSC)

SRabvBSC_master <- rbind(SRabvBSC_master, SRabvBSC_2010)

SRabvBSC_2011 <- SR_PC_raw_2011 %>% 
  select(date_time2, julian_day, month, month_day, year, SRabvBSC) %>% 
  filter(!is.na(SRabvBSC)) %>% 
  rename("temp_C" = SRabvBSC)

SRabvBSC_master <- rbind(SRabvBSC_master, SRabvBSC_2011)

SRabvBSC_2012 <- SR_PC_raw_2012 %>% 
  select(date_time2, julian_day, month, month_day, year, SRabvBSC) %>% 
  filter(!is.na(SRabvBSC)) %>% 
  rename("temp_C" = SRabvBSC)

SRabvBSC_master <- rbind(SRabvBSC_master, SRabvBSC_2012)

SRabvBSC_2013 <- SR_PC_raw_2013 %>% 
  select(date_time2, julian_day, month, month_day, year, SRabvBSC) %>% 
  filter(!is.na(SRabvBSC)) %>% 
  rename("temp_C" = SRabvBSC)

SRabvBSC_master <- rbind(SRabvBSC_master, SRabvBSC_2013)

SRabvBSC_2014 <- SR_PC_raw_2014 %>% 
  select(date_time2, julian_day, month, month_day, year, SRabvBSC) %>% 
  filter(!is.na(SRabvBSC)) %>% 
  rename("temp_C" = SRabvBSC)

SRabvBSC_master <- rbind(SRabvBSC_master, SRabvBSC_2014)

SRabvBSC_2015 <- SR_PC_raw_2015 %>% 
  select(date_time2, julian_day, month, month_day, year, SRabvBSC) %>% 
  filter(!is.na(SRabvBSC)) %>% 
  rename("temp_C" = SRabvBSC)

SRabvBSC_master <- rbind(SRabvBSC_master, SRabvBSC_2015)

SRabvBSC_2016_2019 <- SRabvBSC_raw_2016_2019 %>% 
  select(date_time2, julian_day, month, month_day, year, temp_C) %>% 
  filter(!is.na(temp_C))

SRabvBSC_master <- rbind(SRabvBSC_master, SRabvBSC_2016_2019)

SRabvBSC_master <- SRabvBSC_master %>% 
  mutate(date = date(date_time2)) %>% 
  select(julian_day, date, month, month_day, year, temp_C)

# Create daily, plot, and QA SRabvBSC --------------------------------------

SRabvBSC_daily <- SRabvBSC_master %>% 
  group_by(date, julian_day) %>% 
  summarize(value_mean_C = mean(temp_C))

ggplotly(
  ggplot() + geom_point(data=SRabvBSC_daily[,], aes(x=date, y=value_mean_C)))

SRabvBSC_daily_QA <- SRabvBSC_daily %>% 
  mutate(station_id = "SRabvBSC")

ggplotly(
  ggplot() + geom_point(data=SRabvBSC_daily_QA[,], aes(x=date, y=value_mean_C)))

write_rds(SRabvBSC_daily_QA, path = "data/QA_data/Shasta_SRabvBSC_daily_QA.rds")

#update gage_QA_progress table
#note reviewer initials, whether review is complete, and any final notes
gage_QA_progress[gage_QA_progress$site_id=="SR_abv_BSC",6:8] <- c("ADW", "Y", "QA complete")

# Build raw dataset for PC_mouth ------------------------------------------

PC_mouth_2008 <- SR_PC_raw_2008 %>% 
  select(date_time2, julian_day, month, month_day, year, PC_mouth) %>% 
  filter(!is.na(PC_mouth)) %>% 
  rename("temp_C" = PC_mouth)

PC_mouth_2009 <- SR_PC_raw_2009 %>% 
  select(date_time2, julian_day, month, month_day, year, PC_mouth) %>% 
  filter(!is.na(PC_mouth)) %>% 
  rename("temp_C" = PC_mouth)

PC_mouth_master <- rbind(PC_mouth_2008, PC_mouth_2009)

PC_mouth_2010 <- SR_PC_raw_2010 %>% 
  select(date_time2, julian_day, month, month_day, year, PC_mouth) %>% 
  filter(!is.na(PC_mouth)) %>% 
  rename("temp_C" = PC_mouth)

PC_mouth_master <- rbind(PC_mouth_master, PC_mouth_2010)

PC_mouth_2011 <- SR_PC_raw_2011 %>% 
  select(date_time2, julian_day, month, month_day, year, PC_mouth) %>% 
  filter(!is.na(PC_mouth)) %>% 
  rename("temp_C" = PC_mouth)

PC_mouth_master <- rbind(PC_mouth_master, PC_mouth_2011)

PC_mouth_2012 <- SR_PC_raw_2012 %>% 
  select(date_time2, julian_day, month, month_day, year, PC_mouth) %>% 
  filter(!is.na(PC_mouth)) %>% 
  rename("temp_C" = PC_mouth)

PC_mouth_master <- rbind(PC_mouth_master, PC_mouth_2012)

PC_mouth_2013 <- SR_PC_raw_2013 %>% 
  select(date_time2, julian_day, month, month_day, year, PC_mouth) %>% 
  filter(!is.na(PC_mouth)) %>% 
  rename("temp_C" = PC_mouth)

PC_mouth_master <- rbind(PC_mouth_master, PC_mouth_2013)

PC_mouth_2014 <- SR_PC_raw_2014 %>% 
  select(date_time2, julian_day, month, month_day, year, PC_mouth) %>% 
  filter(!is.na(PC_mouth)) %>% 
  rename("temp_C" = PC_mouth)

PC_mouth_master <- rbind(PC_mouth_master, PC_mouth_2014)

PC_mouth_2015 <- SR_PC_raw_2015 %>% 
  select(date_time2, julian_day, month, month_day, year, PC_mouth) %>% 
  filter(!is.na(PC_mouth)) %>% 
  rename("temp_C" = PC_mouth)

PC_mouth_master <- rbind(PC_mouth_master, PC_mouth_2015)

PC_mouth_2016_2019 <- PCmouth_raw_2016_2019 %>% 
  select(date_time2, julian_day, month, month_day, year, temp_C) %>% 
  filter(!is.na(temp_C))

PC_mouth_master <- rbind(PC_mouth_master, PC_mouth_2016_2019)

PC_mouth_master <- PC_mouth_master %>% 
  mutate(date = date(date_time2)) %>% 
  select(julian_day, date, month, month_day, year, temp_C)

# Create daily, plot, and QA PC_mouth --------------------------------------

PC_mouth_daily <- PC_mouth_master %>% 
  group_by(date, julian_day) %>% 
  summarize(value_mean_C = mean(temp_C))

ggplotly(
  ggplot() + geom_point(data=PC_mouth_daily[,], aes(x=date, y=value_mean_C)))

PC_mouth_daily_QA <- PC_mouth_daily %>% 
  mutate(station_id = "PC_mouth")

ggplotly(
  ggplot() + geom_point(data=PC_mouth_daily_QA[,], aes(x=date, y=value_mean_C)))

write_rds(PC_mouth_daily_QA, path = "data/QA_data/Shasta_PC_mouth_daily_QA.rds")

#update gage_QA_progress table
#note reviewer initials, whether review is complete, and any final notes
gage_QA_progress[gage_QA_progress$site_id=="PC_mouth",6:8] <- c("ADW", "Y", "QA complete")

# Write updated gage_QA_progress table to file ----------------------------

write_csv(gage_QA_progress, path = "data/data_review/gage_QA_progress.csv")

