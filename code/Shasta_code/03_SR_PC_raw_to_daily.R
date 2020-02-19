
# Code description --------------------------------------------------------

# This code processes raw data to the daily average record for sites in the Shasta River and Parks Creek, following the same steps as in 01_BSC_raw_to_QA_daily.


# Libraries ---------------------------------------------------------------

library(tidyverse)
library(lubridate)
library(plotly)

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

SRabvPC_daily <- SRabvPC_master %>% 
  group_by(date, julian_day) %>% 
  summarize(value_mean_C = mean(temp_C))

ggplotly(
  ggplot() + geom_point(data=SRabvPC_daily[,], aes(x=date, y=value_mean_C)))

SRabvPC_daily_QA <- SRabvPC_daily %>% 
  filter(date != "2017-05-23", date != "2017-05-24")

SRabvPC_daily_QA <- SRabvPC_daily_QA[-c(2691:2905),]

ggplotly(
  ggplot() + geom_point(data=SRabvPC_daily_QA[,], aes(x=date, y=value_mean_C)))

write_csv(SRabvPC_daily_QA, path = "data/Shasta/Shasta_QA/SRabvPC_daily_QA.csv")
