
# Code description --------------------------------------------------------

# This code imports data from the Shasta River/Big Springs Creek research projects and creates daily data sets that will be incorporated into the statewide analysis. May create separate codes for each site, depending on how much QA is required for each one.


# Libraries ---------------------------------------------------------------

library(tidyverse)
library(lubridate)
library(plotly)


# Import data -------------------------------------------------------------

BSC_raw_2008 <- read_csv("data/Shasta/Shasta_raw/BSC_longitudinal_2008.csv", col_names = c("datetime", "julian_day", "BSC_dam", "Busk_bridge", "Pond_upstream", "Pond_downstream", "Blw_ww", "corral bridge", "LDB", "abv LSC", "lowest_xing", "BSC_mouth"), col_types = cols(BSC_dam = col_double()) ,skip = 2)

BSC_raw_2008 <- BSC_raw_2008 %>% 
  select(datetime,BSC_dam, Blw_ww, LDB, BSC_mouth) %>% 
  mutate(date_time2 = mdy_hm(datetime),
         julian_day = yday(date_time2), month=month(date_time2), month_day=day(date_time2), year=year(date_time2)) 

BSC_raw_2009 <- read_csv("data/Shasta/Shasta_raw/BSC_longitudinal_2009.csv", col_names = c("datetime", "julian_day", "BSC_dam", "Busk_bridge", "Pond_upstream", "Pond_downstream", "Blw_ww", "corral bridge", "LDB", "abv LSC", "lowest_xing", "BSC_mouth"), col_types = cols(BSC_dam = col_double()) ,skip = 2)

BSC_raw_2009 <- BSC_raw_2009 %>% 
  select(datetime,BSC_dam, Blw_ww, LDB, BSC_mouth) %>% 
  mutate(date_time2 = mdy_hm(datetime),
         julian_day = yday(date_time2), month=month(date_time2), month_day=day(date_time2), year=year(date_time2))

BSC_raw_2010 <- read_csv("data/Shasta/Shasta_raw/BSC_longitudinal_2010.csv", col_names = c("datetime", "julian_day", "BSC_dam", "Busk_bridge", "Pond_upstream", "Pond_downstream", "Blw_ww", "corral bridge", "LDB", "abv LSC", "lowest_xing", "BSC_mouth"), col_types = cols(BSC_dam = col_double()) ,skip = 2)

BSC_raw_2010 <- BSC_raw_2010 %>% 
  select(datetime,BSC_dam, Blw_ww, LDB, BSC_mouth) %>% 
  mutate(date_time2 = mdy_hm(datetime),
         julian_day = yday(date_time2), month=month(date_time2), month_day=day(date_time2), year=year(date_time2))

BSC_raw_2011 <- read_csv("data/Shasta/Shasta_raw/BSC_longitudinal_2011.csv", col_names = c("datetime", "julian_day", "BSC_dam", "Busk_bridge", "Pond_upstream", "Pond_downstream", "Blw_ww", "corral bridge", "LDB", "abv LSC", "lowest_xing", "BSC_mouth"), col_types = cols(BSC_dam = col_double()) ,skip = 2)

BSC_raw_2011 <- BSC_raw_2011 %>% 
  select(datetime,BSC_dam, Blw_ww, LDB, BSC_mouth) %>% 
  mutate(date_time2 = mdy_hm(datetime),
         julian_day = yday(date_time2), month=month(date_time2), month_day=day(date_time2), year=year(date_time2))

BSC_raw_2012 <- read_csv("data/Shasta/Shasta_raw/BSC_longitudinal_2012.csv", col_names = c("datetime", "julian_day", "BSC_dam", "Busk_bridge", "Pond_upstream", "Pond_downstream", "Blw_ww", "corral bridge", "LDB", "abv LSC", "lowest_xing", "BSC_mouth"), col_types = cols(BSC_dam = col_double()) ,skip = 2)

BSC_raw_2012 <- BSC_raw_2012 %>% 
  select(datetime,BSC_dam, Blw_ww, LDB, BSC_mouth) %>% 
  mutate(date_time2 = mdy_hm(datetime),
         julian_day = yday(date_time2), month=month(date_time2), month_day=day(date_time2), year=year(date_time2))

BSC_raw_2013 <- read_csv("data/Shasta/Shasta_raw/BSC_longitudinal_2013.csv", col_names = c("datetime", "julian_day", "BSC_dam", "Busk_bridge", "Pond_upstream", "Pond_downstream", "Blw_ww", "corral bridge", "LDB", "abv LSC", "lowest_xing", "BSC_mouth"), col_types = cols(BSC_dam = col_double()) ,skip = 2)

BSC_raw_2013 <- BSC_raw_2013 %>% 
  select(datetime,BSC_dam, Blw_ww, LDB, BSC_mouth) %>% 
  mutate(date_time2 = mdy_hm(datetime),
         julian_day = yday(date_time2), month=month(date_time2), month_day=day(date_time2), year=year(date_time2))

BSC_raw_2014 <- read_csv("data/Shasta/Shasta_raw/BSC_longitudinal_2014.csv", col_names = c("datetime", "julian_day", "BSC_dam", "Blw_ww", "LDB", "BSC_mouth"), col_types = cols(BSC_dam = col_double()) ,skip = 2)

BSC_raw_2014 <- BSC_raw_2014 %>% 
  select(datetime,BSC_dam, Blw_ww, LDB, BSC_mouth) %>% 
  mutate(date_time2 = mdy_hm(datetime),
         julian_day = yday(date_time2), month=month(date_time2), month_day=day(date_time2), year=year(date_time2))

BSC_raw_2015 <- read_csv("data/Shasta/Shasta_raw/BSC_longitudinal_2015.csv", col_names = c("datetime", "julian_day", "BSC_dam", "Blw_ww", "LDB", "BSC_mouth"), col_types = cols(BSC_dam = col_double()) ,skip = 2)

BSC_raw_2015 <- BSC_raw_2015 %>% 
  select(datetime,BSC_dam, Blw_ww, LDB, BSC_mouth) %>% 
  mutate(date_time2 = mdy_hm(datetime),
         julian_day = yday(date_time2), month=month(date_time2), month_day=day(date_time2), year=year(date_time2))

BSC_raw_2016_2019 <- read_csv("data/Shasta/Shasta_raw/BSC_dam_2016_2019.csv", col_names = c("datetime", "temp_C"), skip = 1)

BSC_raw_2016_2019 <- BSC_raw_2016_2019 %>% 
  mutate(date_time2 = mdy_hm(datetime),
         julian_day = yday(date_time2), month=month(date_time2), month_day=day(date_time2), year=year(date_time2))
# Build raw datasets for BSC_dam ----------------------------------------

BSC_dam_2008 <- BSC_raw_2008 %>% 
  select(date_time2, julian_day, month, month_day, year, BSC_dam) %>% 
  filter(!is.na(BSC_dam)) %>% 
  rename("temp_C" = BSC_dam)

BSC_dam_2009 <- BSC_raw_2009 %>% 
  select(date_time2, julian_day, month, month_day, year, BSC_dam) %>% 
  filter(!is.na(BSC_dam)) %>% 
  rename("temp_C" = BSC_dam)

BSC_dam_master <- rbind(BSC_dam_2008,BSC_dam_2009)

BSC_dam_2010 <- BSC_raw_2010 %>% 
  select(date_time2, julian_day, month, month_day, year, BSC_dam) %>% 
  filter(!is.na(BSC_dam)) %>% 
  rename("temp_C" = BSC_dam)

BSC_dam_master <- rbind(BSC_dam_master,BSC_dam_2010)

BSC_dam_2011 <- BSC_raw_2011 %>% 
  select(date_time2, julian_day, month, month_day, year, BSC_dam) %>% 
  filter(!is.na(BSC_dam)) %>% 
  rename("temp_C" = BSC_dam)

BSC_dam_master <- rbind(BSC_dam_master,BSC_dam_2011)

BSC_dam_2012 <- BSC_raw_2012 %>% 
  select(date_time2, julian_day, month, month_day, year, BSC_dam) %>% 
  filter(!is.na(BSC_dam)) %>% 
  rename("temp_C" = BSC_dam)

BSC_dam_master <- rbind(BSC_dam_master,BSC_dam_2012)

BSC_dam_2013 <- BSC_raw_2013 %>% 
  select(date_time2, julian_day, month, month_day, year, BSC_dam) %>% 
  filter(!is.na(BSC_dam)) %>% 
  rename("temp_C" = BSC_dam)

BSC_dam_master <- rbind(BSC_dam_master,BSC_dam_2013)

BSC_dam_2014 <- BSC_raw_2014 %>% 
  select(date_time2, julian_day, month, month_day, year, BSC_dam) %>% 
  filter(!is.na(BSC_dam)) %>% 
  rename("temp_C" = BSC_dam)

BSC_dam_master <- rbind(BSC_dam_master,BSC_dam_2014)

BSC_dam_2015 <- BSC_raw_2015 %>% 
  select(date_time2, julian_day, month, month_day, year, BSC_dam) %>% 
  filter(!is.na(BSC_dam)) %>% 
  rename("temp_C" = BSC_dam)

BSC_dam_master <- rbind(BSC_dam_master,BSC_dam_2015)

BSC_dam_2016_2019 <- BSC_raw_2016_2019 %>% 
  select(date_time2, julian_day, month, month_day, year, temp_C) %>% 
  filter(!is.na(temp_C))

BSC_dam_master <- rbind(BSC_dam_master,BSC_dam_2016_2019)

BSC_dam_master <- BSC_dam_master %>%
  mutate(date = date(date_time2)) %>% 
  select(julian_day, date, month, month_day, year, temp_C)
  
BSC_dam_daily <- BSC_dam_master %>%  
  group_by(date, julian_day) %>% 
  summarize(value_mean_C = mean(temp_C))

# QA daily data for BSC_dam -----------------------------------------------

ggplotly(
  ggplot() + geom_point(data=BSC_dam_daily[,], aes(x=date, y=value_mean_C)))

BSC_dam_daily_QA <- BSC_dam_daily

#write_csv(BSC_dam_daily_QA, path = "data/Shasta/Shasta_QA/BSC_dam_daily_QA.csv")
write_rds(BSC_dam_daily_QA, path = "data/QA_data/shasta_daily_BSC_dam_QA.rds")

# Build raw datasets for BSC_mouth ----------------------------------------

BSC_mouth_2008 <- BSC_raw_2008 %>% 
  select(date_time2, julian_day, month, month_day, year, BSC_mouth) %>% 
  filter(!is.na(BSC_mouth)) %>% 
  rename("temp_C" = BSC_mouth)

BSC_mouth_2009 <- BSC_raw_2009 %>% 
  select(date_time2, julian_day, month, month_day, year, BSC_mouth) %>% 
  filter(!is.na(BSC_mouth)) %>% 
  rename("temp_C" = BSC_mouth)

BSC_mouth_master <- rbind(BSC_mouth_2008,BSC_mouth_2009)

BSC_mouth_2010 <- BSC_raw_2010 %>% 
  select(date_time2, julian_day, month, month_day, year, BSC_mouth) %>% 
  filter(!is.na(BSC_mouth)) %>% 
  rename("temp_C" = BSC_mouth)

BSC_mouth_master <- rbind(BSC_mouth_master,BSC_mouth_2010)

BSC_mouth_2011 <- BSC_raw_2011 %>% 
  select(date_time2, julian_day, month, month_day, year, BSC_mouth) %>% 
  filter(!is.na(BSC_mouth)) %>% 
  rename("temp_C" = BSC_mouth)

BSC_mouth_master <- rbind(BSC_mouth_master,BSC_mouth_2011)

BSC_mouth_2012 <- BSC_raw_2012 %>% 
  select(date_time2, julian_day, month, month_day, year, BSC_mouth) %>% 
  filter(!is.na(BSC_mouth)) %>% 
  rename("temp_C" = BSC_mouth)

BSC_mouth_master <- rbind(BSC_mouth_master,BSC_mouth_2012)

BSC_mouth_2013 <- BSC_raw_2013 %>% 
  select(date_time2, julian_day, month, month_day, year, BSC_mouth) %>% 
  filter(!is.na(BSC_mouth)) %>% 
  rename("temp_C" = BSC_mouth)

BSC_mouth_master <- rbind(BSC_mouth_master,BSC_mouth_2013)

BSC_mouth_2014 <- BSC_raw_2014 %>% 
  select(date_time2, julian_day, month, month_day, year, BSC_mouth) %>% 
  filter(!is.na(BSC_mouth)) %>% 
  rename("temp_C" = BSC_mouth)

BSC_mouth_master <- rbind(BSC_mouth_master,BSC_mouth_2014)

BSC_mouth_2015 <- BSC_raw_2015 %>% 
  select(date_time2, julian_day, month, month_day, year, BSC_mouth) %>% 
  filter(!is.na(BSC_mouth)) %>% 
  rename("temp_C" = BSC_mouth)

BSC_mouth_master <- rbind(BSC_mouth_master,BSC_mouth_2015)

BSC_mouth_2016_2019 <- BSC_raw_2016_2019 %>% 
  select(date_time2, julian_day, month, month_day, year, temp_C) %>% 
  filter(!is.na(temp_C))

BSC_mouth_master <- rbind(BSC_mouth_master,BSC_mouth_2016_2019)

BSC_mouth_master <- BSC_mouth_master %>%
  mutate(date = date(date_time2)) %>% 
  select(julian_day, date, month, month_day, year, temp_C)

BSC_mouth_daily <- BSC_mouth_master %>%  
  group_by(date, julian_day) %>% 
  summarize(value_mean_C = mean(temp_C))

# QA daily data for BSC_mouth ---------------------------------------------

ggplotly(
  ggplot() + geom_point(data=BSC_mouth_daily[,], aes(x=date, y=value_mean_C)))

BSC_mouth_daily_QA <- BSC_mouth_daily

#write_csv(BSC_mouth_daily_QA, path = "data/Shasta/Shasta_QA/BSC_mouth_daily_QA.csv")

write_rds(BSC_mouth_daily_QA, path = "data/QA_data/shasta_daily_BSC_mouth_QA.rds")

