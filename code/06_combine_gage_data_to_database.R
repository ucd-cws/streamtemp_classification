## SUMMARY: Combine USGS & CDEC data and save to database 
## 2019-Dec
## DETAILS: 
# Cleaned USGS and CDEC gages will be saved to a SQLITE/GEOPACKAGE database for combining later.

# LIBRARIES ---------------------------------------------------------------

library(sf)
library(tidyverse)
library(mapview)
library(purrr)
library(lubridate)
library(tidylog)

# LOAD DATA -----------------------------------------------------------

# load data
load("data/all_gages.rda")
all_gages %>% st_drop_geometry() -> all_gages

# cdec data
load("data/cdec_stations_completed.rda") # station list
cdec_stations <- station_list; rm(station_list)
load("data/cdec_stations_metadata_dateranges_112_stations.rda")
load("data/cdec_temps_all_daily_filt8yr.rda")
load("data/cdec_temps_all_hourly_filt8yr.rda")
load("data/cdec_temps_all_event_filt8yr.rda")

# usgs data
load("data/usgs_stations_completed.rda")
usgs_stations <- station_list_usgs
load("data/usgs_stations_metadata_dateranges_filt8.rda")
load("data/usgs_temps_all_daily_filt8yr.rda")
load("data/usgs_temps_all_iv_filt8yr.rda")

nrow(usgs_temps_day) + nrow(usgs_temps_iv)
# 2.2 million rows of usgs data
nrow(cdec_temps_day) + nrow(cdec_temps_hr) + nrow(cdec_temps_min) 
# 11.5 million rows of CDEC data

# 13.7 million rows of data total!


# Load Functions ----------------------------------------------------------

# load function for converting C to F and F to C
# conversion F to C function
# convert temperatures C-F and F-C
convertTemp <- function (x, unit = c("K", "C", "F"), convert=c("K","C","F")){
  if (!is.numeric(x))
    stop("x must be numeric")
  unit <- match.arg(unit)
  const <- c(K = 0, C = -273.15, F = -459.67)
  a <- c(K = 1, C = 1, F = 5/9)
  units <- names(const)
  u <- match(unit, units)
  TK <- (x - const[u]) * a[u]
  convert <- convert
  ret <- data.frame(K = TK, C = TK - 273.15, F = TK * 1.8 - 459.67)
  row.names(ret) <- NULL
  df<-ret[,which(names(ret) %in% convert)]
  df
}

# DOUBLE CHECK WITH MAP ------------------------------------------------------

# look at cdec metadata and filter to >8 yrs only
cdec_metadata_filt8 <- cdec_info %>% 
  filter(yr_total>8) %>% left_join(., all_gages[,c("site_id","site_name","lat","lon","HR_CODE","HR_NAME")], by=c("site_id")) %>% 
  st_as_sf(coords=c("lon","lat"), remove=FALSE, crs=4326)
# 78 total records
length(unique(cdec_metadata_filt8$site_id)) # 65 total stations
# some duplicates based on same station with Event and Daily

# look at usgs metadata and make spatial
usgs_metadata_filt8 <- usgs_metadata_filt8 %>% 
  st_as_sf(coords=c("dec_long_va","dec_lat_va"), remove=FALSE, crs=4326)

# read in hydroregions:
dwr <- st_read("data/DWR_HydrologicRegions-utm11.shp") %>% st_transform(4326)
# 
# mapview(dwr, color="blue", col.regions=NA) + mapview(cdec_metadata_filt8, col.regions="orange") + mapview(usgs_metadata_filt8, col.regions="maroon")

# CLEAN UP DAILY CDEC DATA -------------------------------------------------------

# DAILY: add C to values and select cols
cdec_temps_day <- cdec_temps_day %>% 
  filter(flag=="N") %>% 
  mutate(value_mean_C = convertTemp(value, unit = "F", convert = "C"),
         month_day = mday(datetime),
         datetime = as.Date(datetime)) %>% 
  rename(value_mean = value) %>% 
  select(station_id:sensor_type, year, month, water_year, water_day, month_day, value_mean, datetime, value_mean_C)

# plot by facet
ggplot() +
  geom_point(data=cdec_temps_day,
             aes(x=datetime, y=value_mean_C, color=station_id), size=1.5, alpha=.7,show.legend = F)+
  ylim(c(0,40)) +
  #facet_grid(station_id~.) +
  scale_color_viridis_d()+
  labs(y="Temp (C)", x="") +
  theme_bw()+
  #ggdark::dark_theme_classic() +
  theme(axis.text.x = element_text(angle=90, hjust = 1))

# CLEAN UP HOURLY CDEC DATA -----------------------------------------------

# HOURLY: make hourly data into daily means
cdec_temps_day2 <- cdec_temps_hr %>% 
  # filter out bad data
  filter(flag=="N") %>% 
  mutate(month_day = mday(datetime)) %>% 
  group_by(station_id, dur_code, sensor_num, sensor_type, year, month, water_year, water_day, month_day) %>% 
  summarize(value_mean = mean(value, na.rm=T)) %>% as.data.frame() %>%
  #add a datetime col back in
  mutate(datetime = mdy(paste0(as.integer(month), "-", month_day,"-", year)),
         value_mean_C = convertTemp(value_mean, unit = "F", convert = "C"))

# how many unique stations (n=39)
cdec_temps_day2 %>% distinct(station_id) %>% tally()

# plot all/single station
ggplot() +
  geom_point(data=cdec_temps_day2, #%>% filter(station_id=="YRS"),
             aes(x=datetime, y=value_mean_C, color=station_id), show.legend = T)+
  #ylim(c(0,90)) +
  scale_color_viridis_d()+
  labs(y="Temp (C)", x="") +
  #ggdark::dark_theme_classic() +
  theme(axis.text.x = element_text(angle=90, hjust = 1))


# CLEAN UP EVENT CDEC DATA ------------------------------------------------

# make event into daily:
cdec_temps_day3 <- cdec_temps_min %>% 
  # filter out bad data
  filter(flag=="N") %>% 
  mutate(month_day = mday(datetime)) %>% 
  group_by(station_id, dur_code, sensor_num, sensor_type, year, month, water_year, water_day, month_day) %>% 
  summarize(value_mean = mean(value, na.rm=T)) %>% as.data.frame() %>% 
  mutate(datetime = mdy(paste0(as.integer(month), "-", month_day,"-", year)),
         value_mean_C = convertTemp(value_mean, unit = "F", convert = "C"))

# how many unique stations (n=18)
cdec_temps_day3 %>% distinct(station_id) %>% tally()

# plot
plotly::ggplotly(
ggplot() +
  geom_line(data=cdec_temps_day3,
            aes(x=datetime, y=value_mean, color=station_id), show.legend = F)+
  scale_x_date(date_breaks = "2 years", date_labels = "%Y")+
  scale_color_viridis_d()+
  labs(y="Temp (F)", x="") +
  ggdark::dark_theme_classic() +
  theme(axis.text.x = element_text(angle=90, hjust = 1)))


# looks like one station is in C already

# fix the one station TTC
cdec_temps_day3_TTC <- cdec_temps_day3 %>% 
  # filter out bad data
  filter(station_id=="TTC") %>% 
  mutate(value_mean_C = value_mean) %>% 
  mutate(value_mean = convertTemp(value_mean_C, unit = "C", convert = "F"))

# replot
ggplot() +
    geom_line(data=cdec_temps_day3_TTC,
              aes(x=datetime, y=value_mean, color=station_id), show.legend = F)+
    scale_x_date(date_breaks = "2 years", date_labels = "%Y")+
    scale_color_viridis_d()+ labs(x="") + theme_bw() +
    theme(axis.text.x = element_text(angle=90, hjust = 1))

# rebind with original dataset
cdec_temps_day3_rev <- cdec_temps_day3 %>% 
  filter(!station_id=="TTC") %>% 
  # now re-add the fixed TTC data
  bind_rows(cdec_temps_day3_TTC)
  
# plot again
ggplot() +
  geom_line(data=cdec_temps_day3_rev,
            aes(x=datetime, y=value_mean_C, color=station_id), show.legend = F)+
  scale_x_date(date_breaks = "2 years", date_labels = "%Y")+
  scale_color_viridis_d()+
  labs(y="Temp (C)", x="") +
  ggdark::dark_theme_classic() +
  theme(axis.text.x = element_text(angle=90, hjust = 1))


# BIND CDEC DATA ------------------------------------------------------------

cdec_daily <- bind_rows(cdec_temps_day, cdec_temps_day2, cdec_temps_day3_rev) # yay!

# check how many sites we have:
cdec_temps_day %>% distinct(station_id) %>% tally # daily
cdec_temps_day2 %>% distinct(station_id) %>% tally # hourly
cdec_temps_day3_rev %>% distinct(station_id) %>% tally # minute/event
cdec_daily %>% distinct(station_id) %>% tally # should be total of above 3 lines

# check how many stations avail per year
cdec_daily %>% group_by(year) %>% distinct(station_id) %>% tally() %>% as.data.frame()

# check how many years per station
cdec_daily %>% group_by(station_id) %>% distinct(year) %>% tally() %>% arrange(n) %>% as.data.frame()
# note, one station w/ less than 9 years, probably due to flagged data that was dropped

# SAVE ALL CDEC DAILY -----------------------------------------------------

# save as rda file...no additional compression needed
save(cdec_daily, file = "data/cdec_temps_merged_daily_filt8yr.rda")


# SAVE CDEC TO DATABASE ---------------------------------------------------

# first let's make a path to the database to store our daily temp data:
dbpath <-paste0(here::here(), "/data/databases/daily_temps.sqlite")

# create DB (ONLY RUN ONCE OR IT WILL OVERWITE EXISTING DB)
dbcon <- src_sqlite(dbpath, create = FALSE) # set create to TRUE

src_tbls(dbcon) # see tables in DB

# ADD A TABLES (can overwrite with append=TRUE or overwrite=T)
copy_to(dbcon, cdec_daily, temporary=FALSE, overwrite=TRUE, indexes=list(c("station_id", "year")))

copy_to(dbcon, df = cdec_info, name = "cdec_metadata_filt8", temporary = FALSE, indexes=list(c("site_id")))

# to check tables now exists
src_tbls(dbcon) # see tables in DB

# check size of tables
library(purrr)
library(RSQLite)
map(src_tbls(dbcon), ~dim(dbReadTable(dbcon$con, .))) %>% 
  set_names(., src_tbls(dbcon))

# gives the dimensions of each table in database
# sqlite_stat1 and sqlite_stat4 are part of the database ref system, can ignore

# to get a table back or "collect it":
#cdec_metadata_filt8 <- tbl(dbcon, "cdec_metadata_filt8") %>% collect()

# CLEAN UP USGS DATA ------------------------------------------------------

# rename col names
usgs_temps_day <- dataRetrieval::renameNWISColumns(usgs_temps_day)

# add C to values
usgs_temps_day1 <- usgs_temps_day %>% 
  rename(date=Date, station_id=site_no, water_year=waterYear) %>% 
  filter(Wtemp_cd =="A") %>% # filter 
  mutate(month_day = mday(date),
         date = as.Date(date),
         month = month(date),
         year = year(date)) %>% 
  rename(value_mean = Wtemp) %>%
  select(-c(agency_cd, Wtemp_cd))
  
# make event/hourly data into daily means
usgs_temps_day2 <- dataRetrieval::renameNWISColumns(usgs_temps_iv)

usgs_temps_day2 <- usgs_temps_day2 %>%
  rename(datetime=dateTime, station_id=site_no, water_year=waterYear) %>% 
  # filter out bad data
  filter(Wtemp_Inst_cd=="A") %>% 
  mutate(month_day = mday(datetime),
         month = month(datetime),
         year = year(datetime)) %>%
  select(-c(agency_cd, Wtemp_Inst_cd, tz_cd)) %>% 
  group_by(station_id, year, month, water_year, month_day) %>% 
  summarize(value_mean_C = mean(Wtemp_Inst, na.rm=T)) %>% 
  as.data.frame() %>%
  #add a datetime col back in
  mutate(date = mdy(paste0(as.integer(month), 
                               "-", month_day,"-", year)))

# how many unique stations 
usgs_temps_day1 %>% distinct(station_id) %>% tally() # (n=25)
usgs_temps_day2 %>% distinct(station_id) %>% tally() # (n=6)

# plot dailies
ggplot() +
  geom_line(data=usgs_temps_day1,
            aes(x=date, y=value_mean, color=station_id), show.legend = F)+
  scale_color_viridis_d() + labs(y="Temp (C)", x="") +
  theme(axis.text.x = element_text(angle=90, hjust = 1))

ggplot() +
  geom_line(data=usgs_temps_day2,
            aes(x=date, y=value_mean_C, color=station_id), show.legend = F)+
  scale_color_viridis_d() + labs(y="Temp (C)", x="") +
  theme(axis.text.x = element_text(angle=90, hjust = 1))

# BIND USGS DATA ----------------------------------------------------------

usgs_daily <- bind_rows(usgs_temps_day1, usgs_temps_day2) # yay!

# check how many sites we have:
usgs_temps_day1 %>% distinct(station_id) %>% tally # daily
usgs_temps_day2 %>% distinct(station_id) %>% tally # hourly
usgs_daily %>% distinct(station_id) %>% tally # should be total of above 2 lines

# check how many stations avail per year
usgs_daily %>% group_by(year) %>% distinct(station_id) %>% tally() %>% as.data.frame()

# SAVE USGS TO DATABASE ---------------------------------------------------

# save as rda file...no additional compression needed
save(usgs_daily, file = "data/usgs_temps_merged_daily_filt8yr.rda")

# SAVE USGS TO DATABASE ---------------------------------------------------

# first let's make a path to the database to store our daily temp data:
dbpath <-paste0(here::here(), "/data/databases/daily_temps.sqlite")

# create DB (ONLY RUN ONCE OR IT WILL OVERWITE EXISTING DB)
dbcon <- src_sqlite(dbpath, create = FALSE) # set create to TRUE

src_tbls(dbcon) # see tables in DB

# ADD A TABLES (can overwrite with append=TRUE or overwrite=T)
copy_to(dbcon, usgs_daily, temporary=FALSE, overwrite=TRUE, indexes=list(c("station_id", "year")))

copy_to(dbcon, usgs_metadata_filt8, name = "usgs_metadata_filt8", temporary=FALSE, overwrite=TRUE)

# to check tables now exists
src_tbls(dbcon) # see tables in DB

# check size of tables
library(purrr)
library(RSQLite)
map(src_tbls(dbcon), ~dim(dbReadTable(dbcon$con, .))) %>% 
  set_names(., src_tbls(dbcon))

# gives the dimensions of each table in database
# sqlite_stat1 and sqlite_stat4 are part of the database ref system, can ignore

# to get a table back or "collect it":
#cdec_metadata_filt8 <- tbl(dbcon, "cdec_metadata_filt8") %>% collect()
