## SUMMARY: Combine USGS & CDEC data and save to database 
## 2019-Dec
## DETAILS: 
# Data downloaded from USGS and CDEC gages will be combined to daily and saved to a SQLITE/GEOPACKAGE database for analysis.

# LIBRARIES ---------------------------------------------------------------

library(sf)
library(tidyverse)
library(mapview)
library(purrr)
library(lubridate)

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
glimpse(cdec_metadata_filt8)
length(unique(cdec_metadata_filt8$site_id)) # 65 total stations

# look at usgs metadata
usgs_metadata_filt8 %>% glimpse()
usgs_metadata_filt8 <- usgs_metadata_filt8 %>% 
  st_as_sf(coords=c("dec_long_va","dec_lat_va"), remove=FALSE, crs=4326)

# read in hydroregions:
dwr <- st_read("data/DWR_HydrologicRegions-utm11.shp") %>% st_transform(4326)

mapview(dwr, color="blue", col.regions=NA) + mapview(cdec_metadata_filt8, col.regions="orange") + mapview(usgs_metadata_filt8, col.regions="maroon")

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
plotly::ggplotly(
  ggplot() +
    geom_point(data=cdec_temps_day,
               aes(x=datetime, y=value_mean_C, color=station_id), size=2.5, alpha=.7,show.legend = F)+
    ylim(c(0,40)) +
    #facet_grid(station_id~.) +
    scale_color_viridis_d()+
    labs(y="Temp (C)", x="") +
    theme_bw()+
    #ggdark::dark_theme_classic() +
    theme(axis.text.x = element_text(angle=90, hjust = 1))
)


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

# plot
plotly::ggplotly(
  ggplot() +
  geom_point(data=cdec_temps_day2 %>% filter(station_id=="YRS"),
            aes(x=datetime, y=value_mean_C, color=station_id), show.legend = T)+
  #ylim(c(0,90)) +
  scale_color_viridis_d()+
  labs(y="Temp (C)", x="") +
  #ggdark::dark_theme_classic() +
  theme(axis.text.x = element_text(angle=90, hjust = 1)))


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

# plot
ggplot() +
  geom_line(data=cdec_temps_day3,
            aes(x=datetime, y=value_mean_C, color=station_id), show.legend = F)+
  scale_x_date(date_breaks = "2 years", date_labels = "%Y")+
  scale_color_viridis_d()+
  labs(y="Temp (F)", x="") +
  ggdark::dark_theme_classic() +
  theme(axis.text.x = element_text(angle=90, hjust = 1))

# BIND CDEC DATA ------------------------------------------------------------

cdec_daily <- bind_rows(cdec_temps_day, cdec_temps_day2, cdec_temps_day3) # yay!

# check how many sites we have:
cdec_daily %>% distinct(station_id) %>% tally

# check how many stations avail per year
cdec_daily %>% group_by(year) %>% distinct(station_id) %>% tally() %>% as.data.frame()

# check how many years per station
cdec_daily %>% group_by(station_id) %>% distinct(year) %>% tally() %>% arrange(n) %>% as.data.frame()
# note, one station w/ less than 9 years, probably due to flagged data that was dropped

# SAVE CDEC TO DATABASE ---------------------------------------------------

# first let's make a path to the database to store our daily temp data:
dbpath <-paste0(here::here(), "/data/databases/daily_temps.sqlite")

# create DB (ONLY RUN ONCE OR IT WILL OVERWITE EXISTING DB)
#dbcon <- src_sqlite(dbpath, create = TRUE) # set create to TRUE

src_tbls(dbcon) # see tables in DB

# ADD A TABLES (can overwrite with append=TRUE or overwrite=T)
copy_to(dbcon, cdec_daily, temporary=FALSE, indexes=list(c("station_id", "year")))

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
cdec_metadata_filt8 <- tbl(dbcon, "cdec_metadata_filt8") %>% collect()


# CLEAN UP USGS DATA ------------------------------------------------------

# look at usgs metadata 
usgs_metadata_filt8 %>% glimpse()

# add C to values
usgs_temps_day <- usgs_temps_day %>% 
  rename(value_mean = X_00010_00003, datetime=Date, station_id=site_no) %>% 
  mutate(month_day = mday(datetime),
         datetime = as.Date(datetime)) %>% 
  rename(value_mean = value) %>% 
  select(station_id:sensor_type, year, month, water_year, water_day, month_day, value_mean, datetime, value_mean_C)

# make hourly data into daily means
cdec_temps_day2 <- cdec_temps_hr %>% 
  # filter out bad data
  filter(flag=="N") %>% 
  mutate(month_day = mday(datetime)) %>% 
  group_by(station_id, dur_code, sensor_num, sensor_type, year, month, water_year, water_day, month_day) %>% 
  summarize(value_mean = mean(value, na.rm=T)) %>% as.data.frame() %>%
  #add a datetime col back in
  mutate(datetime = mdy(paste0(as.integer(month), "-", month_day,"-", year)),
         value_mean_C = convertTemp(value_mean, unit = "F", convert = "C"))

# plot
ggplot() +
  geom_line(data=cdec_temps_day2,
            aes(x=datetime, y=value_mean_C, color=station_id), show.legend = F)+
  #ylim(c(0,90)) +
  scale_color_viridis_d()+
  labs(y="Temp (F)", x="") +
  ggdark::dark_theme_classic() +
  theme(axis.text.x = element_text(angle=90, hjust = 1))

# make event into daily:
cdec_temps_day3 <- cdec_temps_min %>% 
  # filter out bad data
  filter(flag=="N") %>% 
  mutate(month_day = mday(datetime)) %>% 
  group_by(station_id, dur_code, sensor_num, sensor_type, year, month, water_year, water_day, month_day) %>% 
  summarize(value_mean = mean(value, na.rm=T)) %>% as.data.frame() %>% 
  mutate(datetime = mdy(paste0(as.integer(month), "-", month_day,"-", year)),
         value_mean_C = convertTemp(value_mean, unit = "F", convert = "C"))

# plot
ggplot() +
  geom_line(data=cdec_temps_day3,
            aes(x=datetime, y=value_mean_C, color=station_id), show.legend = F)+
  scale_x_date(date_breaks = "2 years", date_labels = "%Y")+
  scale_color_viridis_d()+
  labs(y="Temp (F)", x="") +
  ggdark::dark_theme_classic() +
  theme(axis.text.x = element_text(angle=90, hjust = 1))


# BIND USGS DATA ----------------------------------------------------------


# SAVE USGS TO DATABASE ---------------------------------------------------


