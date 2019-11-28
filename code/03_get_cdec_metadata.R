# Query for CDEC info using sharpshootr

# https://ncss-tech.github.io/AQP/sharpshootR/CDEC.html

library(sharpshootR)
library(sf)
library(tidyverse)
library(mapview)
library(purrr)
library(lubridate)


# TO DO NOTES -------------------------------------------------------------

# adding time intervals and total years of data to metadata
# filter to only daily (then hourly, then event if no other options), 
# filter to ony greater than 12 years as first cut
# add flag for Y or N to download 
# set up the database code to write/save data to sqlite/geopackage
# set up googlesheet to track what's downloaded/filtered, etc
# database for raw data
# database for clean data (all daily, sites vetted, etc)
# database for modeled?

# high certainty=16 yrs
# reasonable=12 yrs
# minimum=8.5 yrs
# metrics using mean daily temps, then daily mean of each wateryear day
# timing of annual max
# magnitude of annual max
# annual mean

# GET SITES AND CLEAN -----------------------------------------------------

# load data
load("data/all_gages.rda")

# add an interval column:
all_gages <- all_gages %>% 
  mutate(interval = NA) %>% 
  select(site_id:date_end, interval, operator:data_source)

# filter to the cdec gages an drop sf class
cdec <- all_gages %>% filter(data_source=="CDEC") %>% 
  st_drop_geometry() %>% 
  select(site_id, interval, date_begin, date_end)


# DOWNLOAD METADATA W PURRR ------------------------------------------------


# use map to get the CDEC_StationInfo
cdec_info <- cdec %>% select(site_id) %>% 
  # these stations don't work, so filter out, check with #slice() 
  filter(!site_id %in% c("BND","FAR", "GRL","HEA", "MSD", 
                         "MST", "OBB", "RDB", "RDR", "SFJ", 
                         "SJP", "VON")) %>% 
  # run function to get metadata for each site_id
  mutate(sensor_data = map(site_id, 
                           ~CDEC_StationInfo(.x)[["sensor.meta"]])) %>% 
  # convert to dataframe (not listcol) & filter to only water temp data 
  unnest(cols=c(sensor_data)) %>% filter(sensor==25) %>% 
  # add a short abbrev col for interval
  mutate(interval_id = case_when(
    interval == "(hourly)" ~ "H",
    interval == "(event)" ~ "E",
    interval == "(daily)" ~ "D"))


# CLEAN METADATA -----------------------------------------------------------

# now clean to get begin/end dates, 
cdec_info <- cdec_info %>% 
  separate(period_of_record, sep = " to ", 
           into = c("date_begin", "date_end"), remove = FALSE) %>% 
  mutate(date_begin = gsub("/", replacement = "-", x = date_begin),
         date_end = gsub("/", replacement = "-", x = date_end),
         date_end = ifelse(date_end=="present", "10-01-2019", date_end),
         date_end = replace(date_end, site_id=="PTK", "10-01-2019"),
         date_begin = mdy(date_begin),
         date_end = mdy(date_end),
         yr_begin = year(date_begin),
         yr_end = year(date_end),
         yr_total = yr_end-yr_begin)

# visualize the stations to see date ranges
ggplot() + 
  geom_linerange(data=cdec_info, 
                 aes(x=site_id, ymin=yr_begin, ymax=yr_end, color=interval_id), position="dodge", show.legend = F) + coord_flip() + 
  labs(x="", y="") + 
  facet_grid(.~interval_id)+
  ggthemes::scale_color_pander()+
  ggdark::dark_theme_bw(base_family = "Roboto Condensed", base_size = 8)

ggsave(filename = "output/figures/cdec_gage_year_ranges_by_interval.pdf", 
       width=11, height = 9.5, units = "in", device = cairo_pdf)

# now filter to data where gages have >8 yrs of data (minimum req is 8.5)
cdec_filt <- cdec_info %>% filter(yr_total > 8)

# visualize the stations to see date ranges
ggplot() + 
  geom_linerange(data=cdec_filt, 
                 aes(x=site_id, ymin=yr_begin, ymax=yr_end, color=interval_id), position="dodge", show.legend = F) + coord_flip() + 
  labs(x="", y="") + 
  ggdark::dark_theme_bw(base_family = "Roboto Condensed", base_size = 8)+
  facet_grid(.~interval_id) +
  ggthemes::scale_color_pander()

ggsave(filename = "output/figures/cdec_gages_data_range_w_5yearsplus_interval.pdf", 
       width=11, height = 9.5, units = "in", device = cairo_pdf)

# DOWNLOAD DAILY DATA w PURRR ----------------------------------------------

# first filter to only "Daily" stations
cdec_daily <- cdec_filt %>% filter(interval_id=="D") %>% 
  select(site_id, sensor, interval_id, date_begin, date_end)

# use "pmap" function (parallel map) to loop through and get data
cdec_temps_day <- cdec_daily %>% 
  mutate(tempdata = pmap(cdec_daily, ~CDECquery(id=..1, sensor=..2, interval=..3, start=..4, end=..5))) 

# convert to dataframe (from list col)
cdec_temps_day <- cdec_temps_day %>% unnest(cols=c(tempdata))

# flag bad values:
cdec_temps_day <- cdec_temps_day %>% 
  mutate(flag=if_else(value<0 | value > 100, "Y", "N"))

# save out
save(cdec_temps_day, file = "data/cdec_temps_all_daily_filt8yr.rda")
save(cdec_info, file="data/cdec_stations_metadata_dateranges_112_stations.rda")

# PLOT --------------------------------------------------------------------

# some prelim plots
ggplot() + 
  geom_point(data=cdec_temps_day %>% filter(flag=="N"), aes(x=water_day, y=value, color=year)) + ylim(c(30,90))+
  facet_grid(site_id~.)

ggplot() + 
  geom_line(data=cdec_temps_day %>% filter(flag=="N"), aes(x=datetime, y=value), color="dodgerblue", show.legend = F) + ylim(c(30,90))+ 
  scale_x_datetime(date_breaks = "1 year", date_labels = "%Y") +
  facet_grid(site_id~.) +
  labs(y="Temp (F)", x="") +
  ggdark::dark_theme_classic() +
  theme(axis.text.x = element_text(angle=90, hjust = 1))

ggsave(filename = "output/figures/daily_cdec_temps_example.png", dpi=300,
       width = 11, height = 8, units = "in")

# TESTING SINGLE GAGE METADATA -----------------------------------------------------

## GETTING METADATA
# look at metadata for a single gage:
tst_id <- "AFD"
meta <- CDEC_StationInfo(tst_id)[["sensor.meta"]] %>% 
  filter(sensor==25)

# get temp sensor = 25 and get date range/period of record
meta <- meta %>% 
  mutate(station_id=tst_id) %>% 
  separate(period_of_record, sep = " ", into = "date_begin", remove = FALSE) %>% 
  mutate(date_begin = gsub("/", replacement = "-", x = date_begin),
         date_end = lubridate::ymd("2019-10-01"),
         date_begin = lubridate::ymd(date_begin))


# TESTING SINGLE GAGE GETTING DATA ------------------------------------------------------------

## GETTING LOGGER DATA

tempdata <- CDECquery(id=temp_sensor$station_id, sensor=25, interval='H', start=temp_sensor$date_begin, end=temp_sensor$date_end)

tempdata_e <- CDECquery(id=temp_sensor$station_id, sensor=25, interval='E', start=temp_sensor$date_begin, end=temp_sensor$date_end)
