## SUMMARY: Download USGS data for selected gages and save 
## 2019-Dec
## DETAILS: 
# Download data from selected USGS gages, see here https://cran.r-project.org/web/packages/dataRetrieval/vignettes/dataRetrieval.html


# LIBRARIES ---------------------------------------------------------------

library(dataRetrieval)
library(sf)
library(tidyverse)
library(mapview)
library(purrr)
library(lubridate)


# GET SITES ---------------------------------------------------------------

# load data
load("data/all_gages.rda")

# add an interval column:
all_gages <- all_gages %>% 
  mutate(interval = NA) %>% 
  select(site_id:date_end, interval, operator:data_source)

# filter to the cdec gages and drop sf class
usgs <- all_gages %>% filter(data_source=="USGS") %>% 
  # drop gage id value in scinotation
  filter(!site_id=="3.84e+14") %>% 
  st_drop_geometry() %>% 
  select(site_id) # only gage numbers


# GET METADATA ------------------------------------------------------------

# temps: parameterCd = "00010", dailymean = statCd "00003"

# check what daily data is available:
(usgs_daily <- whatNWISdata(siteNumber=usgs$site_id, service='dv', parameterCd = '00010', statCd="00003") %>% 
  select(site_no, station_nm, dec_lat_va, dec_long_va, huc_cd, 
         data_type_cd, begin_date:count_nu) %>% 
   rename(interval=data_type_cd, huc8=huc_cd, site_id=site_no,
          date_begin=begin_date, date_end=end_date) %>% 
   mutate(yr_begin = year(date_begin),
          yr_end = year(date_end),
          yr_total = yr_end-yr_begin))
         
# total of 81 gages have daily data

# check what event data is available from gages not found ind daily
usgs_uv_sites <- filter(usgs, !site_id %in% usgs_daily$site_id)

# get event
(usgs_event <- whatNWISdata(siteNumber=usgs_uv_sites$site_id, 
                            service='uv', parameterCd = "00010") %>% 
    select(site_no, station_nm, dec_lat_va, dec_long_va, huc_cd, 
           data_type_cd, begin_date:count_nu) %>% 
    rename(interval=data_type_cd, huc8=huc_cd, site_id=site_no,
           date_begin=begin_date, date_end=end_date) %>% 
    mutate(yr_begin = year(date_begin),
           yr_end = year(date_end),
           yr_total = yr_end-yr_begin))


# VISUALIZE DATE RANGES FOR SITES -----------------------------------------

# visualize the stations to see date ranges
ggplot() + 
  geom_linerange(data=usgs_daily, 
                 aes(x=forcats::fct_reorder(site_id, huc8), ymin=yr_begin, ymax=yr_end, color=huc8), size=1.1, show.legend = F) + coord_flip() + 
  labs(x="", y="") + scale_color_viridis_d(option = "A")+
  theme_bw(base_family = "Roboto Condensed", base_size = 8)
  #ggdark::dark_theme_bw(base_family = "Roboto Condensed", base_size = 8)

ggsave(filename = "output/figures/usgs_gage_year_ranges_by_interval.pdf",
       width=11, height = 9.5, units = "in", device = cairo_pdf)

# now filter to data where gages have >8 yrs of data (minimum req is 8.5)
usgs_filt <- usgs_daily %>% filter(yr_total > 8)

# visualize the stations to see date ranges
ggplot() + 
  geom_linerange(data=usgs_filt, 
                 aes(x=forcats::fct_reorder(site_id, huc8), ymin=yr_begin, ymax=yr_end, color=huc8), size=1.1, show.legend = F) + coord_flip() + 
  labs(x="", y="") +
  scale_color_viridis_d()+
  ggdark::dark_theme_bw(base_family = "Roboto Condensed", base_size = 8)

ggsave(filename = "output/figures/usgs_gages_data_range_w_8yearsplus_interval.pdf", 
       width=11, height = 9.5, units = "in", device = cairo_pdf)


# DOWNLOAD DAILY DATA -----------------------------------------------------

# first filter to only "Daily" stations w > 8yrs data
usgs_filt <- usgs_daily %>% filter(yr_total > 8)

# Get daily
usgs_temps_day <- dataRetrieval::readNWISdv(siteNumbers=usgs_filt$site_id,parameterCd = "00010") 

usgs_temps_day <- usgs_temps_day %>% dataRetrieval::addWaterYear()

# save out
save(usgs_temps_day, file = "data/usgs_temps_all_daily_filt8yr.rda")

# DOWNLOAD UV DATA --------------------------------------------------------

# first filter to only "Daily" stations w > 8yrs data
usgs_filt <- usgs_event %>% filter(yr_total > 8)

usgs_temps_iv <- dataRetrieval::readNWISuv(siteNumbers=usgs_filt$site_id,parameterCd = "00010") 

usgs_temps_iv <- usgs_temps_iv %>% dataRetrieval::addWaterYear() 

# save out
save(usgs_temps_iv, file = "data/usgs_temps_all_iv_filt8yr.rda")
