library(mapview)
library(sf)
library(tidyverse)
library(tmap)
library(ggrepel)
library(ggspatial)

# Get data ----------------------------------------------------------------

CDEC_final <- read_csv("data/CDEC_gages_final.csv")
USGS_final <- read_csv("data/USGS_gages_final.csv")

hydro_regions <- st_read("data/DWR_HydrologicRegions-utm11.shp") %>% 
  st_transform(3310)

#Transform gage data to spatial data

USGS_final <- USGS_final %>% 
  st_as_sf(coords = c("dec_long_va", "dec_lat_va"), crs = 4269, remove = FALSE) %>%
  mutate(data_source = "USGS") %>%
  st_transform(3310)

CDEC_final <- CDEC_final %>% 
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326, remove = FALSE) %>% 
  mutate(data_source = "CDEC") %>% 
  st_transform(3310)

#merge the gage lists

CDEC_final <- CDEC_final %>% 
  rename(site_id = ID, site_name = Station.Name, lat = Latitude, lon = Longitude, operator = Operator) %>% 
  mutate(date_begin = NA, date_end = NA) %>% 
  select(site_id, site_name, lat, lon, date_begin, date_end, operator, data_source)

USGS_final <- USGS_final %>% 
  rename(site_id = site_no, lat = dec_lat_va, lon = dec_long_va, date_begin = qw_begin_date, date_end = qw_end_date) %>% 
  mutate(site_name = NA, operator = 'USGS') %>% 
  select(site_id, site_name, lat, lon, date_begin, date_end, operator, data_source)

all_gages <- rbind(CDEC_final, USGS_final)


# Add hydroregions to metadata --------------------------------------------

all_gages <- st_join(all_gages, hydro_regions[c("HR_CODE", "HR_NAME")])

all_gages %>%
  st_drop_geometry() %>% 
  group_by(HR_NAME) %>% 
  tally()

mapview(all_gages, zcol = 'HR_NAME') + mapview(hydro_regions, col.regions = NA)

mapview(all_gages, zcol = 'data_source')+mapview(hydro_regions, col.regions = NA)


# Save all_gages data as csv and rda files --------------------------------

write_csv(all_gages, path = "data/all_gages.csv") #save csv file

save(all_gages, file = "data/all_gages.rda") #save rda file


# Static map --------------------------------------------------------------

ggplot()+
  geom_sf(data = hydro_regions, fill = NA, color = 'blue', size = 1, alpha = 0.4) +
  geom_sf(data = all_gages, aes(fill = data_source), pch = 21, size = 4) +
  geom_sf_text(data = hydro_regions, aes(label = HR_NAME)) +
  annotation_north_arrow(location = "tr", pad_y = unit(0.1, "cm")) +
  annotation_scale()

ggsave(filename = "output/figures/all_gages_in_hyd_regions.png", dpi = 300, width = 8.5, height = 11, units = "in")
