# join Maheu sites with our sites
# look for overlaps


# Libraries ---------------------------------------------------------------

library(tidyverse)
library(sf)
library(janitor)
library(mapview)
mapviewOptions(fgb = FALSE)
library(USAboundaries)

# Get Data ----------------------------------------------------------------

# get maheu data
maheu <- readxl::read_xlsx(path = "data/Maheu_2016_RRA_USAregimes.xlsx", sheet=1) %>% 
  clean_names() %>% # fix names and remove '' around USGS ids
  mutate(usgs_id = stringr::str_remove_all(usgs_id, pattern = "'")) %>% 
  # make sf
  st_as_sf(coords=c("longitude","latitude"), remove=FALSE, crs=4269) %>% 
  # add k number to match with color scheme but use variable cold = variable cool
  mutate(k_name = case_when(
    thermal_regime == "stable cold" ~ "5-stable cold",
    thermal_regime == "stable cool" ~ "3-stable cool",
    thermal_regime == "variable cold" ~ "4-variable cool"
  ))

# stream class k data, all sites
all_dat_k <- read_rds("output/12b_all_data_k_deg_reg.rds") %>% 
  # make sf
  st_as_sf(coords = c("lon", "lat"), remove=FALSE, crs=4269)

# get CA outline and buffer by 5km
ca <- USAboundaries::us_states(states="ca") %>% 
  st_transform(3310) %>% st_buffer(dist=5000) %>% 
  st_transform(4269)

# Crop Sites by CA --------------------------------------------------------

maheu_ca <- st_intersection(maheu, ca)

# SET UP MAPPING COLORS ----------------------------------------------

# assign color palette based on classifications:
thermCols <- data.frame(k5_group_id = c(1:5),
                        k5_names  = c("1-stable warm", "2-variable warm",
                                      "3-stable cool", "4-variable cool",
                                      "5-stable cold"),
                        color = I(c("#E41A1C", #stable warm
                                    "#FF7F00", #variable warm
                                    "#984EA3", #stable cool
                                    "#4DAF4A", #variable cool
                                    "#377EB8" #stable cold
                        )))

# join with maheu
maheu_ca <- maheu_ca %>% left_join(., thermCols, by=c("k_name"="k5_names"))

# Mapview -----------------------------------------------------------------

mapview(maheu_ca, zcol= "k_name", layer.name="Maheu",
        col.regions=unique(maheu_ca$color[order(maheu_ca$k_name)]),
        cex = 8) + 
  mapview(all_dat_k, zcol = "k5_names", 
          layer.name="Willis",
          col.regions=unique(all_dat_k$color[order(all_dat_k$k5_names)]),
          cex = 3.5)
