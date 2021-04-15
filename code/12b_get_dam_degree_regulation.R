# Code description --------------------------------------------------------

# calculate approximate affect of regulation by looking at drainage area vs. dam size/storage

# Libraries ---------------------------------------------------------------

# mapping
library(sf)
library(mapview)
library(tidyverse)

options(scipen=999)

# 1. LOAD DATA ------------------------------------------------------------

# dams with FID
load("output/11a_dams_nearest_final.rda") # dams_nearest_all_final

# fix duplicates
dams_final <- dams_nearest_all_final %>% 
  distinct(.keep_all = TRUE) %>% # drops 14 total dams
  # drop these dams as they are upstream of another dam w no gage in between
  filter(!NAME %in% c("Goodwin", "Keswick","North Fork", "O'Shaughnessy (Hetch Hetchy Reservoir)")) %>% 
  select(-c(damname:inspdate)) %>% # drop empty cols
  mutate(lon = st_coordinates(.)[,1],
         lat = st_coordinates(.)[,2])

rm(dams_nearest_all_final) # rm orig dataset

# get dam segment COMIDs
dam_segs <- read_rds("output/12_selected_dam_comids.rds")

# get data_k with dam names paired
load("output/12_data_k_centdist_damdist.rda") # data_k_sf_hdyro_regions

# get flowlines downstream of dam
load("output/12_dams_final_mainstems_merged.rda") # ds_main_merged

# Get Degree of Reg Data from Ted's 2014 paper
dor_dat <- readxl::read_xlsx("data/grantham_2014_DOR_fInal_screen.xlsx", sheet=2)

# get supp table info:
all_sites <- read_csv("output/all_sites_metadata_model_results.csv")

# Mapview Map -------------------------------------------------------------

# setup some basemaps
mapbases <- c("Stamen.TonerLite", 
              "CartoDB.PositronNoLabels", "OpenStreetMap",
              "Esri.WorldImagery", "Esri.WorldTopoMap")
# and set defaults
mapviewOptions(basemaps=mapbases)

# preview
mapview(dams_final, col.regions="black", cex=8) +
  mapview(data_k_sf_w_hydro_regions,  zcol="k5_names", map.types=mapbases,
          layer.name="Thermal Classes",
          col.regions=unique(data_k_sf_w_hydro_regions$color[order(data_k_sf_w_hydro_regions$k5_names)]), 
          alpha.regions=0.8, cex=3.5,
          hide=FALSE, homebutton=FALSE) +
  mapview(ds_main_merged, lwd = "segs", layer.name="Streamline")

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

# JOIN Data ---------------------------------------------------------------

# join on NID
dams_final_dor <- left_join(dams_final[,c(1:6,14:16)], dor_dat, by="NID") %>% 
  mutate(NAME = coalesce(NAME.x, NAME.y)) %>% # see here for coalesce usage: https://alistaire.rbind.io/blog/coalescing-joins/
  select(NAME, comid, NID:RIVER, DRAIN_SQKM:CDOR, lon, lat, -c("NAME.x", "NAME.y", "HEIGHT_FT"))
  
# save out
write_rds(dams_final_dor, file="output/12b_dams_final_deg_of_reg.rds")
write_csv(st_drop_geometry(dams_final_dor), file="output/12b_dams_final_deg_of_reg.csv")

# join with all sites 
all_data_k_dor <- left_join(data_k_dist, 
                            st_drop_geometry(dams_final_dor), by=c("dam_name"="NAME"))

m1 <- mapview(all_data_k_dor, zcol="DOR")
m2 <- mapview(all_data_k_dor, zcol="CDOR") + 
  mapview(dams_final, col.regions="black", cex=8) +
  mapview(data_k_dist,  zcol="k5_names", map.types=mapbases,
          layer.name="Thermal Classes",
          col.regions=unique(data_k_dist$color[order(data_k_dist$k5_names)]), 
          alpha.regions=0.8, cex=3.5) +
  mapview(ds_main_merged, lwd = "segs", layer.name="Streamline")


m1+m2


# EXPORT TABLES FOR MANUSCRIPT --------------------------------------------

# split into 2 tables, one with just gage/thermal metadata and reg/unreg
# one with dam DOR info and gage ID so tables can be cross referenced
all_data_k_dor <- all_data_k_dor %>% st_drop_geometry() %>% 
  # drop duplicated cols
  mutate(reg_type = if_else(!is.na(dam_name),"REG", "UNREG")) %>% 
  rename(lon=lon.x, lat=lat.x, station_comid = comid.x, dam_comid=comid.y,
         dam_lon = lon.y, dam_lat = lat.y)

write_rds(all_data_k_dor, file="output/12b_all_data_k_deg_reg.rds")  
write_csv(all_data_k_dor, file="output/12b_all_data_k_deg_reg.csv")  

names(all_data_k_dor)

# split into two tables
all_data_k_no_dor <- all_data_k_dor %>% 
  select(station_id:station_comid, reg_type)

dam_data_dor_only <- all_data_k_dor %>% 
  filter(reg_type=="REG") %>% 
  select(station_id:k5_names, site_name, dam_name, dam_comid:dam_lat)

names(all_data_k_no_dor)
names(dam_data_dor_only)

# save out
write_csv(all_data_k_no_dor, file="output/12b_all_data_k_no_dam_dor.csv")
write_csv(dam_data_dor_only, file="output/12b_dam_data_k_dor_only.csv")

# Get NHD Attribs ---------------------------------------------------------
# 
# # get the COMID for each gage in list using nhdplustools
# usgs_segs <- dams_final %>% rowid_to_column() %>% 
#   split(.$rowid) %>% # split by ID
#   # then apply this function to each split to get COMID
#   map(~discover_nhdplus_id(.x$geometry))
# 
# 
# # now have a list of all COMIDs, check for duplicates
# usgs_segs %>% 
#   purrr::map_lgl(~ length(.x)>1) %>% table() # all FALSE
# 
# # to see as a dataframe:
# dam_segs2 <- unlist(usgs_segs) %>% as_tibble(rownames = "rowid") %>% 
#   rename(comid = value) %>% 
#   # join back to original data
#   bind_cols(., dams_final)
# 
# # download NHD attribs for every COMID that has a dam on it
# # subset_file <- tempfile(fileext = ".gpkg")
# # subset <- subset_nhdplus(comids = dams_segs$comid,
# #                          output_file = subset_file,
# #                          nhdplus_data = "download", 
# #                          flowline_only = FALSE,
# #                          return_data = TRUE)
# # dam_nhd_plus_data <- subset
# # save(dam_nhd_plus_data, file="output/dams_nhd_plus_data.rda")
# # rm(subset)
# # dam_nhd_plus_data$NHDFlowline_Network %>% View()
# 
# # load the data
# load("output/dams_nhd_plus_data.rda")
# 
# # look at drainage area and dam height, pull just NHD flowline network
# dams_nhd <- dam_nhd_plus_data$NHDFlowline_Network %>% st_transform(4326)
# 
# # join attributes of stream segs with dams
# dams_joined <- left_join(dam_segs2, dams_nhd, by=c("comid")) 
# save(dams_joined, file = "output/12b_dam_nhd_comid_attributes.rda")

# join attributes of lakes with dams
#lakes_joined <- st_join(dams_final, st_transform(dam_nhd_plus_data$NHDWaterbody, 4326))


# Calc alteration based on drainage area and size of reservoir -------------

# see attribute definitions: https://www.epa.gov/waterdata/learn-more#Documentation
## Alias: UpstreamCumulativeStreamKm (arbolatesu), qa_ma: mean annual flow? vc velocity, ve velocity adjust?

