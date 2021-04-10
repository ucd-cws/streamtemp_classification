# Code description --------------------------------------------------------

# calculate approximate affect of regulation by looking at drainage area vs. dam size/storage

# Libraries ---------------------------------------------------------------

# mapping
library(sf)
library(mapview)
library(nhdplusTools)
library(tidyverse)

options(scipen=999)

# 1. LOAD DATA ------------------------------------------------------------

# clustering data and dams
load("output/11a_agnes_k_5_final_w_centdist.rda") # data_k_sf_w_hydro_regions

# dams
load("output/11a_dams_nearest_final.rda")

# fix duplicates
dams_final <- dams_nearest_all_final %>% 
  distinct(.keep_all = TRUE) %>% # drops 14 total dams
  # drop these dams as they are upstream of another dam w no gage in between
  filter(!NAME %in% c("Goodwin", "Keswick","North Fork", "O'Shaughnessy (Hetch Hetchy Reservoir)")) %>% 
  select(-c(damname:inspdate)) %>% # drop empty cols
  mutate(lon = st_coordinates(.)[,1],
         lat = st_coordinates(.)[,2])

rm(dams_nearest_all_final)

# clean
data_k_sf <- data_k_sf_w_hydro_regions
data_k_sf <- st_set_crs(data_k_sf,4326)

# update/rename some cols
data_k_sf <- data_k_sf %>% 
  rename(cent_x = x, cent_y = y)

# setup some basemaps
mapbases <- c("Stamen.TonerLite", 
              "CartoDB.PositronNoLabels", "OpenStreetMap",
              "Esri.WorldImagery", "Esri.WorldTopoMap")
# and set defaults
mapviewOptions(basemaps=mapbases)

# preview
mapview(dams_final, col.regions="black", cex=8) +
  mapview(data_k_sf,  zcol="k5_names", map.types=mapbases,
          layer.name="Thermal Classes",
          col.regions=unique(data_k_sf$color[order(data_k_sf$k5_names)]), 
          alpha.regions=0.8, cex=3.5,
          hide=FALSE, homebutton=FALSE)

# 2. SET UP MAPPING -------------------------------------------------------

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


# GET FLOWLINES FROM DAMS ---------------------------------------------

dam_segs <- read_rds("output/12_selected_dam_comids.rds")
load("output/12_selected_nhd_dam_mainstems_us_ds.rda")
load("output/12_selected_gages_w_dams.rda")

# Get NHD Attribs ---------------------------------------------------------

# get the COMID for each gage in list using nhdplustools
usgs_segs <- dams_final %>% rowid_to_column() %>% 
  split(.$rowid) %>% # split by ID
  # then apply this function to each split to get COMID
  map(~discover_nhdplus_id(.x$geometry))


# now have a list of all COMIDs, check for duplicates
usgs_segs %>% 
  purrr::map_lgl(~ length(.x)>1) %>% table() # all FALSE

# to see as a dataframe:
dam_segs2 <- unlist(usgs_segs) %>% as_tibble(rownames = "rowid") %>% 
  rename(comid = value) %>% 
  # join back to original data
  bind_cols(., dams_final)

# download NHD attribs for every COMID that has a dam on it
# subset_file <- tempfile(fileext = ".gpkg")
# subset <- subset_nhdplus(comids = dams_segs$comid,
#                          output_file = subset_file,
#                          nhdplus_data = "download", 
#                          flowline_only = FALSE,
#                          return_data = TRUE)
# dam_nhd_plus_data <- subset
# save(dam_nhd_plus_data, file="output/dams_nhd_plus_data.rda")
# rm(subset)
# dam_nhd_plus_data$NHDFlowline_Network %>% View()

# load the data
load("output/dams_nhd_plus_data.rda")

# look at drainage area and dam height, pull just NHD flowline network
dams_nhd <- dam_nhd_plus_data$NHDFlowline_Network %>% st_transform(4326)

# join attributes of stream segs with dams
dams_joined <- left_join(dam_segs2, dams_nhd, by=c("comid")) 
save(dams_joined, file = "output/12b_dam_nhd_comid_attributes.rda")

# join attributes of lakes with dams
#lakes_joined <- st_join(dams_final, st_transform(dam_nhd_plus_data$NHDWaterbody, 4326))


# Calc alteration based on drainage area and size of reservoir -------------

# see attribute definitions: https://www.epa.gov/waterdata/learn-more#Documentation
## Alias: UpstreamCumulativeStreamKm (arbolatesu), qa_ma: mean annual flow? vc velocity, ve velocity adjust?
dams_joined_sel <- dams_joined %>% 
  select(comid, NAME:STO_10_6m3, gnis_id:lengthkm, arbolatesu, areasqkm:divdasqkm, starts_with("qa"), geometry.x) %>% 
  mutate(across(.cols = c(HEIGHT_FT:STO_10_6m3), as.numeric))

View(dams_joined_sel)

# use qa_ma as approx mean annual flow (cfs) ? Or just add storage volume and total upstream km and drainage area? 
dor <- dams_joined_sel %>% 
  select(comid, NAME:RIVER, STO_m3, STO_10_6m3, arbolatesu, totdasqkm, qa_ma) %>% 
  mutate(STO_10_6f3 = (STO_10_6m3 * 35.31),
         deg_o_reg = qa_ma/STO_10_6f3,
         us_km_by_sto = arbolatesu / STO_10_6m3)


# Get Mean Annual Flow w FFC?------------------------------------------------

# THIS WOULD NEED TO BE DONE FOR EACH AND IS A LOT OF EXTRA WORK
# # or look here: https://rivers.codefornature.org/#/map
# library(ffcAPIClient)
# library(usethis)
# ffctoken <- set_token(Sys.getenv("EFLOWS", ""))
# ffcAPIClient::clean_account(ffctoken)
# 
# load(file="output/dams_nhd_plus_data.rda")
# 
# ffc <- FFCProcessor$new()  # make a new object 
# ffc$set_up(comid = dam_segs$comid[1], token = ffctoken)
# 
# # then run
# ffc$run()
# 
# # then pull metrics
# ffc$alteration
# ffc$doh_data
# ffc$ffc_percentiles
# ffc$ffc_results
# ffc$predicted_percentiles
# ffc$predicted_wyt_percentiles
# 
# # steps
# ffc$step_one_functional_flow_results(comid = 2764502, gage_id = 11376025, token = ffctoken, output_folder = "output/ffc")
# 
# # calc mean annual flow?
# ffc$timeseries %>% group_by(waterYear) %>% 
#   summarize(meanFlow = mean(flow, na.rm=TRUE)) %>%
#   ungroup() %>% 
#   summarize(meanAnnFlow = mean(meanFlow),
#             meanAnnVol_af = meanAnnFlow * 31557600 * 0.000022956840904921) # s in a year

# do calc for DOR: storage volume / mean annual flow vol in acre feet here:
#425/15751 # for macumber
  
