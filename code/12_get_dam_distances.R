# Code description --------------------------------------------------------

# this gets streamlines and dams, snaps points to streamlines and calculates distance to nearest dam

# Libraries ---------------------------------------------------------------

library(mapview)
library(sf)
library(tidyverse)
library(ggthemes)
library(ggrepel)
library(ggspatial)
library(nhdplusTools)
#library(wateRshedTools)

# setup some basemaps
mapbases <- c("Stamen.TonerLite","OpenTopoMap", "CartoDB.PositronNoLabels", "OpenStreetMap",
              "Esri.WorldImagery", "Esri.WorldTopoMap","Esri.WorldGrayCanvas"
)
# and set defaults
mapviewOptions(basemaps=mapbases)

# LOAD DATA ---------------------------------------------------------------

# clustering data and dams
load("output/models/agnes_k_groups_sf_w_dams.rda")

# LOAD INTERMEDIATE DATA FILES --------------------------------------------------

# If script has been run once (steps 01:06), you can start at 07 and load these data:
load("output/12_selected_nhd_mainstems_for_gages_us_ds.rda")
load("output/12_selected_nhd_mainstems_for_gages.rda")
usgs_segs <- readRDS("output/12_selected_gage_comids.rds")
dams_selected <- readRDS("output/12_selected_dam_comids.rds")

# * Reorder Thermal k=5 Classifications -------------------------------------------------

# Thermal Classifications
# "stable warm" = 1
# "reg warm"= 3, 
# "reg cool" = 4,
# "unreg cool"= 2, 
# "stable cold"= 5

# * Make Custom Color Palette ----------------------------------------------

# assign color palette based on classifications:
thermCols <- data.frame(k5_group_id = c(1,3,4,2,5),
                        k5_names  = c("1-stable warm", "2-reg warm",
                                      "3-reg cool", "4-unreg cool",
                                      "5-stable cold"),
                        color = I(c("#E41A1C", #stable warm
                                    "#FF7F00", #reg warm
                                    "#984EA3", #reg cool
                                    "#4DAF4A", #unreg cool
                                    "#377EB8" #stable cold
                        )))

# join w colors
data_k_sf <- left_join(data_k_sf, thermCols, by=c("k_5"="k5_group_id"))

# * Static Plot --------------------------------------------------------------------
# using basemap options

# plot CA counties
plot(USAboundaries::us_counties(resolution = "low", states="ca")$geometry, border = alpha("gray70", 0.8), lwd=0.9, lty=3)
# plot state boundary
plot(USAboundaries::us_boundaries(type="state", states="ca")$geometry, lwd=2, add=T)
plot(data_k_sf$geometry, pch=21, 
     bg=data_k_sf$color, add=T)
legend(x = 'bottomright', 
       legend = as.character(thermCols$k5_names),
       col = thermCols$color,
       pch = 16, bty = 'n', xjust = 1)

# 01: GET COMID FOR GAGES -----------------------------------------------------

# get the COMID for each gage in list using nhdplustools
usgs_segs <- data_k_sf %>% 
  split(.$station_id) %>% # split by ID
  # then apply this function to each split to get COMID
  map(~discover_nhdplus_id(.x$geometry))

# now have a list of all COMIDs, check for duplicates
usgs_segs %>% 
  purrr::map_lgl(~ length(.x)>1) %>% table() # all FALSE
 # to look for TRUE try this: .[.==TRUE] 

# to see as a dataframe:
usgs_segs <- unlist(usgs_segs) %>% as_tibble(rownames = "station_id") %>% 
  rename(comid = value)

# save the USGS station COMIDs file:
write_rds(usgs_segs, path="output/12_selected_gage_comids.rds")
write_csv(usgs_segs, path="output/12_selected_gage_comids.csv")

# 02: GET UPSTREAM FLOWLINES FROM GAGE ---------------------------------------------

# use the list of comids to pass to the nhdplusTools function
coms_list <- map(usgs_segs$comid, ~list(featureSource = "comid", featureID=.x))

# Get Upstream mainstem segs, this can take a minute
mainstemsUS <- map(coms_list, ~navigate_nldi(nldi_feature = .x,
                                             mode="upstreamMain",
                                             #distance_km = 10,
                                             data_source = ""))

# make a single flat layer
mainstems_flat_us <- mainstemsUS %>%
  set_names(., usgs_segs$station_id) %>%
  map2(usgs_segs$station_id, ~mutate(.x, station_id=.y))

# bind together
mainstems_us <- do.call(what = sf:::rbind.sf,
                        args = mainstems_flat_us)

# remove rownames
rownames(mainstems_us) <- c()

# add direction to gage col
mainstems_us <- mainstems_us %>% 
  mutate(from_gage = "US")

rm(mainstems_flat_us, mainstemsUS)

# 03: GET DOWNSTREAM FLOWLINES FROM GAGE -----------------------------------------

# get NHD segments downstream of selected USGS gages, 30 km buffer
mainstemsDS <- map(coms_list, ~navigate_nldi(nldi_feature = .x,
                                             mode="downstreamMain",
                                             distance_km = 30,
                                             data_source = ""))

# make a single flat layer
mainstems_flat_ds <- mainstemsDS %>%
  set_names(., usgs_segs$station_id) %>%
  map2(usgs_segs$station_id, ~mutate(.x, station_id=.y))


# bind together
mainstems_ds <- do.call(what = sf:::rbind.sf,
                        args = mainstems_flat_ds)

# remove weird rownames
rownames(mainstems_ds) <- c()

# add direction to gage col
mainstems_ds <- mainstems_ds %>% 
  mutate(from_gage = "DS")

rm(mainstems_flat_ds, mainstemsDS)

# bind all mainstems
mainstems_all <- rbind(mainstems_us, mainstems_ds)

# quick check
mapview(dams_nearest, col.regions="black", cex=2) + 
  mapview(mainstems_us, color="darkblue", lwd=0.7) +
  mapview(mainstems_ds, color="purple", lwd=1.5) +
  mapview(data_k_sf, col.regions="orange", cex=4)

# 04: COMBINE AND SAVE STREAMLINES ------------------------------------------

save(mainstems_all, file="output/12_selected_nhd_mainstems_for_gages.rda")

save(mainstems_us, mainstems_ds, file = "output/12_selected_nhd_mainstems_for_gages_us_ds.rda")

# to write out as a shapefile:
#st_write(obj=mainstems_all, "output/12_selected_nhd_mainstems_for_gages.shp")

# 05: GET COMID FOR DAMS  ----------------------------------------------------------

# crop ALL dams layer to bounding box of our gage sites:
# both sf objects need to be in same projection
st_crs(dams) == st_crs(data_k_sf) # TRUE is GOOD

# crop
dams_crop <- st_crop(dams, st_bbox(data_k_sf))

# preview
#mapview(dams_crop, color="black",col.regions="gray", alpha.regions=0.8, cex=2) + 
  #mapview(data_k_sf, col.regions="orange", color="orange4", cex=4)
  
## now use this set of dams to get COMIDs

# get the COMID for each gage in list
dam_coms <- dams_crop %>% 
  split(.$NID) %>% # split by ID
  # then apply this function to each split to get COMID
  map(~discover_nhdplus_id(.x$geometry))

# check for dups
dam_coms %>% 
  purrr::map_lgl(~ length(.x)>1) %>% 
  .[.==TRUE] 

# two dups: check and fix by picking first one
dam_coms["CA00555"]
dam_coms["CA00925"]

# fix, just take the first comid 
dam_coms["CA00555"] <- 8007593
dam_coms["CA00925"] <- 7924053

# double check again
dam_coms %>% 
  purrr::map_lgl(~ length(.x)>1) %>% table() # all

# flatten list
dam_coms <- unlist(dam_coms) %>% as_tibble(rownames = "NID") %>% 
  rename(comid = value)

# rejoin with original dams list:
dams_crop <- left_join(dams_crop, dam_coms) %>% 
  select(comid, everything())

# 06: SELECT NEAREST DAMS USING COMID ---------------------------------------

## search for dams nearest to a gage site that occur in same 
# comid/nhdline mainstem list. 

dams_selected <- dams_crop %>% filter(comid %in% mainstems_all$nhdplus_comid)

# save selected dam_comids
saveRDS(dams_selected, file = "output/12_selected_dam_comids.rds")

# can also select nearest dams with st_nearest:
# BUT this will select the nearest dam for every single gage, even if not in same watershed (irrespective of watersheds or distance)

# use this after selecting dams (to keep in same drainage), to figure out distances
dams_nearest <- dams_selected[st_nearest_feature(data_k_sf, dams_selected),]


# 07: GET DISTANCE TO NEAREST DAM -----------------------------------------

# need to make sure this is in same CRS...
# transform to UTM for more accurate grid using 3310
dams_nearest <- st_transform(dams_nearest, 3310)
dams_selected <- st_transform(dams_selected, 3310)
data_k_sf <- st_transform(data_k_sf, 3310)
mainstems_all <- st_transform(mainstems_all, 3310)

# get distance to nearest DAM from GAGE:
# this gives a matrix of each dam and each gage
dam_dist_matrix <- st_distance(dams_nearest, data_k_sf, 
            by_element = FALSE) %>% units::set_units("km")

# this gets a measurement for each gage but....not sure about it
data_k_sf <- data_k_sf %>% 
  mutate(
    dist_to_dam_m = as.vector(st_distance(dams_nearest, ., 
                                          by_element = TRUE)),
    dist_to_dam_km = dist_to_dam_m/1000)

save(data_k_sf, file = "output/models/agnes_k_groups_w_selected_dams_v2.rda")

# 08: MAPVIEW IT! ------------------------------------------------------------------

# map it!
m5 <- mapview(dams_nearest, col.regions="black", color="gray50",
              alpha.regions=0.8, layer.name="Nearest Dams", 
              cex=6, homebutton=FALSE) +
  mapview(dams, col.regions="gray50", alpha.regions=0.5, 
          cex=3.4, layer.name="All Dams", homebutton=FALSE) +
  mapview(data_k_sf, zcol="k_5_f", col.regions=unique(thermCols$color),
          alpha.regions=0.8, homebutton=FALSE, layer.name="Thermal Classes") +
  mapview(mainstems_all, color="steelblue", lwd=2.2, 
          layer.name="NHD Flowlines", legend=FALSE)

m5@map %>% leaflet::addMeasure(primaryLengthUnit = "meters")

# in looking at this, there are a bunch of diversions that should be dropped...and focus on just the larger dams. 


# 08B: Look at Distances by Group -----------------------------------------

data_k_sf %>% st_drop_geometry %>% 
  group_by(k_5) %>% tally() -> grp_cnts

data_k_sf %>% st_drop_geometry %>% 
  group_by(k_5) %>%  
  summarize(mean_dam_dist_km = mean(dist_to_dam_km)) %>%
  left_join(grp_cnts) %>% 
  ggplot() + geom_col(aes(x= k_5, y=mean_dam_dist_km), fill=thermCols$color, alpha=0.8) + theme_classic()

ggsave("output/figures/mean_dist_to_dam_by_k5.png", width = 8, height = 6, units = "in", dpi = 300)

# filter out to just reg warm/reg cool
data_k_sf %>% st_drop_geometry() %>% 
  filter(k_5 %in% c("reg warm", "reg cool")) %>% 
  group_by(k_5) %>% 
  ggplot() + geom_histogram(aes(x=dist_to_dam_km, fill=color), 
                            #color=c("#FF7F00", "#984EA3"), 
                            binwidth = 3, bins = 50) +
  facet_wrap(k_5~.) + theme_clean()

ggsave("output/figures/hist_dist_to_dam_by_reg_cool_warm.png", width = 8, height = 6, units = "in", dpi = 300)

# 09: STATIC MAP FOR k=5 --------------------------------------------------------------

ggplot()+
  geom_sf(data=USAboundaries::us_boundaries(type="state", states="ca")$geometry, fill = NA, color = 'slategray4', size = 1, alpha = 0.4) +
  geom_sf(data=USAboundaries::us_counties(states="ca")$geometry, fill = NA, color = 'slategray4', size = 0.3, lty=2, alpha = 0.4) +
  
  geom_sf(data = data_k_sf, aes(fill = k_5), pch = 21, size = 3) +
  annotation_north_arrow(location = "tr", pad_y = unit(0.1, "cm")) +
  theme_map(base_family = "Roboto Condensed", base_size = 14)+
  scale_fill_manual("Thermal \nClasses", values=thermCols$color) +
  annotation_scale(location="br")

ggsave(filename="output/figures/agnes_k_5_classification_map.png", dpi=300, width=8, height = 11.5, units="in")

