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

# rearrange levels to match color palette:
data_k_sf <- data_k_sf %>% 
  mutate(k_5 = forcats::fct_relevel(k_5, "stable warm", "reg warm", "reg cool", "unreg cool", "stable cold"))

levels(data_k_sf$k_5)

# * Make Custom Color Palette ----------------------------------------------

# assign color palette based on classifications:
# thermCol1 <- RColorBrewer::brewer.pal(nlevels(data_k_sf$k_5), name="Set1")

# make a custom set of colors in a dataframe
thermCols <- with(data_k_sf, 
                  data.frame(k_5 = levels(k_5),
                             color = I(c("#E41A1C", #stable warm
                                         "#FF7F00", #reg warm
                                         "#984EA3", #reg cool
                                         "#4DAF4A", #unreg cool
                                         "#377EB8" #stable cold
                                       ))))
thermCols

# use "match" to match colors
#thermCols$color[match(data_k_sf$k_5, thermCols$k_5)]

# or use "merge" to add to existing dataframe
data_k_sf <- merge(data_k_sf, thermCols)

# * Static Plot --------------------------------------------------------------------
# using basemap options

# plot CA counties
plot(USAboundaries::us_counties(resolution = "low", states="ca")$geometry, border = alpha("gray70", 0.8), lwd=0.9, lty=3)
# plot state boundary
plot(USAboundaries::us_boundaries(type="state", states="ca")$geometry, lwd=2, add=T)
plot(data_k_sf$geometry, pch=21, 
     bg=data_k_sf$color, add=T)
legend(x = 'bottomright', 
       legend = as.character(thermCols$k_5),
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
#dams_nearest <- dams[st_nearest_feature(data_k_sf, dams),]

# 07: DROP CANAL SITE ---------------------------------------------------------

# note: this site: BW-12 IMPORT TO BUTTE CREEK is a canal and should be dropped
data_k_sf_v2 <- data_k_sf %>% filter(station_id != "BBW")


# 08: GET DISTANCE TO NEAREST DAM -----------------------------------------

# need to make sure this is in same CRS...
# transform to UTM for more accurate grid using 3310
dams_selected <- st_transform(dams_selected, 3310)
data_k_sf_v2 <- st_transform(data_k_sf_v2, 3310)
mainstems_all <- st_transform(mainstems_all, 3310)

# get distance to nearest DAM from GAGE:
# this gives a matrix of each dam and each gage
dam_dist_matrix <- st_distance(dams_selected, data_k_sf_v2, 
            by_element = FALSE) %>% units::set_units("km")

# this gets a measurement for each gage but....not sure about it
data_k_sf_v2 <- data_k_sf_v2 %>% 
  mutate(
    dist_to_dam_m = as.vector(st_distance(dams_selected, ., 
                                          by_element = TRUE)),
    dist_to_dam_km = dist_to_dam_m/1000)

# warning is ok...just means there ar fewer dams than gages...
# save it out:
save(data_k_sf_v2, file = "output/models/agnes_k_groups_v2_w_selected_dams.rda")

# 09: MAPVIEW IT! ------------------------------------------------------------------

# map it!
m5 <- mapview(dams_selected, col.regions="black", color="gray50",
              alpha.regions=0.8,
              layer.name="Dams", cex=2,
              hide=TRUE, homebutton=FALSE)+
  mapview(data_k_sf_v2,  zcol="k_5", map.types=mapbases,
          col.regions=unique(data_k_sf$color), alpha.regions=0.8, 
          burst=TRUE, hide=FALSE, homebutton=FALSE) +
  mapview(mainstems_all, zcol="from_gage", lwd=2)

m5@map %>% leaflet::addMeasure(primaryLengthUnit = "meters")

# 10: STATIC MAP FOR k=5 --------------------------------------------------------------

ggplot()+
  geom_sf(data=USAboundaries::us_boundaries(type="state", states="ca")$geometry, fill = NA, color = 'slategray4', size = 1, alpha = 0.4) +
  geom_sf(data=USAboundaries::us_counties(states="ca")$geometry, fill = NA, color = 'slategray4', size = 0.3, lty=2, alpha = 0.4) +
  
  geom_sf(data = data_k_sf, aes(fill = k_5), pch = 21, size = 3) +
  annotation_north_arrow(location = "tr", pad_y = unit(0.1, "cm")) +
  theme_map(base_family = "Roboto Condensed", base_size = 14)+
  scale_fill_manual("Thermal \nClasses", values=thermCols$color) +
  annotation_scale(location="br")

ggsave(filename="output/figures/agnes_k_5_classification_map.png", dpi=300, width=8, height = 11.5, units="in")

# THIS IS ALL SCRATCH ----------------------------------------------------------------

# make a matrix of touching riversegs
# library(rgeos)
# 
# # Create a matrix of segments that touch (so contiguous streamlines)
# touching_rivers <- st_touches(mainstems_all, sparse = FALSE)
# 
# # Merge all rivers that touch each other and convert all
# rivers_hclust <- hclust(as.dist(!touching_rivers), method = "single")
# 
# # Cut the dendrogram at height=0.5 so all touching rivers cluster together
# rivers_groups <- cutree(rivers_hclust, h = 0.5)
# 
# # check
# table(rivers_groups)
# 
# # filter to a single group: Sacramento
# sac_sf <- mainstems_all[rivers_groups == 5, ]
# # filter to a single group: Sacramento
# sj_sf <- mainstems_all[rivers_groups == 4, ]
# # check
# mapview(sac_sf, color="blue", lwd=2) + mapview(sj_sf, color="purple3", lwd=2)
# 
# 
# # Estimate the nearest point 
# nearest_point <- data_k_sf_v2 %>%
#   mutate(
#     index_of_nearest_feature = st_nearest_feature(., dams_selected),
#     nearest_feature = st_geometry(sac_sf[index_of_nearest_feature,]),
#     nearest_point = purrr::pmap(
#       list(geometry, nearest_feature),
#       ~ st_nearest_points(.x, .y) %>% 
#         st_cast("POINT") %>%
#         magrittr::extract2(2))) %>%
#   pull(nearest_point) %>% 
#   st_as_sfc(., crs=3310) #%>% 
# #st_cast("POINT")  %>% 
# 
# mapview(sac_sf, color="red",lwd=2) + nearest_point + 
#   mapview(data_k_sf_v2, col.regions="orange")+
#   mapview(dams_selected, col.regions="black")
# 
# 
# # Snap Points to River
# # buffer the points by the tolerance
# points_buf <- st_buffer(points, 15)
# # intersect the line with the buffer
# line_intersect <- st_intersection(line, points_buf)

