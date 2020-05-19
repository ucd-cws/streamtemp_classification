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
library(wateRshedTools)
library(hrbrthemes)

# setup some basemaps
mapbases <- c("Stamen.TonerLite","OpenTopoMap", "CartoDB.PositronNoLabels", "OpenStreetMap",
              "Esri.WorldImagery", "Esri.WorldTopoMap","Esri.WorldGrayCanvas"
)
# and set defaults
mapviewOptions(basemaps=mapbases)

# LOAD DATA ---------------------------------------------------------------

# clustering data and dams
load("output/models/agnes_k_groups_w_selected_dams.rda") # data_k_sf

# update/drop some cols
data_k_sf <- data_k_sf %>% 
  mutate(k_5_integer = as.integer(k_5)) %>% 
  select(-c(k_3, k_6, reviewer, completed_Y_N, notes, dist_to_dam_m, dist_to_dam_km, color)) %>% 
  select(k_5, k_5_integer, station_id:geometry)

dams_final <- readr::read_rds(path = "output/models/dams_final_selected_sf.rds")

# fix duplicates
dams_final <- dams_final %>% 
  distinct(.keep_all = TRUE) %>% # drops to 27 total dams
  select(-c(damname:inspdate)) %>% # drop empty cols
  mutate(lon = st_coordinates(.)[,1],
         lat = st_coordinates(.)[,2])
  

# LOAD INTERMEDIATE DATA FILES --------------------------------------------------

# If script has been run once (steps 01:06), you can start at 07 and load these data:
load("output/12_selected_nhd_mainstems_for_gages_us_ds.rda")
load("output/12_selected_nhd_mainstems_for_gages.rda")
usgs_segs <- readRDS("output/12_selected_gage_comids.rds")
dams_selected <- readRDS("output/12_selected_dam_comids.rds")

# * Make Custom Color Palette ----------------------------------------------

# assign color palette based on classifications:
thermCols <- data.frame(k5_group_id = c(1:5),
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
data_k_sf <- left_join(data_k_sf, thermCols, by=c("k_5_integer"="k5_group_id")) %>% 
  st_transform(4326)

# * Static Plot --------------------------------------------------------------------

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

# 01A: GET COMID FOR GAGES -----------------------------------------------------

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

# update one COMID for Oroville which seems to be not joining (or giving diff comid)
#station_id=FRA, COMID=7969421
# reassign to: COMID=7969423
usgs_segs <- usgs_segs %>% mutate(comid = replace(comid, station_id=="FRA", 7969423))

# save the USGS station COMIDs file:
write_rds(usgs_segs, path="output/12_selected_gage_comids.rds")
write_csv(usgs_segs, path="output/12_selected_gage_comids.csv")

# 02A: GET UPSTREAM FLOWLINES FROM GAGE ---------------------------------------------

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

# add direction to gage col
mainstems_us <- mainstems_us %>% 
  mutate(from_gage = "US")

rm(mainstems_flat_us, mainstemsUS)

# 03A: GET DOWNSTREAM FLOWLINES FROM GAGE -----------------------------------------

# get NHD segments downstream of selected USGS gages, 50 km buffer
mainstemsDS <- map(coms_list, ~navigate_nldi(nldi_feature = .x,
                                             mode="downstreamMain",
                                             distance_km = 50,
                                             data_source = ""))

# make a single flat layer
mainstems_flat_ds <- mainstemsDS %>%
  set_names(., usgs_segs$station_id) %>%
  map2(usgs_segs$station_id, ~mutate(.x, station_id=.y))


# bind together
mainstems_ds <- do.call(what = sf:::rbind.sf,
                        args = mainstems_flat_ds)

# add direction to gage col
mainstems_ds <- mainstems_ds %>% 
  mutate(from_gage = "DS")

rm(mainstems_flat_ds, mainstemsDS)

# bind all mainstems
mainstems_gage_all <- rbind(mainstems_us, mainstems_ds)

# quick check
mapview(dams_final, col.regions="black", cex=7) + 
  mapview(mainstems_us, color="darkblue", lwd=0.7) +
  mapview(mainstems_ds, color="purple", lwd=1.5) +
  mapview(data_k_sf, col.regions="orange", cex=4)

# 01B: GET COMID FOR DAMS -----------------------------------------------------

# get the COMID for each gage in list using nhdplustools
dam_segs <- dams_final %>% 
  split(.$NAME) %>% # split by ID#
  map(~discover_nhdplus_id(.x$geometry))

# now have a list of all COMIDs, check for duplicates
dam_segs %>% 
  purrr::map_lgl(~ length(.x)>1) %>% table() # all FALSE
# to look for TRUE try this: .[.==TRUE] 

# to see as a dataframe:
dam_segs <- unlist(dam_segs) %>% as_tibble(rownames = "dam_name") %>% 
  rename(comid = value)

# save the USGS station COMIDs file:
write_rds(dam_segs, path="output/12_selected_dam_comids.rds")
write_csv(dam_segs, path="output/12_selected_dam_comids.csv")

# 02B: GET UPSTREAM FLOWLINES FROM DAMS ---------------------------------------------

# use the list of comids to pass to the nhdplusTools function
dams_list <- map(dam_segs$comid, ~list(featureSource = "comid", featureID=.x))

# Get Upstream mainstem segs, this can take a minute
dmainstemsUS <- map(dams_list, ~navigate_nldi(nldi_feature = .x,
                                             mode="upstreamMain",
                                             #distance_km = 10,
                                             data_source = ""))

# make a single flat layer
dmainstems_flat_us <- dmainstemsUS %>%
  set_names(., dam_segs$dam_name) %>%
  map2(dam_segs$dam_name, ~mutate(.x, dam_name=.y))

# bind together
dmainstems_us <- do.call(what = sf:::rbind.sf,
                        args = dmainstems_flat_us)

# add direction to gage col
dmainstems_us <- dmainstems_us %>% 
  mutate(from_dam = "US")

rm(dmainstems_flat_us, dmainstemsUS)

# 03B: GET DOWNSTREAM FLOWLINES FROM DAMS -----------------------------------------

# get NHD segments downstream of selected USGS gages, 100 km buffer
dmainstemsDS <- map(dams_list, ~navigate_nldi(nldi_feature = .x,
                                             mode="downstreamMain",
                                             distance_km = 100,
                                             data_source = ""))

# make a single flat layer
dmainstems_flat_ds <- dmainstemsDS %>%
  set_names(., dam_segs$dam_name) %>%
  map2(dam_segs$dam_name, ~mutate(.x, dam_name=.y))


# bind together
dmainstems_ds <- do.call(what = sf:::rbind.sf,
                        args = dmainstems_flat_ds)

# add direction to gage col
dmainstems_ds <- dmainstems_ds %>% 
  mutate(from_dam = "DS")

rm(dmainstems_flat_ds, dmainstemsDS)

# bind all mainstems
mainstems_dam_all <- rbind(dmainstems_us, dmainstems_ds)

# quick check
mapview(dams_final, col.regions="black", cex=7) + 
  mapview(dmainstems_us, color="darkblue", lwd=0.7) +
  mapview(dmainstems_ds, color="purple", lwd=1.5) +
  mapview(data_k_sf, col.regions="orange", cex=4)


# 04: COMBINE AND SAVE GAGE/DAM STREAMLINES ------------------------------------------

save(mainstems_dam_all,mainstems_gage_all, file="output/12_selected_nhd_mainstems.rda")
save(mainstems_us, mainstems_ds, dmainstems_us, dmainstems_ds, file = "output/12_selected_nhd_mainstems_us_ds.rda")

# to write out as a shapefile:
#st_write(obj=mainstems_dams_all, "output/12_selected_nhd_mainstems_dams.shp")

# 05: SELECT NEAREST DAMS USING COMID ---------------------------------------

## search for dams nearest to a gage site that occur in same 
# comid/nhdline mainstem list. 

# join comids with dams
dams_final <- dams_final %>% left_join(., dam_segs, by=c("NAME"="dam_name"))

# first join comids with gage
data_k_sf <- data_k_sf %>% left_join(., usgs_segs)

# now select the mainstem dam comids that are in the list of gage_comids
k_coms_selected <- data_k_sf  %>% filter(comid %in% dmainstems_ds$nhdplus_comid)

# use this after selecting dams (to keep in same drainage), to figure out distances
#dams_nearest <- dams_final[st_nearest_feature(k_coms_selected, dams_final),]

# quick map
mapview(dams_final, col.region="black", cex=6) + 
  mapview(k_coms_selected, col.region="yellow", cex=3) +
  #mapview(data_k_sf, col.region="orange", cex=3) +
  mapview(dmainstems_ds, color="purple", lwd=1.5)


# 06. SNAP POINTS TO LINES ----------------------------------------------------

dmainstems_ds <- dmainstems_ds %>% st_transform(3310)

# can use st_nearest_points first, returns lines
pts_near_mainstem <- st_nearest_points(k_coms_selected, dmainstems_ds)

# function to snap points to lines using 1000 m buffer 
# (adapted from SO and Tim Salabim: https://stackoverflow.com/questions/51292952/snap-a-point-to-the-closest-point-on-a-line-segment-using-sf )
st_snap_points = function(x, y, namevar, max_dist = 1000) {
  
  if (inherits(x, "sf")) n = nrow(x)
  if (inherits(x, "sfc")) n = length(x)
  
  out = do.call(c,
                lapply(seq(n), function(i) {
                  nrst = st_nearest_points(st_geometry(x)[i], y)
                  nrst_len = st_length(nrst)
                  nrst_mn = which.min(nrst_len)
                  if (as.vector(nrst_len[nrst_mn]) > max_dist) return(st_geometry(x)[i])
                  return(st_cast(nrst[nrst_mn], "POINT")[2])
                })
  )
  out_xy <- st_coordinates(out) %>% as.data.frame()
  out_xy <- out_xy %>% 
    mutate({{namevar}} := x[[namevar]]) %>% 
    st_as_sf(coords=c("X","Y"), crs=st_crs(x), remove=FALSE)
  
  return(out_xy)
}

# now snap points to the lines
pts_sel <- st_snap_points(k_coms_selected, dmainstems_ds, namevar = "station_id", max_dist = 1000)


# check it out!
mapview(pts_sel, col.regions="yellow") + 
  mapview(dams_final, col.region="black", cex=6) + 
  mapview(data_k_sf, col.region="orange", cex=3) +
  mapview(dmainstems_ds, color="purple", lwd=1.5)

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

