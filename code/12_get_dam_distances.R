# Code description --------------------------------------------------------

# this gets streamlines and dams, snaps points to streamlines and calculates distance to nearest dam

# Libraries ---------------------------------------------------------------

# mapping
library(sf)
library(mapview)
library(nhdplusTools)
library(ggspatial)
#library(wateRshedTools)
# wrangling/plotting
library(dplyr)
library(ggplot2)
library(purrr)
library(readr)
library(tidylog)
library(ggthemes)
library(hrbrthemes)

options(scipen=999)

# 1. LOAD DATA ------------------------------------------------------------

# clustering data and dams
load("output/11a_agnes_k_5_final_w_centdist.rda") # data_k_sf

# update/rename some cols
data_k_sf <- data_k_sf %>% 
  rename(cent_x = x, cent_y = y)

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

# 1X. LOAD INTERMEDIATE DATA FILES -----------------------------------------

# If script has been run once (steps 03-05), you can start at 06 and load these data:
dams_segs <- readRDS("output/12_selected_dam_comids.rds")
usgs_segs <- readRDS("output/12_selected_gage_comids.rds")

load("output/12_selected_nhd_dam_mainstems_us_ds.rda")


# 2. SET UP MAPPING -------------------------------------------------------

# * Make Custom Color Palette ----------------------------------------------

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

# * Static Plot --------------------------------------------------------------------

# using baseplot options here

# plot CA counties
plot(USAboundaries::us_counties(resolution = "low", states="ca")$geometry, border = alpha("gray70", 0.8), lwd=0.9, lty=3)
# plot state boundary
plot(USAboundaries::us_boundaries(type="state", states="ca")$geometry, lwd=2, add=T)
# plot data
plot(data_k_sf$geometry, pch=21, 
     bg=data_k_sf$color, add=T)
plot(dams_final$geometry, add=T, pch=24, bg=alpha("black", 0.8))
legend(x = 'bottomright', 
       legend = as.character(thermCols$k5_names),
       col = thermCols$color,
       pch = 16, bty = 'n', xjust = 1)


# 03A: GET COMID FOR GAGES -----------------------------------------------------

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

# 03B: GET UPSTREAM FLOWLINES FROM GAGE ---------------------------------------------

# use the list of comids to pass to the nhdplusTools function
usgs_list <- map(usgs_segs$comid, ~list(featureSource = "comid", featureID=.x))

# Get Upstream mainstem segs, this can take a minute
mainstemsUS <- map(usgs_list, ~navigate_nldi(nldi_feature = .x,
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

# 03C: GET DOWNSTREAM FLOWLINES FROM GAGE -----------------------------------------

# get NHD segments downstream of selected USGS gages, 50 km buffer
mainstemsDS <- map(usgs_list, ~navigate_nldi(nldi_feature = .x,
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

# 04A: GET COMID FOR DAMS -----------------------------------------------------

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

# 04B: GET UPSTREAM FLOWLINES FROM DAMS ---------------------------------------------

# read in file from 4A above
dam_segs <- read_rds("output/12_selected_dam_comids.rds")

# use the list of comids to pass to the nhdplusTools function
dams_list <- map(dam_segs$comid, ~list(featureSource = "comid", featureID=.x))

# Get Upstream mainstem segs, this can take a minute
dmainstemsUS <- map(dams_list, ~navigate_nldi(nldi_feature = .x,
                                             mode="upstreamMain",
                                             distance_km = 50,
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
# save temp:
save(dmainstems_us, file="output/12_selected_nhd_dam_mainstems_us.rda")
load("output/12_selected_nhd_dam_mainstems_us.rda")

# 04C: GET DOWNSTREAM FLOWLINES FROM DAMS -----------------------------------------

dam_segs <- read_rds("output/12_selected_dam_comids.rds")

# use the list of comids to pass to the nhdplusTools function
dams_list <- map(dam_segs$comid, ~list(featureSource = "comid", featureID=.x))


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

# save
save(dmainstems_ds, file="output/12_selected_nhd_dam_mainstems_ds.rda")

(m_ds <- mapview(data_k_sf, col.regions="yellow") + mapview(dams_final, col.regions="black") + mapview(dmainstems_ds, layer.name="DS Mainstems"))


# extend line for big springs from streamline to dam:
library(mapedit); library(leaflet); library(leafem)

# select bigspr
bigspr_dam <- dmainstems_ds %>% filter(dam_name=="Big Springs Dam")

# editFeatures returns sf dataframe of the original!!
bigspr_ext <- mapedit::editFeatures(bigspr_dam, map=m_ds, editor="leafpm")

# fix fields
bigspr_ext <- bigspr_ext %>%  
  mutate(
    dam_name = case_when(
      is.na(dam_name) ~ "Big Springs Dam",
      TRUE ~ dam_name),
    from_dam = case_when(
      is.na(from_dam) ~ "DS",
      TRUE ~ from_dam)) %>%
  select(-c(X_leaflet_id, layerId))

# clip out big springs dam from everything
dmainstems_ds_clipped <- dmainstems_ds %>% filter(dam_name!="Big Springs Dam")

# rejoin
dmainstems_ds_rev <- st_as_sf(data.table::rbindlist(
  list(dmainstems_ds_clipped, bigspr_ext), fill = TRUE))

# check class
unique(st_geometry_type(dmainstems_ds_rev))

# remap
#mapview(dmainstems_ds_rev) + mapview(bigspr_ext, color="red") + mapview(dams_final, col.region="black") +mapview(data_k_sf, col.regions="yellow")

# bind all mainstems
mainstems_dam_all <- rbind(dmainstems_us, dmainstems_ds_rev)

# quick check
mapview(dams_final, col.regions="black", cex=7) + 
  mapview(dmainstems_us, color="darkblue", lwd=0.7) +
  mapview(dmainstems_ds_rev, color="purple", lwd=1.5) +
  mapview(data_k_sf, col.regions="orange", cex=4)

# 05: COMBINE AND SAVE GAGE/DAM STREAMLINES ------------------------------------------

#save(mainstems_gage_all, file="output/12_selected_nhd_gage_mainstems.rda")
save(mainstems_dam_all, file="output/12_selected_nhd_dam_mainstems.rda")
save(dmainstems_us, dmainstems_ds_rev, file = "output/12_selected_nhd_dam_mainstems_us_ds.rda")

# 06: SELECT NEAREST DAMS USING COMID ---------------------------------------

## search for dams nearest to a gage site that occur in same 
# comid/nhdline mainstem list. 

# load intermediate files:
load("output/12_selected_nhd_dam_mainstems_us_ds.rda")

# join comids with dams
dams_final <- dams_final %>% left_join(., dams_segs, by=c("NAME"="dam_name"))

# first join comids with gage
data_k_sf <- data_k_sf %>% left_join(., usgs_segs)

# now select the COMID for segments from the gages dataset that exist in mainstem dam comids
k_coms_selected <- data_k_sf %>% filter(comid %in% dmainstems_ds_rev$nhdplus_comid)

# quick map
mapview(dams_final, col.region="black", cex=6) + 
  mapview(k_coms_selected, col.region="yellow", cex=4) +
  mapview(data_k_sf, col.region="orange", cex=1) +
  mapview(dmainstems_ds_rev, color="purple", lwd=1.5)

# 07. SNAP POINTS TO LINES ----------------------------------------------------

dmainstems_ds <- dmainstems_ds_rev %>% st_transform(3310)
k_coms_selected <- st_transform(k_coms_selected, 3310)
dams_final <- st_transform(dams_final, 3310)

# function to snap points to lines using 1000 m buffer 
# (adapted from SO and Tim Salabim: https://stackoverflow.com/questions/51292952/snap-a-point-to-the-closest-point-on-a-line-segment-using-sf )
st_snap_points <- function(x, y, namevar, max_dist = 1000) {
  
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
pts_snapped <- st_snap_points(k_coms_selected, dmainstems_ds, namevar = "station_id", max_dist = 500)

dams_snapped <- st_snap_points(dams_final, dmainstems_ds, namevar="NAME", max_dist=500)

# check it out!
mapview(pts_snapped, col.regions="yellow", layer.name="Snapped Gages") + 
  mapview(dams_final, col.region="black", cex=6, layer.name="Dams", homebutton=FALSE) +
  mapview(dams_snapped, col.region="gray40", cex=6, layer.name="Dams_snap", homebutton=FALSE) +
  mapview(data_k_sf, col.region="orange", cex=3, layer.name="Original Gages", homebutton=FALSE) +
  mapview(dmainstems_ds, color="purple", lwd=1.5, layer.name="Mainstems", homebutton=FALSE)

### IMPORTANT NOTE ###
# there is a known issue with precision that effects the ability for snapping points to lines...
# see: https://github.com/r-spatial/sf/issues/790, and here: https://gis.stackexchange.com/questions/288570/find-nearest-point-along-polyline-using-sf-package-in-r

# the easiest work around is to give a very small buffer from the points/lines of interest
# and set the precision. 

## try snapping/intersect to see if this works with a single point/line
# check with a single value:
# tst_pt <- pts_snapped %>% filter(station_id=="SCQ") %>%  
#   # use a 1 cm buffer
#   st_buffer(0.1) #%>% st_set_precision(precision = 1e3)
# tst_ln <- dmainstems_ds %>% filter(dam_name=="Success") %>% 
#   group_by(dam_name) %>% summarize()
# 
# # use this to assess if the point/line are intersecting and how far apart if not:
# cat("Line intersects:", st_intersects(tst_pt, tst_ln, sparse=FALSE), "\nDistance between:", st_distance(tst_pt, tst_ln))
# 
# # quick map
# mapview(tst_ln) + mapview(pts_snapped, cex=4) +
#   mapview(tst_pt, color="orange3", col.region="orange", cex=10, lwd=5)

# intermediate_temp data save
#save(k_coms_selected, pts_snapped, data_k_sf, dams_final, dams_segs, dams_snapped, usgs_segs, file = "output/12_gage_dams_dist_data.rda")

# 08A. MERGE RIVER SEGMENTS AND DAMS/GAGE POINTS --------------------------

library(lwgeom)

# need to merge the dams and snapped gage pts in to one dataset
dams_abb <- dams_final %>% 
  select(NAME, geometry) %>% 
  mutate(type="dam") %>% 
  st_transform(3310) 

# just gages snapped
gage_abb <- pts_snapped %>% select(station_id, geometry) %>% mutate(type="gage") %>% 
  rename(NAME=station_id) %>% st_transform(3310)

# bind together into one dataset
pts_abb <- rbind(dams_abb, gage_abb) 

#  merge mainstem streams into one line per "dam"
ds_main_merged <- dmainstems_ds %>% 
  group_by(dam_name) %>% 
  summarize(segs=n()) %>%
  st_line_merge() # use this to flatten everything to a single segment

#unique(st_geometry_type(dmainstems_ds_rev))
unique(st_geometry_type(ds_main_merged))

# need to fix to all linestrings
ds_main_merged <- do.call(rbind,lapply(1:nrow(ds_main_merged),function(i){st_cast(ds_main_merged[i,],"LINESTRING")}))
  
# quick check
mapview(ds_main_merged, zcol="dam_name", legend=FALSE) + 
   mapview(pts_abb, zcol="type")

# check for intersect at a 1m buffer (should see some TRUE's)
sf::st_intersects(ds_main_merged, st_buffer(pts_abb, 1), sparse=FALSE)

# 08B. SPLIT LINES BY POINTS AND CALC DISTANCE ------------------------------

# now split lines by points
segs <- st_collection_extract(lwgeom::st_split(ds_main_merged, st_buffer(pts_abb, 1)), "LINESTRING") %>% 
  mutate(seg_len_m = units::drop_units(units::set_units(st_length(.), "m")),
         seg_len_km = seg_len_m/1000) %>% 
  # need to drop some spatial fragments (lengths less than 3m)
  filter(seg_len_m>3) %>% 
  arrange(dam_name) %>% 
  tibble::rownames_to_column(var = "rowid") %>% 
  mutate(rowid=as.integer(rowid))

# it worked!
pal <- mapviewPalette("mapviewRasterColors")

# filter out the most downstream segment ("loose ends")
# by removing the segment that has the largest cumulative length for a given dam "group"
segs_filt <- segs %>% 
  # drop this seg:
  filter(rowid!=3) %>% 
  group_by(dam_name) %>% arrange(rowid) %>% 
  mutate(cum_len_km = cumsum(seg_len_km)) %>% 
  filter(cum_len_km < max(cum_len_km)) 

# preview
mapview(gage_abb, col.regions="yellow", 
        layer.name="Snapped Gages", cex=4, basemaps=mapbases) + 
  mapview(dams_final, col.region="black", 
          cex=6, layer.name="Dams", homebutton=FALSE) + 
  mapview(segs_filt, zcol="rowid", color=pal(5), lwd=3, 
          legend=FALSE, layer.name="Mainstems", homebutton=FALSE)+
  mapview(segs, zcol="rowid", color=pal(5), lwd=1, 
          legend=FALSE, layer.name="Mainstems", homebutton=FALSE)

# save segments layer
save(segs, segs_filt, file = 'output/12_mainstem_segs_w_distances_from_dams_to_gage_pts.rda')

# save points layers
selected_gages_dams <- pts_abb
save(selected_gages_dams, file = "output/12_selected_gages_w_dams.rda")


# 09. SPATIAL JOIN SELECTED PTS TO CUMULATIVE DIST LINES -------------------------

# Spatial join k_points to lines to get distances
# left_join based on a 10m buffer and convert back to point w st_centroid
gages_w_distances <- st_join(st_buffer(gage_abb, 10), left=TRUE, segs_filt) %>% st_centroid() %>%
  # then group by and pick the lowest "rowid" to ensure the "U/S" segment used for cumul. distance
  group_by(NAME, dam_name) %>%
  filter(rank(rowid)==1) %>% 
  rename(station_id = NAME)

# how many duplicates?
st_drop_geometry(gages_w_distances) %>% group_by(station_id) %>% tally() %>% filter(n>1) %>% pull(station_id)
# dup sites
dup_sites <- c("BSF", "CCR", "DGC", "JLF", "KWK", "NFH", "SHD", "TLK")

# these are the sites to keep (no filtering needed)
single_sites <- setdiff(unique(gages_w_distances$station_id), dup_sites)

# need to drop/assigne gages to a dam b/c assoc with multiple mainstems
# SHASTA = c("SHD", "KWK", "CCR", "BSF", "JLF")
# LEWISTON = c("TLK", "DGC", "NFH")
# filter
gages_w_dist_filt <- gages_w_distances %>% 
  filter(
    # filter single sites
    (station_id %in% single_sites) |
      # filter shasta sites
      (station_id %in% c("SHD", "KWK", "CCR", "BSF", "JLF") & dam_name=="Shasta") |
      # filter lewiston sites
      (station_id %in% c("TLK", "DGC", "NFH") & dam_name=="Lewiston")) %>% 
  # now clean and tidy for join
  select(station_id, dam_name, seg_len_km, cum_len_km) 


# join this back to the original k classifications
data_k_dist <- data_k_sf %>% 
  left_join(., st_drop_geometry(gages_w_dist_filt), by="station_id")

# mapview
mfinal <- mapview(data_k_dist, zcol="k5_names",
                  layer.name="Distance to Dam",
                  legend=FALSE, cex="cum_len_km", basemaps=mapbases,
                  col.regions=unique(data_k_dist$color[order(data_k_dist$k5_names)])) + 
  mapview(dams_final, col.region="black", 
          cex=6, layer.name="Dams", homebutton=FALSE) + 
  mapview(ds_main_merged, col.region="darkblue", lwd=1.5,
          homebutton=FALSE, legend=FALSE, layer.name="Rivers DS") +
  
  mapview(data_k_dist,  zcol="k5_names", map.types=mapbases,
          layer.name="Thermal Classes",
          col.regions=unique(data_k_dist$color[order(data_k_dist$k5_names)]), 
          alpha.regions=0.8, cex=3.5,
          hide=FALSE, homebutton=FALSE)

mfinal@map %>% leaflet::addMeasure(primaryLengthUnit = "meters")

# save this out!
save(data_k_dist, file = "output/12_data_k_centdist_damdist.rda")


# 10: GET EUCLIDEAN DISTANCE TO NEAREST DAM ---------------------------------

# this is old and less useful now
# all objects should be transformed to UTM 
# for more accurate grid using 3310
# data_k_sf <- st_transform(data_k_sf, 3310)
# 
# # get distance to nearest DAM from GAGE:
# # this gives a matrix of each dam and each gage
# dam_dist_matrix <- st_distance(dams_abb, gage_abb, 
#             by_element = FALSE) %>% units::set_units("km")
# 
# # get min of every col and add to gage_abb?
# gage_abb$euclid_dist_km <- apply(dam_dist_matrix,2,min)
# 
# # this gets a measurement for each gage but....not sure about it
# gages_dam_dist <- gage_abb %>% 
#   mutate(
#     dist_to_dam_m = as.vector(st_distance(dams_abb, ., 
#                                           by_element = TRUE)),
#     dist_to_dam_km = dist_to_dam_m/1000) %>% 
#   # now drop sites that don't have dams or 
# 
# 
# save(data_k_sf, file = "output/models/agnes_k_groups_w_selected_dams_v2.rda")

# 11: Look at Distances by Group -----------------------------------------

load("output/12_data_k_centdist_damdist.rda")

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

# get group counts
data_k_dist %>% st_drop_geometry %>% 
  group_by(k_5, k5_names) %>% tally() %>% 
  left_join(., thermCols) -> grp_cnts

# now look at mean dist to dam per group
data_k_dist %>% st_drop_geometry %>% 
  group_by(k_5) %>%  
  summarize(mean_dam_dist_km = mean(cum_len_km, na.rm=TRUE)) %>%
  left_join(grp_cnts) %>%
  filter(!is.na(mean_dam_dist_km)) %>% #iew()
  ggplot() + geom_col(aes(x= k5_names, y=mean_dam_dist_km, fill=color), alpha=0.8) + hrbrthemes::theme_ft_rc() +
  labs(x="Thermal Class", y="Mean Distance to U/S Dam (km)")

ggsave("output/figures/12_mean_dist_to_dam_by_k5.png", width = 8, height = 6, units = "in", dpi = 300)

# filter out to just reg warm/reg cool
data_k_dist %>% st_drop_geometry() %>% 
  filter(k5_names %in% c("2-variable warm", "3-stable cool")) %>%
  group_by(k_5, k5_names) %>% 
  ggplot() + geom_histogram(aes(x=cum_len_km, fill=color), 
                            binwidth = 3, bins = 50) +
  facet_wrap(k5_names~.) + theme_clean()

ggsave("output/figures/12_hist_dist_to_dam_by_stable_cool_variable_warm.png", width = 8, height = 6, units = "in", dpi = 300)

# 09: STATIC MAP FOR k=5 --------------------------------------------------------------

ggplot()+
  geom_sf(data=USAboundaries::us_boundaries(type="state", states="ca")$geometry, fill = NA, color = 'slategray4', size = 1, alpha = 0.4) +
  geom_sf(data=USAboundaries::us_counties(states="ca")$geometry, fill = NA, color = 'slategray4', size = 0.3, lty=2, alpha = 0.4) +
  
  geom_sf(data = data_k_dist, aes(fill = k5_names), pch = 21, size = 3) +
  annotation_north_arrow(location = "tr", pad_y = unit(0.1, "cm")) +
  theme_map(base_family = "Roboto Condensed", base_size = 14)+
  scale_fill_manual("Thermal \nClasses", values=thermCols$color) +
  annotation_scale(location="br")

ggsave(filename="output/figures/12_agnes_k_5_classification_map.png", dpi=300, width=8, height = 11.5, units="in")

