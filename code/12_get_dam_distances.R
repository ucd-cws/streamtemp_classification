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

# Load data ---------------------------------------------------------------

# clustering data and dams
load("output/models/agnes_k_groups_sf_w_dams.rda")

# Thermal Classifications
# "stable warm" = 1
# "reg warm"= 3, 
# "reg cool" = 4,
# "unreg cool"= 2, 
# "stable cold"= 5, 

# Reorder Classifications -------------------------------------------------

# rearrange levels to match color palette:
data_k_sf <- data_k_sf %>% 
  mutate(k_5 = forcats::fct_relevel(k_5, "stable warm", "reg warm", "reg cool", "unreg cool", "stable cold"))

levels(data_k_sf$k_5)


#  Make Custom Color Palette ----------------------------------------------

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

# Plot --------------------------------------------------------------------
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

# GET COMID FOR GAGES -----------------------------------------------------

library(nhdplusTools)

# get the COMID for each gage in list
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

# GET UPSTREAM FLOWLINES --------------------------------------------------

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

# GET DOWNSTREAM FLOWLINES ------------------------------------------------

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

## now use this to search for dams nearest to the site that occur in same comid/nhdline list. Then calc distance via segments.
# see here as well: https://stackoverflow.com/questions/57116416/snap-points-to-line-in-order-in-r

# * SAVE OUT STREAMLINES FOR GAGES ------------------------------------------

save(mainstems_all, file="output/12_selected_nhd_mainstems_for_gages.rda")

save(mainstems_us, mainstems_ds, file = "output/12_selected_nhd_mainstems_for_gages_us_ds.rda")

# Get Nearest Dams --------------------------------------------------------

# need to make sure this is in same CRS...
# transform to UTM for more accurate grid
# using 3310
dams <- st_transform(dams, 3310)
data_k_sf <- st_transform(data_k_sf, 3310)

# note this is irrespective of watershed!
dams_nearest <- dams[st_nearest_feature(data_k_sf, dams),]

# get distance to a given point:
data_k_sf <- data_k_sf %>% 
  mutate(
    dist_to_dam_m = as.vector(st_distance(dams_nearest, ., 
                                          by_element = TRUE)),
    dist_to_dam_km = dist_to_dam_m/1000)


# Mapview -----------------------------------------------------------------

# setup some basemaps
mapbases <- c("Stamen.TonerLite","OpenTopoMap", "CartoDB.PositronNoLabels", "OpenStreetMap",
              "Esri.WorldImagery", "Esri.WorldTopoMap","Esri.WorldGrayCanvas"
)
mapviewOptions(basemaps=mapbases)

# map k5
m5 <- mapview(dams_nearest, col.regions="black", alpha.regions=0.8,
                layer.name="Dams", cex=2,
                hide=TRUE, homebutton=FALSE)+
  mapview(data_k_sf,  zcol="k_5", map.types=mapbases,
          col.regions=unique(data_k_sf$color), 
          burst=TRUE, hide=FALSE, homebutton=FALSE)
  
m5@map %>% leaflet::addMeasure(primaryLengthUnit = "meters")

# Static map for k5 --------------------------------------------------------------

ggplot()+
  geom_sf(data=USAboundaries::us_boundaries(type="state", states="ca")$geometry, fill = NA, color = 'slategray4', size = 1, alpha = 0.4) +
  geom_sf(data=USAboundaries::us_counties(states="ca")$geometry, fill = NA, color = 'slategray4', size = 0.3, lty=2, alpha = 0.4) +
  
  geom_sf(data = data_k_sf, aes(fill = k_5), pch = 21, size = 3) +
  annotation_north_arrow(location = "tr", pad_y = unit(0.1, "cm")) +
  theme_map(base_family = "Roboto Condensed", base_size = 14)+
  scale_fill_manual("Thermal \nClasses", values=thermCols$color) +
  annotation_scale(location="br")

ggsave(filename="output/figures/agnes_k_5_classification_map.png", dpi=300, width=8, height = 11.5, units="in")




# Snap Points to River



# buffer the points by the tolerance
points_buf <- st_buffer(points, 15)
# intersect the line with the buffer
line_intersect <- st_intersection(line, points_buf)

# Calc Centroid Distances -------------------------------------------------
