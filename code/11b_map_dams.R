
# Code description --------------------------------------------------------

# Take selected dams and finalize

# Libraries ---------------------------------------------------------------

library(mapview)
library(sf)
library(tidyverse)
library(ggthemes)
library(ggspatial)


# Load data ---------------------------------------------------------------

# the k groups data 
load("output/11a_agnes_k_5_final_w_centdist.rda") # data_k_sf

# the filtered dams data
load("output/11a_dams_nearest_final.rda")

# get rivers
load("output/12_selected_nhd_mainstems_for_gages.rda")

# all dams CA/OR
dams <- read_sf("data/shps/CA_OR_dams.shp", quiet = F) %>% st_transform(4326)

# clip to only dams in CA
ca <- USAboundaries::us_boundaries(type="state", states="ca")
ca_co <- USAboundaries::us_counties(states = "ca", resolution = "low")
dams <- dams[ca,] # now clip (spatial join to only ca)

# make spatial bases ---------------------------------------------------------

# setup some basemaps
mapbases <- c("Stamen.TonerLite","OpenTopoMap", "CartoDB.PositronNoLabels", "OpenStreetMap",
              "Esri.WorldImagery", "Esri.WorldTopoMap","Esri.WorldGrayCanvas"
)
mapviewOptions(basemaps=mapbases)

# map k5
m5 <- mapview(dams_nearest_final, col.regions="black",
                layer.name="Selected Dams", cex=6,
                hide=TRUE, homebutton=FALSE)+
  mapview(dams, col.regions="gray50", alpha.regions=0.5, cex=3.4, layer.name="All Dams") +
  mapview(mainstems_all, color="steelblue", cex=3, 
          layer.name="NHD Flowlines") +
  mapview(data_k_sf,  zcol="k5_names", map.types=mapbases,
          layer.name="Thermal Classes",
          col.regions=unique(data_k_sf$color[order(data_k_sf$k5_names)]), 
          alpha.regions=0.8, cex=3.5,
          hide=FALSE, homebutton=FALSE) 

m5@map %>% leaflet::addMeasure(primaryLengthUnit = "meters")


# Static map for k5 --------------------------------------------------------------

ggplot()+
  geom_sf(data=ca) + 
  geom_sf(data=ca_co, color="gray80", lwd=.5, lty=2)+
  geom_sf(data = dams_nearest_final, fill = 'gray10', pch=24, 
          size = 2, alpha = 0.8) +
  geom_sf(data = data_k_sf, aes(fill = k_5), pch = 21, size = 1) +
  annotation_north_arrow(location = "tr", pad_y = unit(0.1, "cm"), width = unit(0.7, "cm"), height = unit(0.8, "cm")) +
  annotation_scale() +
  theme_map()+
  theme(legend.position = c(0.7, 0.6), 
        legend.background = element_rect(fill = "transparent"))

