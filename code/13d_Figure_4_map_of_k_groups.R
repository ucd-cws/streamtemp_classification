# Code description --------------------------------------------------------

# Figure 4: Map of study locations and k-5 clusters in CA.

# Libraries ---------------------------------------------------------------

library(sf)
library(tidyverse)
library(ggthemes)
library(ggrepel)
library(ggspatial)
#library(nhdplusTools)
#library(wateRshedTools)

# LOAD DATA ---------------------------------------------------------------

# clustering data and dams
load("output/models/agnes_k_groups_v2_w_selected_dams.rda")

# rename and transform for code below:
data_k_sf <- data_k_sf_v2 %>% st_transform(4326)
rm(data_k_sf_v2)

# dams in selected locations
dams_selected <- readRDS("output/12_selected_dam_comids.rds")


# * Check Thermal k=5 Classifications -------------------------------------------------

# Thermal Classifications
# "stable warm" = 1
# "reg warm"= 3, 
# "reg cool" = 4,
# "unreg cool"= 2, 
# "stable cold"= 5

levels(data_k_sf$k_5)

# * Make Custom Color Palette ----------------------------------------------

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

## this already part of dataset, but leaving for reference
# use "merge" to add to existing dataframe
#data_k_sf <- merge(data_k_sf, thermCols)

# SIMPLE BASE PLOT --------------------------------------------------------------------
# using basemap options

# plot CA counties
plot(USAboundaries::us_counties(resolution = "low", states="ca")$geometry, border = alpha("gray70", 0.8), lwd=0.9, lty=3)
# plot state boundary
plot(USAboundaries::us_boundaries(type="state", states="ca")$geometry, lwd=2, add=T)
plot(data_k_sf$geometry, pch=21, 
     bg=data_k_sf$color, add=T)
legend(x = 'bottomright', 
       legend = levels(data_k_sf$k_5),
       col = thermCols$color,
       pch = 16, bty = 'n', xjust = 1)

# FANCY GGPLOT MAP FOR k=5 --------------------------------------------------------------
# use these symbols:15, 16, 17, 18, 8


(map1 <- ggplot()+
   #add state boundary
   geom_sf(data=USAboundaries::us_boundaries(type="state", states="ca")$geometry, 
           fill = NA, color = 'slategray4', size = 1, alpha = 0.4) +
   # add county boundaries
   geom_sf(data=USAboundaries::us_counties(states="ca")$geometry, 
           fill = NA, color = 'slategray4', size = 0.3, lty=2, alpha = 0.4) +
   # add k groups/points
   geom_sf(data = data_k_sf, aes(color = k_5, shape=k_5), size = 4) +
   # add north arrow and scale bar
   annotation_north_arrow(location="tr", 
                          pad_x = unit(1.5, "cm"), 
                          pad_y = unit(0.5, "cm")) +
   annotation_scale(location="br") +
   scale_color_manual("Thermal \nClasses", values=thermCols$color) +
   scale_shape_manual("Thermal \nClasses", values=c(15,16,17,18,8)) +
   theme_map(base_family = "Roboto Condensed", base_size = 15) +
   theme(legend.background = element_rect(fill = NA)))

ggsave(filename="output/figures/Fig_4_classification_map.jpg", dpi=300, width=8, height = 11.5, units="in")

