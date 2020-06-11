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
load("output/12_data_k_centdist_damdist.rda")

# dams in selected locations
dams_selected <- readRDS("output/12_selected_dam_comids.rds")


# * Check Thermal k=5 Classifications -------------------------------------------------

# Thermal Classifications
# "stable warm" = 1
# "variable warm"= 3, 
# "stable cool" = 4,
# "variable cool"= 2, 
# "stable cold"= 5

levels(data_k_dist$k5_names)

# * Make Custom Color Palette ----------------------------------------------

# make a custom set of colors in a dataframe
thermCols <- with(data_k_dist,
                  data.frame(k_5 = levels(k5_names),
                             color = I(c("#E41A1C", #stable warm
                                         "#FF7F00", #variable warm
                                         "#984EA3", #stable cool
                                         "#4DAF4A", #variable cool
                                         "#377EB8" #stable cold
                                       ))))
thermCols

# SIMPLE BASE PLOT --------------------------------------------------------------------
# using basemap options

# plot CA counties
plot(USAboundaries::us_counties(resolution = "low", states="ca")$geometry, border = alpha("gray70", 0.8), lwd=0.9, lty=3)
# plot state boundary
plot(USAboundaries::us_boundaries(type="state", states="ca")$geometry, lwd=2, add=T)
plot(data_k_dist$geometry, pch=21, 
     bg=data_k_dist$color, add=T)
legend(x = 'topright', 
       legend = levels(data_k_dist$k5_names),
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
   geom_sf(data = data_k_dist, aes(color = k5_names, shape=k5_names), size = 4) +
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

