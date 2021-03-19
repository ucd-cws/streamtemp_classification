# Make Map Panels
# requires 13d_Figure_5_map_of_k_group outputs:


# Libraries ---------------------------------------------------------------


library(sf)
library(tidyverse)
library(ggthemes)
library(ggspatial)
library(mapview)
# setup some basemaps
mapbases <- c("Stamen.TonerLite", 
              "CartoDB.PositronNoLabels", "OpenStreetMap",
              "Esri.WorldImagery", "Esri.WorldTopoMap")
# and set defaults
mapviewOptions(basemaps=mapbases, fgb = FALSE)
library(purrr)
library(ggrepel)

# Load Data ---------------------------------------------------------------

load("output/13d_main_map_data_pieces.rda")


# Set Defaults ------------------------------------------------------------

scalesizes <- scale_size_binned("Centroid \nDistance", limits = c(0,45), 
                                n.breaks = 3, breaks = c(5, 10, 40),  range = c(2,10),
                                guide=guide_legend(order=2))

# plos requirements are TIFF at 300+ DPI:
# Width: 789 – 2250 pixels (at 300 dpi). Height maximum: 2625 pixels (at 300 dpi).
# https://journals.plos.org/plosone/s/figures

# PANEL E: CENTROID DIST MAP -----------------------------------------------------

# see types here:
# rosm::osm.types() osm, hillshade, cartolight,

# make a bounding box in lat/lon
(mapRange1 <- c(range(st_coordinates(panel_areas[1,])[,1]),range(st_coordinates(panel_areas[1,])[,2])))

# crop everything by panel_area 1 (E)
rivs_E <- st_intersection(rivs, panel_areas[1,]$geometry)
mainstems_gage_all_E <- st_intersection(mainstems_gage_all, panel_areas[1,]$geometry)
dams_final_E <- st_crop(dams_final, panel_areas[1,]$geometry)
data_k_dist_E <- st_crop(data_k_dist, panel_areas[1,]$geometry)

# use these symbols:15, 16, 17, 18, 8
(map_pan_E <- ggplot() +
    annotation_map_tile(data=mainstems_gage_all_E, type = "hillshade", cachedir = system.file("rosm.cache", package = "ggspatial"), forcedownload = F) +
    #annotation_map_tile(data=rivs_E, zoomin = 0, type = "hikebike", cachedir = system.file("rosm.cache", package = "ggspatial"), forcedownload = TRUE) +
    geom_sf(data=rivs_E, lwd=0.3, color="dodgerblue", show.legend = FALSE, alpha=0.6, inherit.aes = FALSE) +
    
    geom_sf(data=mainstems_gage_all_E, lwd=0.3, color="dodgerblue", show.legend = FALSE, alpha=0.9, inherit.aes = FALSE) + 
    
    geom_sf(data=dams_final_E, fill="black", pch=25, size=4.5, alpha=0.75, inherit.aes = FALSE) +
    #geom_sf(data = data_k_dist_E, aes(fill = k5_names, size=dist_to_centroid, shape=k5_names), size = 3.5, alpha=0.97) +
    geom_text_repel(data=data_k_dist_E,
                    aes(x=lon, y=lat, label=station_id), inherit.aes = FALSE,
                    fontface="bold", family="Roboto Condensed",
                    segment.color="gray10",
                    color = "white",     # text color
                    bg.color = "grey10", # shadow color
                    bg.r = 0.15,
                    size=4.5,
                    point.padding = 0.1,
                    box.padding = 0.1,
                    force=1,
                    nudge_x = 0.03,
                    min.segment.length = 0.9,
                    segment.inflect = FALSE,
                    #segment.square=FALSE,
                    segment.ncp = 3,
                    segment.curvature = -0.1) +
    geom_sf(data = data_k_dist_E %>% filter(k5_names %in% c("2-variable warm")), 
            aes(size=dist_to_centroid, fill=k5_names, shape=k5_names), color="black", 
            alpha=0.97, show.legend = TRUE, inherit.aes = FALSE) +
    # use custom scales
    scalesizes +
    scale_shape_manual("Thermal \nClasses", 
                       #values=c(22, 21, 24, 23, 8),
                       values=c(21),
                       guide=guide_legend(order=1)) +
    scale_fill_manual("Thermal \nClasses", 
                      values=thermCols$color[2],
                      guide=guide_legend(override.aes = list(size=4), order=1)) +
    
    #add north arrow and scale bar
    annotation_north_arrow(location="br", 
                           width = unit(0.8,"cm"),
                           height=unit(1.2, "cm"),
                           pad_y = unit(0.9, "cm")) +
    annotation_scale(location="br") +
    
    # add themes and sizes
    theme_map(base_family = "Roboto Condensed", base_size = 8) +
    theme(legend.background = element_rect(fill = NA),
          #plot.background = element_rect(color="black", size = 0.5),
          legend.box.background = element_rect(fill="white"),
          legend.key = element_rect(color = NA, size=NA),
          legend.position = c(0.05, 0.5), 
          legend.spacing.y = unit(0.1,"cm"),
          legend.margin = margin(0.15, 0.1, 0.1, 0.2, "cm"))
)

# save plot
save(map_pan_E, file="output/figures/PLOS_ONE/figure_map_panel_E.rda")

# save
ggsave(filename="output/figures/PLOS_ONE/Fig5_E_classification_map_cent_dist.pdf", dpi=300, width=8, height = 6, units="in", device = cairo_pdf)
ggsave(filename="output/figures/PLOS_ONE/Fig5_E_classification_map_cent_dist.tiff", dpi=300, 
       width=5.2, height = 3, units="in", scale = 1.3)

  # PANEL D: CENTROID DIST MAP -----------------------------------------------------

# panel 2 includes:
sites2 <- c("GRF", "DNB", "H41", "FWQ")
pan2 <- data_k_dist %>% filter(station_id %in% sites2)

# make a bounding box in lat/lon
(mapRange2 <- c(range(st_coordinates(pan2)[,1]),range(st_coordinates(pan2)[,2])) + 
    # add slight buffer around sites
    c(-0.05, .075, -0.05, 0.075))
# make polygon
pan2_bbx <- st_as_sfc(st_bbox(c(xmin = mapRange2[1], xmax = mapRange2[2], ymax = mapRange2[4], ymin = mapRange2[3]), crs = st_crs(4326)))

# crop everything by panel_area 1 (E)
rivs_D <- st_intersection(rivs, pan2_bbx)
mainstems_gage_all_D <- st_intersection(mainstems_gage_all, pan2_bbx)
dams_final_D <- st_crop(dams_final, pan2_bbx)
data_k_dist_D <- st_crop(data_k_dist, pan2_bbx)

# use these symbols:15, 16, 17, 18, 8
(map_pan_D <- ggplot() +
    #annotation_map_tile(data=pan2_bbx, type = "osm", cachedir = system.file("rosm.cache", package = "ggspatial"), forcedownload = F, alpha=0.9) +
    annotation_map_tile(data=pan2_bbx, type = "hillshade", cachedir = system.file("rosm.cache", package = "ggspatial"), forcedownload = FALSE, alpha=0.9) +
    
    geom_sf(data=rivs_D, lwd=0.3, color="dodgerblue", show.legend = FALSE, alpha=0.6, inherit.aes = FALSE) +
    
    geom_sf(data=mainstems_gage_all_D, lwd=0.3, color="dodgerblue", show.legend = FALSE, alpha=0.9, inherit.aes = FALSE) + 
    #geom_sf(data=ca, fill = NA, color = 'slategray4', size = 0.7, alpha = 0.3, inherit.aes = FALSE) +
    geom_sf(data=dams_final_D, fill="black", pch=25, size=4.5, alpha=0.75, inherit.aes = FALSE) +
    #geom_sf(data = data_k_dist, aes(fill = k5_names, shape=k5_names), size = 3.5, alpha=0.97) +
    geom_text_repel(data=data_k_dist_D %>% filter(k5_names %in% c("2-variable warm", "3-stable cool")), 
                    aes(x=lon, y=lat, label=station_id), inherit.aes = FALSE, 
                    fontface="bold", family="Roboto Condensed",
                    segment.color="gray10",
                    color = "white",     # text color
                    bg.color = "grey10", # shadow color
                    bg.r = 0.15,
                    size=4.5,
                    point.padding = 0.1,
                    box.padding = 0.1,
                    force=1,
                    nudge_x = 0.03,
                    min.segment.length = 0.9,
                    segment.inflect = FALSE, 
                    #segment.square=FALSE,
                    segment.ncp = 3,
                    segment.curvature = -0.1) +
    
    geom_sf(data = data_k_dist_D %>% filter(k5_names %in% c("2-variable warm", "3-stable cool")), 
            aes(size=dist_to_centroid, fill=k5_names, shape=k5_names), color="black", 
            alpha=0.97, show.legend = TRUE, inherit.aes = FALSE) +
    # add north arrow and scale bar
    annotation_north_arrow(location="br", 
                           width = unit(0.8,"cm"),
                           height=unit(1.2, "cm"),
                           pad_y = unit(0.9, "cm")) +
    annotation_scale(location="br") +
    # use custom scales
    scalesizes +
    scale_shape_manual("Thermal \nClasses", 
                       #values=c(22, 21, 24, 23, 8),
                       values=c(21, 24),
                       guide=guide_legend(order=1)) +
    scale_fill_manual("Thermal \nClasses", 
                      values=thermCols$color[2:3],
                      guide=guide_legend(override.aes = list(size=4), order=1)) +
    
    theme_map(base_family = "Roboto Condensed", base_size = 8) +
    theme(legend.background = element_rect(fill = NA),
          legend.box.background = element_rect(fill="white"),
          legend.key = element_rect(color = NA, size=NA),
          legend.position = c(0.03, 0.45), 
          legend.spacing.y = unit(0,"cm"),
          legend.margin = margin(0.15, 0.1, 0.1, 0.2, "cm"))
)


# save plot
save(map_pan_D, file="output/figures/PLOS_ONE/figure_map_panel_D.rda")

# save
ggsave(filename="output/figures/PLOS_ONE/Fig5_D_classification_map_cent_dist.pdf", dpi=300, width=8, height = 6, units="in", device = cairo_pdf)
ggsave(filename="output/figures/PLOS_ONE/Fig5_D_classification_map_cent_dist.tiff", dpi=300, 
       width=5.2, height = 3, units="in", scale = 1.3)

# PANEL C: CENTROID DIST MAP -----------------------------------------------------

# panel 3 includes:
sites3 <- c("11303500", "11303000", "GMB", "ORA", "OBS", "GDC", "CLP")
pan3 <- data_k_dist %>% filter(station_id %in% sites3)

# make a bounding box in lat/lon
(mapRange3 <- c(range(st_coordinates(pan3)[,1]),range(st_coordinates(pan3)[,2])) + 
    # add slight buffer around sites
    c(-0.09, .09, -0.05, 0.075))

# make polygon
pan3_bbx <- st_as_sfc(st_bbox(c(xmin = mapRange3[1], xmax = mapRange3[2], ymax = mapRange3[4], ymin = mapRange3[3]), crs = st_crs(4326)))

# crop everything by panel_area 3 (C)
rivs_C <- st_intersection(rivs, pan3_bbx)
mainstems_gage_all_C <- st_intersection(mainstems_gage_all, pan3_bbx)
dams_final_C <- st_crop(dams_final, pan3_bbx)
data_k_dist_C <- st_crop(data_k_dist, pan3_bbx)

# use these symbols:15, 16, 17, 18, 8
(map_pan_3 <-
    ggplot() +
    annotation_map_tile(data=pan3_bbx, type = "osm", zoomin = -1, cachedir = system.file("rosm.cache", package = "ggspatial"), forcedownload = FALSE, alpha=0.8) +
    annotation_map_tile(data=pan3_bbx, type = "hillshade", zoom = 12, zoomin= -1,
                        cachedir = system.file("rosm.cache", package = "ggspatial"), 
                        forcedownload = FALSE, alpha=0.9) +
    
    geom_sf(data=rivs_C, lwd=0.3, color="dodgerblue", 
            show.legend = FALSE, alpha=0.6, inherit.aes = FALSE) +
    geom_sf(data=mainstems_gage_all_C, lwd=0.3, color="dodgerblue", 
            show.legend = FALSE, alpha=0.9, inherit.aes = FALSE) + 
    geom_sf(data=dams_final_C, fill="black", pch=25, size=4.5, alpha=0.75, inherit.aes = FALSE) +
    geom_text_repel(data=data_k_dist_C %>% 
                      filter(k5_names %in% c("2-variable warm", "3-stable cool", "4-variable cool")), 
                    aes(x=lon, y=lat, label=station_id), inherit.aes = FALSE, 
                    fontface="bold", family="Roboto Condensed",
                    segment.color="gray10",
                    color = "white",     # text color
                    bg.color = "grey10", # shadow color
                    bg.r = 0.15,
                    size=4.5,
                    point.padding = 0.2,
                    box.padding = 0.2,
                    force=1,
                    nudge_x = 0.03,
                    nudge_y = 0.03,
                    min.segment.length = 1.1,
                    segment.inflect=TRUE,
                    segment.angle = 25,
                    segment.ncp = 2,
                    segment.curvature = -0.1) +
    
    geom_sf(data = data_k_dist_C %>% 
              filter(k5_names %in% c("2-variable warm", "3-stable cool", "4-variable cool")), 
            aes(size=dist_to_centroid, fill=k5_names, shape=k5_names), color="black", 
            alpha=0.97, show.legend = TRUE, inherit.aes = FALSE) +
    # add north arrow and scale bar
    annotation_north_arrow(location="br", 
                           width = unit(0.8,"cm"),
                           height=unit(1.2, "cm"),
                           pad_y = unit(0.9, "cm")) +
    annotation_scale(location="br") +
    # use custom scales
    scalesizes +
    scale_shape_manual("Thermal \nClasses", 
                       #values=c(22, 21, 24, 23, 8),
                       values=c(21, 24, 23),
                       guide=guide_legend(order=1)) +
    scale_fill_manual("Thermal \nClasses", 
                      values=thermCols$color[2:4],
                      guide=guide_legend(override.aes = list(size=4), order=1)) +
    
    theme_map(base_family = "Roboto Condensed", base_size = 8) +
    theme(legend.background = element_rect(fill = NA),
          legend.box.background = element_rect(fill="white"),
          legend.key = element_rect(color = NA, size=NA),
          legend.position = c(0.03, 0.5), 
          legend.spacing.y = unit(0,"cm"),
          legend.margin = margin(0.15, 0.1, 0.1, 0.2, "cm"))
)

# save plot
save(map_pan_D, file="output/figures/PLOS_ONE/figure_map_panel_C.rda")

# save
ggsave(filename="output/figures/PLOS_ONE/Fig5_C_classification_map_cent_dist.pdf", dpi=300, width=8, height = 6, units="in", device = cairo_pdf)
ggsave(filename="output/figures/PLOS_ONE/Fig5_C_classification_map_cent_dist.tiff", dpi=250, 
       width=5, height = 4.1, units="in", scale = 1.3)


# PANEL B: CENTROID DIST MAP -----------------------------------------------------

# make a bounding box in lat/lon
(mapRange4 <- c(range(st_coordinates(panel_areas[4,])[,1]),range(st_coordinates(panel_areas[4,])[,2])))

# make polygon
pan4_bbx <- st_as_sfc(st_bbox(c(xmin = mapRange4[1], xmax = mapRange4[2], ymax = mapRange4[4], ymin = mapRange4[3]), crs = st_crs(4326)))

# crop everything by panel_area 4 (B)
rivs_B <- st_intersection(rivs, pan4_bbx)
mainstems_gage_all_B <- st_intersection(mainstems_gage_all, pan4_bbx)
dams_final_B <- st_crop(dams_final, pan4_bbx)
data_k_dist_B <- st_crop(data_k_dist, pan4_bbx)

# map
(map_pan_4 <- ggplot() +
    
    # background
    annotation_map_tile(data=pan4_bbx, type = "osm", zoom = 11, cachedir = system.file("rosm.cache", package = "ggspatial"), forcedownload = FALSE, alpha=0.8) +

    # rivers
    geom_sf(data=rivs_B, lwd=0.3, color="dodgerblue", 
            show.legend = FALSE, alpha=0.6, inherit.aes = FALSE) +
    geom_sf(data=mainstems_gage_all_B, lwd=0.3, color="dodgerblue", 
            show.legend = FALSE, alpha=0.9, inherit.aes = FALSE) + 
    geom_sf(data=dams_final_B, fill="black", pch=25, size=4.5, alpha=0.75, inherit.aes = FALSE) +
    geom_text_repel(data=data_k_dist_B %>% 
                      filter(k5_names %in% c("3-stable cool", "4-variable cool")), 
                    aes(x=lon, y=lat, label=station_id), inherit.aes = FALSE, 
                    fontface="bold", family="Roboto Condensed",
                    segment.color="gray10",
                    color = "white",     # text color
                    bg.color = "grey10", # shadow color
                    bg.r = 0.15,
                    size=4.5,
                    point.padding = 0.2,
                    box.padding = 0.2,
                    force=1,
                    nudge_x = 0.03,
                    nudge_y = 0.03,
                    min.segment.length = 1.1,
                    segment.inflect=TRUE,
                    segment.angle = 25,
                    segment.ncp = 2,
                    segment.curvature = -0.1) +
    
    geom_sf(data = data_k_dist_B %>% 
              filter(k5_names %in% c("3-stable cool", "4-variable cool")), 
            aes(size=dist_to_centroid, fill=k5_names, shape=k5_names), color="black", 
            alpha=0.97, show.legend = TRUE, inherit.aes = FALSE) +
    # add north arrow and scale bar
    annotation_north_arrow(location="br", 
                           width = unit(0.8,"cm"),
                           height=unit(1.2, "cm"),
                           pad_y = unit(0.9, "cm")) +
    annotation_scale(location="br") +
    # customize scales
    scalesizes +
    scale_shape_manual("Thermal \nClasses", 
                       #values=c(22, 21, 24, 23, 8),
                       values=c(24, 23),
                       guide=guide_legend(order=1)) +
    scale_fill_manual("Thermal \nClasses", 
                      values=thermCols$color[3:4],
                      guide=guide_legend(override.aes = list(size=4), order=1)) +
    
    theme_map(base_family = "Roboto Condensed", base_size = 8) +
    theme(legend.background = element_rect(fill = NA),
          legend.box.background = element_rect(fill="white"),
          legend.key = element_rect(color = NA, size=NA),
          legend.position = c(0.03, 0.6), 
          legend.spacing.y = unit(0,"cm"),
          legend.margin = margin(0.2, 0.4, 0.2, 0.2, "cm"))
)


# save plot
save(map_pan_4, file="output/figures/PLOS_ONE/figure_map_panel_B.rda")

# save
ggsave(filename="output/figures/PLOS_ONE/Fig5_B_classification_map_cent_dist.pdf", dpi=300, width=8, height = 6, units="in", device = cairo_pdf)
ggsave(filename="output/figures/PLOS_ONE/Fig5_B_classification_map_cent_dist.tiff", dpi=250, 
       width=5, height = 4.1, units="in", scale = 1.3)


# PANEL A: CENTROID DIST MAP -----------------------------------------------------

# fix shasta dam site to be just label

# make a bounding box in lat/lon
(mapRange5 <- c(range(st_coordinates(panel_areas[5,])[,1]),range(st_coordinates(panel_areas[5,])[,2])) +
   # add slight buffer around sites
   c(-0.03, .01, -0.05, 0.065))

# make polygon
pan5_bbx <- st_as_sfc(st_bbox(c(xmin = mapRange5[1], xmax = mapRange5[2], ymax = mapRange5[4], ymin = mapRange5[3]), crs = st_crs(4326)))

# crop everything by panel_area 5 (A)
rivs_A <- st_intersection(rivs, pan5_bbx)
mainstems_gage_all_A <- st_intersection(mainstems_gage_all, pan5_bbx)
dams_final_A <- st_crop(dams_final, pan5_bbx)
data_k_dist_A <- st_crop(data_k_dist, pan5_bbx)

# map
(map_pan_5 <- ggplot() +
    # background
    annotation_map_tile(data=pan5_bbx, type = "osm", zoom = 11, cachedir = system.file("rosm.cache", package = "ggspatial"), forcedownload = FALSE, alpha=0.8) +
    # river
    geom_sf(data=rivs_A, lwd=0.1, color="dodgerblue", 
            show.legend = FALSE, alpha=0.3, inherit.aes = FALSE) +
    geom_sf(data=mainstems_gage_all_A, lwd=0.3, color="dodgerblue", 
            show.legend = FALSE, alpha=0.9, inherit.aes = FALSE) + 
    geom_sf(data=dams_final_A, fill="black", pch=25, size=4.5, alpha=0.75, inherit.aes = FALSE) +
    geom_text_repel(data=data_k_dist_A %>% 
                      filter(!k5_names %in% c("1-stable warm")), 
                    aes(x=lon, y=lat, label=station_id), inherit.aes = FALSE, 
                    fontface="bold", family="Roboto Condensed",
                    segment.color="gray10",
                    color = "white",     # text color
                    bg.color = "grey10", # shadow color
                    bg.r = 0.15,
                    size=4.5,
                    point.padding = 0.2,
                    box.padding = 0.2,
                    force=1,
                    nudge_x = 0.03,
                    nudge_y = 0.03,
                    min.segment.length = 1.5,
                    segment.inflect=TRUE,
                    segment.angle = 25,
                    segment.ncp = 2,
                    segment.curvature = -0.1) +
    
    # points
    geom_sf(data = data_k_dist_A %>%
              filter(k5_names %in% c("5-stable cold")),
            aes(size=dist_to_centroid), fill=thermCols$color[5], pch=21,
            alpha=0.97, show.legend = FALSE, inherit.aes = FALSE) +
    
    geom_sf(data = data_k_dist_A %>% 
              filter(!k5_names %in% c("1-stable warm")), 
            aes(size=dist_to_centroid, fill=k5_names, shape=k5_names), color="black", 
            alpha=0.97, show.legend = TRUE, inherit.aes = FALSE) + 

    
    # add north arrow and scale bar
    annotation_north_arrow(location="bl", 
                           width = unit(0.8,"cm"),
                           height=unit(1.2, "cm"),
                           pad_y = unit(0.9, "cm")) +
    annotation_scale(location="bl") +
    
    # customize scales
    scalesizes +
    scale_shape_manual("Thermal \nClasses",
                       #values=c(22, 21, 24, 23, 8),
                       values=c(21, 24, 23, 8),
                       guide=guide_legend(order=1)) +
    scale_fill_manual("Thermal \nClasses", 
                      values=c(thermCols$color[2:5]),
                      guide=guide_legend(override.aes = list(size=4), order=1)) +
    
    theme_map(base_family = "Roboto Condensed", base_size = 8) +
    theme(
      #plot.margin = margin(0, 0, 0, 5, "cm"),
      legend.background = element_rect(fill = NA),
      legend.box.background = element_rect(fill="white"),
      legend.key = element_rect(color = NA, size=NA),
      legend.position = c(0.01, 0.15), 
      legend.spacing.y = unit(0,"cm"),
      legend.margin = margin(0.2, 0.2, 0.2, 0.2, "cm"))
)


# save plot
save(map_pan_5, file="output/figures/PLOS_ONE/figure_map_panel_A.rda")

# write out
ggsave(filename="output/figures/PLOS_ONE/Fig5_A_classification_map_cent_dist.pdf", dpi=300, width=4, height = 8, units="in", device = cairo_pdf)

ggsave(filename="output/figures/PLOS_ONE/Fig5_A_classification_map_cent_dist.tiff", dpi=300, 
       width=2.7, height = 5.5, units="in", scale = 1.3)



# ARCHIVED CODE -----------------------------------------------------------


# MAPVIEW MAP OF DIST TO CENTROID -----------------------------------------

library(mapview)

# mapview sized by cent dist and dist to dam
mfinalcent <- 
  # distance to dam
  mapview(data_k_dist, zcol="cum_len_km",
          layer.name="Distance<br> to Dam (km)",
          legend=TRUE, cex="cum_len_km", basemaps=mapbases) +
  #col.regions=unique(data_k_dist$color[order(data_k_dist$k5_names)])) +
  
  # distance to centroid
  mapview(data_k_dist, zcol="dist_to_centroid",
          layer.name="Distance <br>to Centroid",
          legend=TRUE, cex="dist_to_centroid", basemaps=mapbases) +
  #col.regions=unique(data_k_dist$color[order(data_k_dist$k5_names)])) + 
  
  # dams 
  mapview(dams_final, col.region="black", 
          cex=6, layer.name="Dams", homebutton=FALSE) + 
  
  # ds mainstems
  mapview(ds_main_merged, col.region="darkblue", lwd=1.5,
          homebutton=FALSE, legend=FALSE, layer.name="Rivers DS") +
  
  # k classes
  mapview(data_k_dist,  zcol="k5_names", map.types=mapbases,
          layer.name="Thermal Classes",
          col.regions=unique(data_k_dist$color[order(data_k_dist$k5_names)]), 
          alpha.regions=0.8, cex=3.5,
          hide=FALSE, homebutton=FALSE)

mfinalcent@map %>% leaflet::addMeasure(primaryLengthUnit = "meters")







# GGMAP BACKGROUND --------------------------------------------------------

library(ggmap)

# make a bounding box in lat/lon
mapRange1 <- st_transform(ca, 4326) 
(mapRange1 <- c(range(st_coordinates(mapRange1)[,1]),range(st_coordinates(mapRange1)[,2])))

# this only works with an API KEY
map3 <- get_map(location=c(mapRange1[1], mapRange1[3],mapRange1[2], mapRange1[4]), crop = F,
                color="bw",
                maptype="terrain-background",
                source="stamen",
                zoom=8)
# save as an object for later
ggmap_base_z8 <- map3
save(ggmap_base_z8, file = "output/13d_ggmap_base_layer_zoom8.rda")

# quick view?
ggmap(map3)
#ggmap(ggmap_base)

# Define a function to fix the bbox to be in EPSG:3857
#  ggmap_bbox <- function(map) {
#    if (!inherits(map, "ggmap")) stop("map must be a ggmap object")
#    # Extract the bounding box (in lat/lon) from the ggmap to a numeric vector, 
#    # and set the names to what sf::st_bbox expects:
#    map_bbox <- setNames(unlist(attr(map, "bb")), 
#                         c("ymin", "xmin", "ymax", "xmax"))
#    
#    # Convert the bbox to an sf polygon, transform it to 3857, 
#    # and convert back to a bbox (convoluted, but it works)
#    bbox_3310 <- st_bbox(st_transform(st_as_sfc(st_bbox(map_bbox, crs = 4326)), 3310))
#    
#    # Overwrite the bbox of the ggmap object with the transformed coordinates 
#    attr(map, "bb")$ll.lat <- bbox_3310["ymin"]
#    attr(map, "bb")$ll.lon <- bbox_3310["xmin"]
#    attr(map, "bb")$ur.lat <- bbox_3310["ymax"]
#    attr(map, "bb")$ur.lon <- bbox_3310["xmax"]
#    map
# }
# Use the function:
# test_map <- ggmap_bbox(map3)
# ggmap(test_map) + 
#    coord_sf(crs = st_crs(3310))  # force the ggplot2 into 3310


# actual map
(ggmap(map3) + coord_sf(crs = st_crs(4326))+
    geom_sf(data=hydro, aes(lty="Hydroregions"), color="black", fill=NA, size=0.25, inherit.aes = FALSE) +
    scale_linetype_manual("", values = c("Hydroregions" = 2), 
                          guide = guide_legend(override.aes = list(color = "white", alpha=0.5, lwd=0.5), order=3)) +
    #geom_sf(data=rivs, lwd=0.1, color="darkblue", show.legend = FALSE, alpha=0.3) +
    geom_sf(data=rivers_ca, lwd=0.2, color="turquoise4", show.legend = FALSE, alpha=0.45, inherit.aes = FALSE) + 
    geom_sf(data=mainstems_gage_all, lwd=0.3, color="turquoise4", show.legend = FALSE, alpha=0.7, inherit.aes = FALSE) + 
    #geom_sf(data=ca, fill = NA, color = 'slategray4', size = 0.7, alpha = 0.3, inherit.aes = FALSE) +
    geom_sf(data=dams_final, aes(color="Dams"), fill="black", pch=25, size=3, alpha=0.8, inherit.aes = FALSE) +
    scale_color_manual("", values=c("Dams"="black"), 
                       guide = guide_legend(override.aes = list(alpha=1, lty=NA), order=2))+
    geom_sf(data = data_k_dist %>% filter(k_5), aes(fill = k5_names, size=dist_to_centroid), pch=21, alpha=0.97, inherit.aes = FALSE) +
    geom_sf(data = data_k_dist, aes(fill = k5_names, size=dist_to_centroid), pch=21, alpha=0.97, inherit.aes = FALSE) +
    #geom_text_repel(data=hydro, aes(x=lon, y=lat, label=HR_NAME), size=3, point.padding = 0.7, min.segment.length = 5, segment.alpha = 0.5, force=8, segment.color = "gray20", show.legend = FALSE) +
    # add north arrow and scale bar
    annotation_north_arrow(location="tr", 
                           pad_x = unit(1.5, "cm"), 
                           pad_y = unit(0.5, "cm")) +
    annotation_scale(location="br") +
    scale_fill_manual("Thermal \nClasses", values=thermCols$color, guide=guide_legend(override.aes = list(size=4), order=1)) +
    scale_size_area("Centroid Dist.", guide=guide_legend(order=2)) +
    theme_map(base_family = "Roboto Condensed", base_size = 15) +
    theme(legend.background = element_rect(fill = NA),
          #plot.background = element_rect(color="black", size = 0.5),
          legend.key = element_rect(fill = NA, color = NA, size=NA),
          legend.position = c(0, 0.05), 
          legend.spacing.y = unit(0.1,"cm"),
          legend.margin = margin(0.1, 0, 0, 0.2, "cm"))
)

# save
ggsave(filename="output/figures/Fig_4_classification_map_rivers_select_cent_dist_w_ggmap_terrain.pdf", dpi=300, width=8.5, height = 11, units="in", device=cairo_pdf)   

