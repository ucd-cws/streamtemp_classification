# Code description --------------------------------------------------------

# Figure 4: Map of study locations and k-5 clusters in CA.

# Libraries ---------------------------------------------------------------

library(sf)
library(tidyverse)
library(ggthemes)
library(ggspatial)
library(mapview)
library(purrr)
library(ggrepel)
#library(wateRshedTools)

# LOAD DATA ---------------------------------------------------------------

# pick a CRS
crs_proj <- 3310

# clustering data and dams
load("output/12_data_k_centdist_damdist.rda")
load("output/12_dams_final_mainstems_merged.rda")

# transform to same projection:
data_k_dist <- data_k_dist %>% st_transform(crs_proj)
dams_final <- dams_final %>% st_transform(crs_proj)
ds_main_merged <- ds_main_merged %>% st_transform(crs_proj)

# major rivers
load("data/major_rivers_dissolved.rda")
rivs <- st_transform(rivs, crs_proj)

# hydrologic regions
hydro <- st_read("data/shps/DWR_HydrologicRegions-utm11.shp") %>% 
   st_transform(crs_proj)

# get more river data
load("output/13d_rivers_ca_streamorder_6.rda")
load("output/12_selected_nhd_gage_mainstems.rda") # mainstems from gages
mainstems_gage_all <- mainstems_gage_all %>% st_transform(crs_proj)

# load("output/dam_panel_areas.rda")
# # fix and save:
# panel_areas <- panel_areas$finished %>% st_set_crs(4326) %>% 
#    select(-X_leaflet_id) %>% 
#    rownames_to_column("panelID") %>% 
#    mutate(panelID = as.integer(panelID))
# mapview(panel_areas)
#save(panel_areas,file = "output/dam_panel_areas_revised.rda")
load("output/dam_panel_areas_revised.rda")

mapview(panel_areas) + 
   mapview(data_k_dist,  zcol="k5_names", map.types=mapbases,
           layer.name="Thermal Classes",
           col.regions=unique(data_k_dist$color[order(data_k_dist$k5_names)]), 
           alpha.regions=0.8, cex=3.5,
           hide=FALSE, homebutton=FALSE)

# Tidy Data ---------------------------------------------------------------

hydro <- st_transform(hydro, 4326)

# add centroid labels
hydro <- hydro %>% 
   # add centroid values for labels using the geometry column
   mutate(lon=map_dbl(geometry, ~st_centroid(.x)[[1]]), 
          lat=map_dbl(geometry, ~st_centroid(.x)[[2]])) 

# simplify (seems a very dense object) (keep = the % of points you keep)
hydro <- rmapshaper::ms_simplify(hydro, keep = 0.1,
                        keep_shapes = TRUE)

#mapview(hydro, zcol="HR_NAME") + mapview(mainstems_gage_all, color="steelblue")

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

# check levels
levels(data_k_dist$k5_names)

# setup some basemaps
mapbases <- c("Stamen.TonerLite", 
              "CartoDB.PositronNoLabels", "OpenStreetMap",
              "Esri.WorldImagery", "Esri.WorldTopoMap")
# and set defaults
mapviewOptions(basemaps=mapbases)

# get CA and County Boundaries
ca <- USAboundaries::us_boundaries(type="state", states="ca", resolution = "high")
ca_co <- USAboundaries::us_counties(states="ca") %>% st_transform(crs_proj)

# crop rivs by ca
rivs <- st_intersection(rivs, st_transform(ca, crs_proj))

# crop the other rivers data set
rivers_ca <- st_transform(rivers, st_crs(ca)) %>% st_intersection(., ca)
#summary(rivers_ca$streamorde)

#plot(ca$geometry, border="gray40")
#plot(rivers_ca$geometry, col="steelblue", lwd = (rivers_ca$streamorde / 4), add=TRUE)

# now fix the CRS
ca <- ca %>% st_transform(crs_proj)
rivers_ca <- rivers_ca %>% st_transform(crs_proj)

# Get Major Streams for CA ------------------------------------------------
# 
# library(httr)
# #library(wateRshedTools)
# 
# get_flowlines <- function(streamorder, mapRange){
#    postURL <- "https://cida.usgs.gov/nwc/geoserver/nhdplus/ows"
#    
#    filterXML <- paste0('<?xml version="1.0"?>',
#                        '<wfs:GetFeature xmlns:wfs="http://www.opengis.net/wfs" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:gml="http://www.opengis.net/gml" service="WFS" version="1.1.0" outputFormat="shape-zip" xsi:schemaLocation="http://www.opengis.net/wfs http://schemas.opengis.net/wfs/1.1.0/wfs.xsd">',
#                        '<wfs:Query xmlns:feature="https://gov.usgs.cida/nhdplus" typeName="feature:nhdflowline_network" srsName="EPSG:4326">',
#                        '<ogc:Filter xmlns:ogc="http://www.opengis.net/ogc">',
#                        '<ogc:And>',
#                        '<ogc:PropertyIsGreaterThan>',
#                        '<ogc:PropertyName>streamorde</ogc:PropertyName>',
#                        '<ogc:Literal>',streamorder-1,'</ogc:Literal>',
#                        '</ogc:PropertyIsGreaterThan>',
#                        '<ogc:BBOX>',
#                        '<ogc:PropertyName>the_geom</ogc:PropertyName>',
#                        '<gml:Envelope>',
#                        '<gml:lowerCorner>',mapRange[3]," ",mapRange[1],'</gml:lowerCorner>',
#                        '<gml:upperCorner>',mapRange[4]," ",mapRange[2],'</gml:upperCorner>',
#                        '</gml:Envelope>',
#                        '</ogc:BBOX>',
#                        '</ogc:And>',
#                        '</ogc:Filter>',
#                        '</wfs:Query>',
#                        '</wfs:GetFeature>')
#    
#    destination = file.path(tempdir(),"nhdflowline_network.zip")
#    file <- POST(postURL, body = filterXML, write_disk(destination, overwrite=T))
#    
#    filePath <- tempdir()
#    print("unzipping...")
#    unzip(destination, exdir = filePath)
#    
#    flowLines <- st_read(filePath, layer = 'nhdflowline_network')
#    
#    return(flowLines)
# }
# 
# # get range of lat/longs from counties for mapping and river function
# mapRange1 <- c(range(st_coordinates(ca)[,1]),range(st_coordinates(ca)[,2]))
# 
# # get rivers
# rivers <- get_flowlines(6, mapRange1)
# 
# # quick plot
# plot(ca$geometry, border="gray40")
# plot(rivers$geometry, col="steelblue", lwd = (rivers$streamorde / 4), add=TRUE)
# 
# # SAVE
# save(rivers, file = "output/13d_rivers_ca_streamorder_6.rda")

# BASE GGPLOT MAP FOR k=5 --------------------------------------------------------------

# transform a few layers
ca <- st_transform(ca, 4326)
rivers_ca <- st_transform(rivers_ca, 4326)
rivs <- st_transform(rivs, 4326)
mainstems_gage_all <- st_transform(mainstems_gage_all, 4326)
dams_final <- st_transform(dams_final, 4326)
data_k_dist <- st_transform(data_k_dist, 4326)


# use these symbols:15, 16, 17, 18, 8

(map1 <- ggplot()+
      #coord_sf(label_axes = "--EN", datum = 4326) +
      geom_sf(data=hydro, aes(lty="Hydroregions"), color=alpha("black", 0.8), fill=NA, size=0.25) +
      scale_linetype_manual("", values = c("Hydroregions" = 2), 
                            guide = guide_legend(override.aes = list(color = "black", alpha=0.5, lwd=0.5), order=2)) +
      geom_sf(data=rivs, lwd=0.1, color="dodgerblue", show.legend = FALSE, alpha=0.3) +
      #geom_sf(data=rivers_ca, lwd=0.3, color="dodgerblue", show.legend = FALSE, alpha=0.45) + 
      geom_sf(data=mainstems_gage_all, lwd=0.3, color="dodgerblue", show.legend = FALSE, alpha=0.9) + 
      geom_sf(data=ca, fill = NA, color = 'slategray4', size = 0.7, alpha = 0.3) +
      geom_sf(data=dams_final, aes(fill="Dams"), pch=25, size=4.5, alpha=0.75) +
      scale_fill_manual("", values=c("Dams"="black"), 
                        guide = guide_legend(override.aes = list(alpha=1, order=2, lty=NA)))+
      geom_sf(data = data_k_dist, aes(color = k5_names, shape=k5_names), size = 3.5, alpha=0.97) +
      geom_sf(data = data_k_dist %>% filter(k5_names=="2-variable warm"), pch=21, color="black", 
              fill=thermCols$color[2], size = 3.5, alpha=0.97, show.legend = FALSE) +
      geom_sf(data = data_k_dist %>% filter(k5_names=="1-stable warm"), pch=22, color="black", 
              fill=thermCols$color[1], size = 3.5, alpha=0.97, show.legend = FALSE) +
      geom_sf(data = data_k_dist %>% filter(k5_names=="3-stable cool"), pch=24, color="black", 
              fill=thermCols$color[3], size = 3.5, alpha=0.97, show.legend = FALSE) +
      geom_sf(data = data_k_dist %>% filter(k5_names=="4-variable cool"), pch=23, color="black", 
              fill=thermCols$color[4], size = 3.5, alpha=0.97, show.legend = FALSE) +
      geom_sf(data = data_k_dist %>% filter(k5_names=="5-stable cold"), pch=8, 
              color="cyan", size = 3.7, show.legend = FALSE) +
      geom_text_repel(data=hydro, aes(x=lon, y=lat, label=HR_NAME), 
                      # need to tweak these individually, a bit annoying
                      nudge_x = c(-1.2, # north coast, 
                                  -0.6, # sf bay
                                  1.3, # san joaq
                                  0.1, # central coast
                                  -0.1, # tulare
                                  0, # south coast
                                  0.9, # sacto
                                  0, # N lahontan
                                  0.1, # S lahontan
                                  0), # colorado
                      nudge_y = c(0.5, 0, -0.15, 0, .1, 0, 0, 1, 0, 0),
                      fontface="bold", family="Roboto Condensed",
                      segment.color="gray10",
                      color = "black",     # text color
                      bg.color = "grey90", # shadow color
                      bg.r = 0.15,
                      size=4.5,
                      box.padding = 0.5,
                      force=1.5,
                      min.segment.length = 5,
                      #segment.inflect = FALSE, 
                      #segment.square=FALSE,
                      segment.ncp = 3,
                      segment.curvature = -0.1) +
      # add north arrow and scale bar
      annotation_north_arrow(location="br", 
                             width = unit(0.8,"cm"),
                             height=unit(1.2, "cm"),
                             pad_y = unit(0.9, "cm")) +
      annotation_scale(location="br") +
      scale_color_manual("Thermal \nClasses", values=thermCols$color, 
                         guide=guide_legend(override.aes = 
                                               list(pch=c(22, 21, 24, 23, 8),
                                                    fill=thermCols$color, color="black"), order=1)) +
      scale_shape_manual("Thermal \nClasses", 
                         #values=c(15,16,17,18,8),
                         values=c(22, 21, 24, 23, 8),
                         guide=guide_legend(override.aes = list(lty=NA), order=1)) +
      theme_map(base_family = "Roboto Condensed", base_size = 15) +
      theme(legend.background = element_rect(fill = NA),
            #plot.background = element_rect(color="black", size = 0.5),
            legend.key = element_rect(color = NA, size=NA),
            legend.position = c(0, 0.05), 
            legend.spacing.y = unit(0,"cm"),
            legend.margin = margin(0.1, 0, 0, 0, "cm"))
)

# save
ggsave(filename="output/figures/Fig_4_classification_map_hydroregions_all_rivers.pdf", dpi=300, width=8, height = 11.5, units="in", device=cairo_pdf)
ggsave(filename="output/figures/Fig_4_classification_map_hydroregions_all_rivers.png", dpi=300, width=8, height = 11.5, units="in")
#ggsave(filename="output/figures/Fig_4_classification_map_hydroregions_rivers_select.pdf", dpi=300, width=8, height = 11.5, units="in", device=cairo_pdf)


# ADD INSET ---------------------------------------------------------------

us <- USAboundaries::us_boundaries(type="state", resolution = "low") %>% 
   filter(!state_abbr %in% c("PR", "AK", "HI"))

# make a box around rivers (a grid with an n=1) for inset
ca_box <- st_make_grid(ca, n = 1) #%>% st_centroid()

# Inset map: US
(p2 <- ggplot() + 
   geom_sf(data = us, colour = "grey10", fill = "tan", alpha=0.4) +
   coord_sf() +
   theme_minimal() + 
   geom_sf(data=ca_box, color="purple4", fill=NA, size=.8, alpha=0.7) +
   labs(x = NULL, y = NULL) +
   theme(axis.text.x = element_blank(),
         axis.text.y = element_blank(),
         axis.ticks = element_blank(),
         axis.title.x = element_blank(),
         axis.title.y = element_blank(),
         panel.grid.major = element_line(colour = "transparent"),
         plot.background = element_rect(color = "black", fill="white"),
         #plot.background = element_blank(),
         panel.border = element_blank(),
         plot.margin = unit(c(0, 0, 0 ,0), "mm")))
p2


# SAVE FINAL MAP ----------------------------------------------------------


# add diff libraries
library(grid)
library(gridExtra)

# to save:
#start from this first (may need to change to just "pdf" instead of "cairo_pdf")
png(filename = "output/figures/Fig_4_classification_map_hydroregions_inset.png", width = 11, height = 8, units = "in", res = 300)

# to just view, start from below here
grid.newpage()
mainmap <- viewport(width = 1, height = 1, x = 0.5, y = 0.5) # main map
insetmap <- viewport(width = 0.32, height = 0.28, x = 0.7, y = 0.8) # inset
print(map1, vp = mainmap) 
print(p2, vp = insetmap)

# make sure to run this if saving out as pdf/png
dev.off()

# PANEL 1: CENTROID DIST MAP -----------------------------------------------------

library(ggmap)

# make a bounding box in lat/lon
(mapRange1 <- c(range(st_coordinates(panel_areas[1,])[,1]),range(st_coordinates(panel_areas[1,])[,2])))

# this only works with an API KEY
map1 <- get_map(location=c(mapRange1[1], mapRange1[3],mapRange1[2], mapRange1[4]), crop = F,
                force=TRUE,
                color="color", #color
                maptype="terrain", # or color
                source="google", # google
                zoom=12)

# save as an object for later
save(map1, file = "output/13d_ggmap_base_layer_map1.rda")

# quick view?
ggmap(map1)


# use these symbols:15, 16, 17, 18, 8
(map_pan_1 <- ggmap(map1) +
      #coord_sf(label_axes = "--EN", datum = 4326) +
      geom_sf(data=rivs, lwd=0.1, color="dodgerblue", show.legend = FALSE, alpha=0.3, inherit.aes = FALSE) +
       
      geom_sf(data=mainstems_gage_all, lwd=0.3, color="dodgerblue", show.legend = FALSE, alpha=0.9, inherit.aes = FALSE) + 
      #geom_sf(data=ca, fill = NA, color = 'slategray4', size = 0.7, alpha = 0.3, inherit.aes = FALSE) +
      geom_sf(data=dams_final, fill="black", pch=25, size=4.5, alpha=0.75, inherit.aes = FALSE) +
      #geom_sf(data = data_k_dist, aes(fill = k5_names, shape=k5_names), size = 3.5, alpha=0.97) +
      geom_text_repel(data=data_k_dist %>% filter(k5_names %in% c("2-variable warm")), 
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
      
      geom_sf(data = data_k_dist %>% filter(k5_names %in% c("2-variable warm")), 
              aes(size=dist_to_centroid, fill=k5_names, shape=k5_names), color="black", 
              alpha=0.97, show.legend = TRUE, inherit.aes = FALSE) +
      # add north arrow and scale bar
      annotation_north_arrow(location="br", 
                             width = unit(0.8,"cm"),
                             height=unit(1.2, "cm"),
                             pad_y = unit(0.9, "cm")) +
      annotation_scale(location="br") +
      scale_size_binned("Centroid \nDistance",
                        guide=guide_legend(order=2)) +
      scale_shape_manual("Thermal \nClasses", 
                         #values=c(22, 21, 24, 23, 8),
                         values=c(21),
                         guide=guide_legend(order=1)) +
      scale_fill_manual("Thermal \nClasses", 
                        values=thermCols$color[2],
                        guide=guide_legend(override.aes = list(size=4), order=1)) +
      
      theme_map(base_family = "Roboto Condensed", base_size = 14) +
      theme(legend.background = element_rect(fill = NA),
            #plot.background = element_rect(color="black", size = 0.5),
            legend.box.background = element_rect(fill="white"),
            legend.key = element_rect(color = NA, size=NA),
            legend.position = c(0.05, 0.25), 
            legend.spacing.y = unit(0.1,"cm"),
            legend.margin = margin(0.15, 0.1, 0.1, 0.2, "cm"))
)
      

# save
ggsave(filename="output/figures/Fig_4_classification_map_cent_dist_panel_1.png", dpi=300, width=8, height = 6, units="in")

# PANEL 2: CENTROID DIST MAP -----------------------------------------------------

# panel 2 includes:
sites2 <- c("GRF", "DNB", "H41", "FWQ")
pan2 <- data_k_dist %>% filter(station_id %in% sites2)

# make a bounding box in lat/lon
(mapRange2 <- c(range(st_coordinates(pan2)[,1]),range(st_coordinates(pan2)[,2])) + 
      # add slight buffer around sites
      c(-0.05, .075, -0.05, 0.075))

# this only works with an API KEY
map2 <- get_map(location=c(mapRange2[1], mapRange2[3], mapRange2[2], mapRange2[4]), 
                crop = T,
                force=TRUE,
                #color="bw",
                maptype = "terrain-background",
                source="stamen",
                color="color", 
                #maptype="terrain", # or color
                #source="google", # google
                zoom=12, 
                )

# quick view?
ggmap(map2)

# save as an object for later
save(map2, file = "output/13d_ggmap_base_layer_map2.rda")

# use these symbols:15, 16, 17, 18, 8
(map_pan_2 <- ggmap(map2) +
      #coord_sf(label_axes = "--EN", datum = 4326) +
      geom_sf(data=rivs, lwd=0.1, color="dodgerblue", show.legend = FALSE, alpha=0.3, inherit.aes = FALSE) +
      
      geom_sf(data=mainstems_gage_all, lwd=0.3, color="dodgerblue", show.legend = FALSE, alpha=0.9, inherit.aes = FALSE) + 
      #geom_sf(data=ca, fill = NA, color = 'slategray4', size = 0.7, alpha = 0.3, inherit.aes = FALSE) +
      geom_sf(data=dams_final, fill="black", pch=25, size=4.5, alpha=0.75, inherit.aes = FALSE) +
      #geom_sf(data = data_k_dist, aes(fill = k5_names, shape=k5_names), size = 3.5, alpha=0.97) +
      geom_text_repel(data=data_k_dist %>% filter(k5_names %in% c("2-variable warm", "3-stable cool")), 
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
      
      geom_sf(data = data_k_dist %>% filter(k5_names %in% c("2-variable warm", "3-stable cool")), 
              aes(size=dist_to_centroid, fill=k5_names, shape=k5_names), color="black", 
              alpha=0.97, show.legend = TRUE, inherit.aes = FALSE) +
      # add north arrow and scale bar
      annotation_north_arrow(location="br", 
                             width = unit(0.8,"cm"),
                             height=unit(1.2, "cm"),
                             pad_y = unit(0.9, "cm")) +
      annotation_scale(location="br") +
      scale_size_binned("Centroid \nDistance", breaks=c(seq(2,8,2)),
                        guide=guide_legend(order=2)) +
      scale_shape_manual("Thermal \nClasses", 
                         #values=c(22, 21, 24, 23, 8),
                         values=c(21, 24),
                         guide=guide_legend(order=1)) +
      scale_fill_manual("Thermal \nClasses", 
                        values=thermCols$color[2:3],
                        guide=guide_legend(override.aes = list(size=4), order=1)) +
      
      theme_map(base_family = "Roboto Condensed", base_size = 14) +
      theme(legend.background = element_rect(fill = NA),
            legend.box.background = element_rect(fill="white"),
            legend.key = element_rect(color = NA, size=NA),
            legend.position = c(0.03, 0.5), 
            legend.spacing.y = unit(0,"cm"),
            legend.margin = margin(0.15, 0.1, 0.1, 0.2, "cm"))
)


# save
ggsave(filename="output/figures/Fig_4_classification_map_cent_dist_panel_2.png", dpi=300, width=8, height = 6, units="in")

# PANEL 3: CENTROID DIST MAP -----------------------------------------------------

# panel 3 includes:
sites3 <- c("11303500", "11303000", "GMB", "ORA", "OBS", "GDC", "CLP")
pan3 <- data_k_dist %>% filter(station_id %in% sites3)

# make a bounding box in lat/lon
(mapRange3 <- c(range(st_coordinates(pan3)[,1]),range(st_coordinates(pan3)[,2])) + 
      # add slight buffer around sites
      c(-0.09, .09, -0.05, 0.075))

# this only works with an API KEY
map3 <- get_map(location=c(mapRange3[1], mapRange3[3], mapRange3[2], mapRange3[4]), 
                crop = TRUE, # clips exactly to bounding box
                force=TRUE,
                #color="bw",
                maptype = "terrain-background",
                source="stamen",
                color="color", 
                #maptype="terrain", # or color
                #source="google", # google
                zoom=11, 
)

# quick view?
ggmap(map3)

# save as an object for later
save(map3, file = "output/13d_ggmap_base_layer_map3.rda")

# use these symbols:15, 16, 17, 18, 8
(map_pan_3 <- ggmap(map3) +
      #coord_sf(label_axes = "--EN", datum = 4326) +
      geom_sf(data=rivs, lwd=0.1, color="dodgerblue", 
              show.legend = FALSE, alpha=0.3, inherit.aes = FALSE) +
      geom_sf(data=mainstems_gage_all, lwd=0.3, color="dodgerblue", 
              show.legend = FALSE, alpha=0.9, inherit.aes = FALSE) + 
      geom_sf(data=dams_final, fill="black", pch=25, size=4.5, alpha=0.75, inherit.aes = FALSE) +
      geom_text_repel(data=data_k_dist %>% 
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
      
      geom_sf(data = data_k_dist %>% 
                 filter(k5_names %in% c("2-variable warm", "3-stable cool", "4-variable cool")), 
              aes(size=dist_to_centroid, fill=k5_names, shape=k5_names), color="black", 
              alpha=0.97, show.legend = TRUE, inherit.aes = FALSE) +
      # add north arrow and scale bar
      annotation_north_arrow(location="br", 
                             width = unit(0.8,"cm"),
                             height=unit(1.2, "cm"),
                             pad_y = unit(0.9, "cm")) +
      annotation_scale(location="br") +
      scale_size_binned("Centroid \nDistance",
                        guide=guide_legend(order=2)) +
      scale_shape_manual("Thermal \nClasses", 
                         #values=c(22, 21, 24, 23, 8),
                         values=c(21, 24, 23),
                         guide=guide_legend(order=1)) +
      scale_fill_manual("Thermal \nClasses", 
                        values=thermCols$color[2:4],
                        guide=guide_legend(override.aes = list(size=4), order=1)) +
      
      theme_map(base_family = "Roboto Condensed", base_size = 14) +
      theme(legend.background = element_rect(fill = NA),
            legend.box.background = element_rect(fill="white"),
            legend.key = element_rect(color = NA, size=NA),
            legend.position = c(0.03, 0.5), 
            legend.spacing.y = unit(0,"cm"),
            legend.margin = margin(0.15, 0.1, 0.1, 0.2, "cm"))
)


# save
ggsave(filename="output/figures/Fig_4_classification_map_cent_dist_panel_3.png", dpi=300, width=8, height = 6, units="in")

# PANEL 4: CENTROID DIST MAP -----------------------------------------------------


# make a bounding box in lat/lon
(mapRange4 <- c(range(st_coordinates(panel_areas[4,])[,1]),range(st_coordinates(panel_areas[4,])[,2])))
#      c(-0.09, .09, -0.05, 0.075))

# this only works with an API KEY
map4 <- get_map(location=c(mapRange4[1], mapRange4[3], mapRange4[2], mapRange4[4]), 
                crop = TRUE, # clips exactly to bounding box
                force = TRUE,
                #color="bw",
                maptype = "terrain-background",
                source="stamen",
                color="color", 
                zoom=11 
)

# quick view?
ggmap(map4)

# save as an object for later
save(map4, file = "output/13d_ggmap_base_layer_map4.rda")

# map
(map_pan_4 <- ggmap(map4) +
      #coord_sf(label_axes = "--EN", datum = 4326) +
      geom_sf(data=rivs, lwd=0.1, color="dodgerblue", 
              show.legend = FALSE, alpha=0.3, inherit.aes = FALSE) +
      geom_sf(data=mainstems_gage_all, lwd=0.3, color="dodgerblue", 
              show.legend = FALSE, alpha=0.9, inherit.aes = FALSE) + 
      geom_sf(data=dams_final, fill="black", pch=25, size=4.5, alpha=0.75, inherit.aes = FALSE) +
      geom_text_repel(data=data_k_dist %>% 
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
      
      geom_sf(data = data_k_dist %>% 
                 filter(k5_names %in% c("3-stable cool", "4-variable cool")), 
              aes(size=dist_to_centroid, fill=k5_names, shape=k5_names), color="black", 
              alpha=0.97, show.legend = TRUE, inherit.aes = FALSE) +
      # add north arrow and scale bar
      annotation_north_arrow(location="br", 
                             width = unit(0.8,"cm"),
                             height=unit(1.2, "cm"),
                             pad_y = unit(0.9, "cm")) +
      annotation_scale(location="br") +
      scale_radius("Centroid \nDistance",range = c(2,8),
                        guide=guide_legend(order=2)) +
      scale_shape_manual("Thermal \nClasses", 
                         #values=c(22, 21, 24, 23, 8),
                         values=c(24, 23),
                         guide=guide_legend(order=1)) +
      scale_fill_manual("Thermal \nClasses", 
                        values=thermCols$color[3:4],
                        guide=guide_legend(override.aes = list(size=4), order=1)) +
      
      theme_map(base_family = "Roboto Condensed", base_size = 14) +
      theme(legend.background = element_rect(fill = NA),
            legend.box.background = element_rect(fill="white"),
            legend.key = element_rect(color = NA, size=NA),
            legend.position = c(0.03, 0.6), 
            legend.spacing.y = unit(0,"cm"),
            legend.margin = margin(0.2, 0.4, 0.2, 0.2, "cm"))
)


# save
ggsave(filename="output/figures/Fig_4_classification_map_cent_dist_panel_4.png", dpi=300, width=8, height = 7, units="in")


# PANEL 5: CENTROID DIST MAP -----------------------------------------------------

# fix shasta dam site to be just label

# make a bounding box in lat/lon
(mapRange5 <- c(range(st_coordinates(panel_areas[5,])[,1]),range(st_coordinates(panel_areas[5,])[,2])) +
    # add slight buffer around sites
    c(-0.03, .01, -0.05, 0.065))

# this only works with an API KEY
map5 <- get_map(location=c(mapRange5[1], mapRange5[3], mapRange5[2], mapRange5[4]), 
                crop = TRUE, # clips exactly to bounding box
                force = TRUE,
                #color="bw",
                maptype = "terrain-background",
                source="stamen",
                color="color", 
                zoom=11 
)

# quick view?
ggmap(map5)

# save as an object for later
save(map5, file = "output/13d_ggmap_base_layer_map5.rda")

# map
(map_pan_5 <- ggmap(map5) +
      #coord_sf(label_axes = "--EN", datum = 4326) +
      geom_sf(data=rivs, lwd=0.1, color="dodgerblue", 
              show.legend = FALSE, alpha=0.3, inherit.aes = FALSE) +
      geom_sf(data=mainstems_gage_all, lwd=0.3, color="dodgerblue", 
              show.legend = FALSE, alpha=0.9, inherit.aes = FALSE) + 
      geom_sf(data=dams_final, fill="black", pch=25, size=4.5, alpha=0.75, inherit.aes = FALSE) +
      geom_text_repel(data=data_k_dist %>% 
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
                      min.segment.length = 1.1,
                      segment.inflect=TRUE,
                      segment.angle = 25,
                      segment.ncp = 2,
                      segment.curvature = -0.1) +
      
      geom_sf(data = data_k_dist %>% 
                 filter(!k5_names %in% c("1-stable warm")), 
              aes(size=dist_to_centroid, fill=k5_names), pch=21, color="black", 
              alpha=0.97, show.legend = TRUE, inherit.aes = FALSE) +
      # geom_sf(data = data_k_dist %>% 
      #            filter(k5_names %in% c("5-stable cold")), 
      #         aes(size=dist_to_centroid, shape=k5_names), color="cyan4", 
      #         alpha=0.97, show.legend = FALSE, inherit.aes = FALSE) +
      # add north arrow and scale bar
      annotation_north_arrow(location="bl", 
                             width = unit(0.8,"cm"),
                             height=unit(1.2, "cm"),
                             pad_y = unit(0.9, "cm")) +
      annotation_scale(location="bl") +
      scale_radius("Centroid \nDistance", range = c(3,9),
                        guide=guide_legend(order=2)) +
      # scale_shape_manual("Thermal \nClasses", 
      #                    #values=c(22, 21, 24, 23, 8),
      #                    values=c(21, 24, 23, 8),
      #                    guide=guide_legend(order=1)) +
      scale_fill_manual("Thermal \nClasses", 
                        values=c(thermCols$color[2:5]),
                        guide=guide_legend(override.aes = list(size=4), order=1)) +
      
      theme_map(base_family = "Roboto Condensed", base_size = 14) +
      theme(
         #plot.margin = margin(0, 0, 0, 5, "cm"),
         legend.background = element_rect(fill = NA),
         legend.box.background = element_rect(fill="white"),
         legend.key = element_rect(color = NA, size=NA),
         legend.position = c(0.01, 0.1), 
         legend.spacing.y = unit(0,"cm"),
         legend.margin = margin(0.2, 0.2, 0.2, 0.2, "cm"))
)


# save
ggsave(filename="output/figures/Fig_4_classification_map_cent_dist_panel_5.png", dpi=300, width=8, height = 11.5, units="in")



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
