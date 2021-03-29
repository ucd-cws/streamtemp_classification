# Code description --------------------------------------------------------

# Figure 5: Map of study locations and k-5 clusters in CA.

# Libraries ---------------------------------------------------------------

library(sf)
library(tidyverse)
library(ggthemes)
library(ggspatial)
library(mapview)
library(purrr)
library(ggrepel)
#library(wateRshedTools)

# setup some basemaps
mapbases <- c("Stamen.TonerLite", 
              "CartoDB.PositronNoLabels", "OpenStreetMap",
              "Esri.WorldImagery", "Esri.WorldTopoMap")
# and set defaults
mapviewOptions(basemaps=mapbases, fgb = FALSE)


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

# test:
mapview(data_k_dist[data_k_dist$k_5==2, ],  zcol="site_name",
        alpha.regions=0.8, cex=3.5,
        hide=FALSE, homebutton=FALSE)

# Panels ------------------------------------------------------------------


# load("output/dam_panel_areas.rda")
# # fix and save:
# panel_areas <- panel_areas$finished %>% st_set_crs(4326) %>% 
#    select(-X_leaflet_id) %>% 
#    rownames_to_column("panelID") %>% 
#    mutate(panelID = as.integer(panelID))
# mapview(panel_areas)
#save(panel_areas,file = "output/dam_panel_areas_revised.rda")
load("output/dam_panel_areas_revised.rda")

# add panel names
panel_areas <- panel_areas %>% 
   mutate(panel_name= case_when(
      panelID==5 ~ "A",
      panelID==4 ~ "B", 
      panelID==3 ~ "C",
      panelID==2 ~ "D",
      panelID==1 ~ "E"
      ))

# make UTM for easier buffer
panel_areas <- panel_areas %>% st_transform(3310)

# buffer panel E slightly (50 km)
panel_areas_1 <- panel_areas[1, ] %>% st_make_grid(., n = 1) %>% st_buffer(dist = 20000, endCapStyle = "FLAT", joinStyle = "MITRE", nQuadSegs = 1) %>% st_as_sf()
panel_areas[1,3] <- panel_areas_1

panel_areas <- st_transform(panel_areas, 4326)

# add centroid labels
panel_areas <- panel_areas %>% 
   # add centroid values for labels using the geometry column
   mutate(lon=map_dbl(geometry, ~st_centroid(.x)[[1]]), 
          lat=map_dbl(geometry, ~st_centroid(.x)[[2]])) 

rm(panel_areas_1)

# preview
mapview(panel_areas, zcol="panel_name") +
   mapview(data_k_dist,  zcol="k5_names", burst=TRUE, 
           col.regions=data_k_dist$color[order(data_k_dist$k5_names)],
           layer.name="Thermal Classes",
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

# Get and Crop Rivers by State --------------------------------------------

# get CA and County Boundaries
ca <- USAboundaries::us_boundaries(type="state", states="ca", resolution = "high")
ca_co <- USAboundaries::us_counties(states="ca") %>% st_transform(crs_proj)

# crop rivs by ca
rivs <- st_intersection(st_transform(rivs, 4326), ca$geometry)

# crop the other rivers data set
rivers_ca <- st_transform(rivers, st_crs(ca)) %>% st_intersection(., ca$geometry)
#summary(rivers_ca$streamorde)

# plot(ca$geometry, border="gray40")
# plot(rivers_ca$geometry, col="steelblue", lwd = (rivers_ca$streamorde / 4), add=TRUE)
# plot(rivs$Shape, col="dodgerblue", lwd = 0.2, add=TRUE)

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

# crop CA for just points:
data_k_dist <- st_transform(data_k_dist, 4326)
data_k_bbox <- st_make_grid(data_k_dist, n = 1) %>% st_buffer(dist = 0.5)

ca_crop <- st_intersection(ca, data_k_bbox) # rounded box
#ca_crop <- st_crop(ca, st_bbox(data_k_dist)) # bbox
hydro <- st_intersection(hydro, data_k_bbox)
#mapview(data_k_bbox) + mapview(data_k_dist) + mapview(ca_crop)
rivers_ca <- st_transform(rivers_ca, 4326) %>% st_intersection(., data_k_bbox)
rivs <- st_transform(rivs, 4326) %>% st_intersection(., data_k_bbox)
mainstems_gage_all <- st_transform(mainstems_gage_all, 4326) %>% st_intersection(., data_k_bbox)
dams_final <- st_transform(dams_final, 4326)


# use these symbols:15, 16, 17, 18, 8

(map1 <- ggplot() +
      #coord_sf(label_axes = "--EN", datum = 4326) +
      geom_sf(data=hydro, aes(lty="Hydroregions"), color=alpha("black", 0.8), fill=NA, size=0.25) +
      scale_linetype_manual("", values = c("Hydroregions" = 2), 
                            guide = guide_legend(override.aes = list(color = "black", alpha=0.5, lwd=0.5), order=2)) +
      geom_sf(data=rivs, lwd=0.1, color="dodgerblue", show.legend = FALSE, alpha=0.3) +
      geom_sf(data=rivers_ca, lwd=0.3, color="dodgerblue", show.legend = FALSE, alpha=0.45) + 
      geom_sf(data=mainstems_gage_all, lwd=0.3, color="dodgerblue", show.legend = FALSE, alpha=0.9) + 
      geom_sf(data=ca_crop, fill = NA, color = 'slategray4', size = 0.7, alpha = 0.3) +
      geom_sf(data=panel_areas, fill = "gray", color="slategray", alpha=0.2) +
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
      # hydroregions labels
      geom_text_repel(data=hydro, aes(x=lon, y=lat, label=HR_NAME), 
                      # need to tweak these individually, a bit annoying
                      nudge_x = c(-.2, # north coast, 
                                  -0.5, # sf bay
                                  1, # san joaq
                                  -0.1, # central coast
                                  -0.3, # tulare
                                  0.8, # sacto
                                  0, # N lahontan
                                  -1), # S lahontan
                      nudge_y = c(0.8, 0, -0.25, 0.4, 0.2, 0, 0.7, 2),
                      fontface="bold", family="Roboto Condensed",
                      segment.color="gray10",
                      color = "gray25",     # text color
                      bg.color = "gray90", # shadow color
                      bg.r = 0.05,
                      size=4,
                      box.padding = 0.5,
                      force=1.5,
                      min.segment.length = 15,
                      #segment.inflect = FALSE, 
                      #segment.square=FALSE,
                      segment.ncp = 3,
                      segment.curvature = -0.1) +
      # panel labels
      geom_text_repel(data=panel_areas, aes(x=lon, y=lat, label=panel_name),
                      fontface="bold.italic", family="Roboto Condensed",
                      segment.color="gray10",
                      color = "black",     # text color
                      bg.color = "grey90", # shadow color
                      bg.r = 0,
                      size=6.5, force=1.5, min.segment.length = 10,
                      nudge_x = c(0.2, # E 
                                  0.34, # D
                                  -0.3, # C 
                                  -0.05, # B
                                  -0.1), # A
                      nudge_y = c(0.2, -0.2, 0.12, 0.05, -0.35)) +

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
            legend.position = c(0.02, 0.05), 
            legend.spacing.y = unit(0,"cm"),
            legend.margin = margin(0.1, 0, 0, 0, "cm"))
)

# save
ggsave(filename="output/figures/Fig_5_classification_map_hydroregions_all_rivers.pdf", dpi=300, width=8.5, height = 11, units="in", device=cairo_pdf)
ggsave(filename="output/figures/Fig_5_classification_map_hydroregions_all_rivers.png", dpi=300, width=8.5, height = 11, units="in")


# ADD INSET ---------------------------------------------------------------

us <- USAboundaries::us_boundaries(type="state", resolution = "low") %>% 
   filter(!state_abbr %in% c("PR", "AK", "HI"))

# make a box around rivers (a grid with an n=1) for inset
ca_box <- st_make_grid(data_k_dist, n = 1) #%>% st_centroid()

# Inset map: US
(p2 <- ggplot() + 
   geom_sf(data = us, colour = "grey10", fill = "tan", alpha=0.4) +
   coord_sf() +
   theme_minimal() + 
   geom_sf(data=ca_box, color="purple4", fill=NA, size=1.5, alpha=0.7) +
   labs(x = NULL, y = NULL) +
   theme(axis.text.x = element_blank(),
         axis.text.y = element_blank(),
         axis.ticks = element_blank(),
         axis.title.x = element_blank(),
         axis.title.y = element_blank(),
         panel.grid.major = element_line(colour = "transparent"),
         plot.background = element_rect(color = "white", fill="white"),
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
jpeg(filename = "output/figures/Fig_5_classification_map_hydroregions_inset.jpg", width = 8.5, height = 11, units = "in", res = 300)

# to just view, start from below here
grid.newpage()
mainmap <- viewport(width = 1, height = 1, x = 0.5, y = 0.5) # main map
insetmap <- viewport(width = 0.32, height = 0.28, x = 0.8, y = 0.8) # inset
print(map1, vp = mainmap) 
print(p2, vp = insetmap)

# make sure to run this if saving out as pdf/png
dev.off()

## TIFF
tiff(filename = "output/figures/PLOS_ONE/Fig5_MainMap_hydroregions_inset_200dpi.tiff", width = 7.5, height = 8, units = "in", res = 200)

# to just view, start from below here
grid.newpage()
mainmap <- viewport(width = 1, height = 1, x = 0.5, y = 0.5) # main map
insetmap <- viewport(width = 0.32, height = 0.28, x = 0.8, y = 0.8) # inset
print(map1, vp = mainmap) 
print(p2, vp = insetmap)

# make sure to run this if saving out as pdf/png
dev.off()


# Save Map Files ----------------------------------------------------------

save(ca, ca_box, ca_co, ca_crop, dams_final, data_k_bbox, data_k_dist, ds_main_merged, hydro, mainstems_gage_all, panel_areas, rivers, rivers_ca, rivs, thermCols, file = "output/13d_main_map_data_pieces.rda")
