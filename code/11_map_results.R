
# Code description --------------------------------------------------------

# This code add the Shasta coordinates to the gage list and maps the cluster results from code 10_calculate_model_metrics_for_classification


# Libraries ---------------------------------------------------------------

library(mapview)
library(sf)
library(tidyverse)
library(ggthemes)
library(ggrepel)
library(ggspatial)

#added these packages to interactively select dams:
library(mapedit)
library(leaflet)
library(leafpm)

# Load data ---------------------------------------------------------------

hydro_regions <- read_sf("data/shps//DWR_HydrologicRegions-utm11.shp") %>%
  st_transform(4326)

dams <- read_sf("data/shps/CA_OR_dams.shp", quiet = F) %>% st_transform(4326)

all_sites <- read_csv("data/data_review/gage_QA_progress.csv")

sites <- all_sites %>% 
  filter(grepl("QA complete", notes)) %>% 
  rename(station_id = site_id)

# get rivers
load("output/12_selected_nhd_mainstems_for_gages.rda")

# Add K_group Data --------------------------------------------------------

# different classification grouping method
load("output/models/agnes_k_groups_final.rda") # agnes
agnes_k_groups <- agnes_k_groups %>% 
  rename(station_id = site_id)


load("output/models/classification_group_results.rda") # class_groups
unique(agnes_k_groups$station_id)

# find differences between lists:
anti_join(sites, agnes_k_groups) # localities in sites df not in agnes_k_groups
anti_join(agnes_k_groups, sites) # localities in agnes_k_groups not in sites

# join and save
data_k <- left_join(agnes_k_groups, sites)

# thermColor scale
thermCols <- data.frame(k5_group_id = c(1,3,4,2,5),
                        k5_names  = c("1-stable warm", "2-reg warm",
                                      "3-reg cool", "4-unreg cool",
                                      "5-stable cold"),
                        color = I(c("#E41A1C", #stable warm
                                    "#FF7F00", #reg warm
                                    "#984EA3", #reg cool
                                    "#4DAF4A", #unreg cool
                                    "#377EB8" #stable cold
                        )))

data_k <- data_k %>% 
  mutate(k_5_f=factor(k_5,levels = c(1,3,4,2,5),
                    labels = c("1-stable warm","2-reg warm","3-reg cool",
                               "4-unreg cool", "5-stable cold"
                               ))) %>% 
  select(station_id, k_5, k_5_f, k_6:operator)


# how many per group?
table(data_k$k_5)
table(data_k$k_5_f)

# Thermal Classifications
# 
# "stable warm" = 1
# "reg warm"= 3, 
# "reg cool" = 4,
# "unreg cool"= 2, 
# "stable cold"= 5, 

# make spatial ------------------------------------------------------------

data_k_sf <- data_k %>% 
  st_as_sf(coords = c("lon", "lat"), crs = 4326, remove = FALSE)

# clip to only dams in CA
ca <- USAboundaries::us_boundaries(type="state", states="ca")
dams <- dams[ca,] # now clip (spatial join to only ca)

# get nearest dams
dams_nearest <- dams[st_nearest_feature(data_k_sf, dams),]

#filter dams that are irrelevant to the stream segment
dams_nearest_filtered <- dams_nearest %>% 
  filter(!OBJECTID %in% c(656, 613, 733, 655, 689, 739, 715, 589, 719, 218, 543, 670, 71, 665, 792, 213, 692, 199, 133, 731, 684, 761, 833, 676))

# save out data for future mapping
save(dams, dams_nearest_filtered, data_k_sf, file = "output/models/agnes_k_groups_sf_w_dams.rda")

# setup some basemaps
mapbases <- c("Stamen.TonerLite","OpenTopoMap", "CartoDB.PositronNoLabels", "OpenStreetMap",
              "Esri.WorldImagery", "Esri.WorldTopoMap","Esri.WorldGrayCanvas"
)
mapviewOptions(basemaps=mapbases)

# map k3
# m3 <-   mapview(dams_nearest, col.regions="black", alpha.regions=0.8,
#                 layer.name="Dams", cex=2,
#                 hide=TRUE, homebutton=FALSE)+
#   mapview(data_k_sf, zcol="k_3",
#         #col.regions=RColorBrewer::brewer.pal(3, "Set1") , 
#         burst=TRUE, hide=FALSE, homebutton=FALSE)
# m3@map %>% leaflet::addMeasure(primaryLengthUnit = "meters")

# map k5
m5 <- mapview(dams_nearest_filtered, col.regions="black",
                layer.name="Selected Dams", cex=6,
                hide=TRUE, homebutton=FALSE)+
  mapview(dams, col.regions="gray50", alpha.regions=0.5, cex=3.4, layer.name="All Dams") +
  mapview(mainstems_all, color="steelblue", cex=3, 
          layer.name="NHD Flowlines") +
  mapview(data_k_sf,  zcol="k_5_f", map.types=mapbases,
          layer.name="Thermal Classes",
          col.regions=unique(thermCols$color), 
          alpha.regions=0.8, cex=7,
          hide=FALSE, homebutton=FALSE) 

m5@map %>% leaflet::addMeasure(primaryLengthUnit = "meters")


# Select dams to add to dams_nearest_filtered (manually with mapedit) -------------

dams_selected = selectFeatures(dams, map = m5) #Commented out once features were selected to preserve object

dams_nearest_all <- rbind(dams_nearest_filtered, dams_selected)

#Review selected dams
m6 <- mapview(dams_nearest_all, col.regions="black",
              layer.name="Selected Dams", cex=6,
              hide=TRUE, homebutton=FALSE)+
  mapview(dams, col.regions="gray50", alpha.regions=0.5, cex=3.4, layer.name="All Dams") +
  mapview(mainstems_all, color="steelblue", cex=3, 
          layer.name="NHD Flowlines") +
  mapview(data_k_sf,  zcol="k_5_f", map.types=mapbases,
          layer.name="Thermal Classes",
          col.regions=unique(thermCols$color), 
          alpha.regions=0.8, cex=7,
          hide=FALSE, homebutton=FALSE) 

m6@map %>% leaflet::addMeasure(primaryLengthUnit = "meters")

#filter out downstream or unrelated dams
dams_nearest_all_filtered <- dams_nearest_all %>% 
  filter(!OBJECTID %in% c(98, 837, 868, 96, 592, 5, 720))

#update map
m7 <- mapview(dams_nearest_all_filtered, col.regions="black",
              layer.name="Selected Dams", cex=6,
              hide=TRUE, homebutton=FALSE)+
  mapview(dams, col.regions="gray50", alpha.regions=0.5, cex=3.4, layer.name="All Dams") +
  mapview(mainstems_all, color="steelblue", cex=3, 
          layer.name="NHD Flowlines") +
  mapview(data_k_sf,  zcol="k_5_f", map.types=mapbases,
          layer.name="Thermal Classes",
          col.regions=unique(thermCols$color), 
          alpha.regions=0.8, cex=7,
          hide=FALSE, homebutton=FALSE) 

m7@map %>% leaflet::addMeasure(primaryLengthUnit = "meters")

#Add New Melones
New_Melones = selectFeatures(dams, map = m7)

dams_nearest_all_filtered <-  rbind(dams_nearest_all_filtered, New_Melones)

#Add Big Springs Dam
Big_Springs_Dam = editMap(m7)

Big_Springs_Dam

Big_Springs_Dam_object <- Big_Springs_Dam$finished

Big_Springs_Dam_sf <- Big_Springs_Dam_object %>% 
  select(geometry) %>% 
  mutate(NAME = "Big Springs Dam", COUNTY = "Siskiyou", RIVER = "Big Springs Creek", OBJECTID = "NA", NID = "NA", HEIGHT_FT = "NA", STOR_AF = "NA", BASIN_SQMI = "NA", STO_m3 = "NA", STO_10_6m3 = "NA", NEAR_FID = "NA", NEAR_DIST = "NA", damname = "NA", damheight = "NA", nidstorage = "NA", file_nbr = "NA", basin_nbr = "NA", location = "NA", latitude = "NA", longitude = "NA", inspdate = "NA")

Big_Springs_Dam_sf <- Big_Springs_Dam_sf %>% 
  select(OBJECTID, NID, NAME, COUNTY, RIVER, HEIGHT_FT, STOR_AF, BASIN_SQMI, STO_m3, STO_10_6m3, NEAR_FID, NEAR_DIST, damname, damheight, nidstorage, file_nbr, basin_nbr, location, latitude, longitude,inspdate, geometry)

# Save final dams list ----------------------------------------------------


# save out data for future mapping
save(dams_nearest_all_filtered, file = "output/models/dams_nearest_all_filtered.rda")
save(Big_Springs_Dam_sf, file = "output/models/Big_Springs_Dam.rda")

#Try to combine dams_nearest_all_filtered and Big_Springs_Dam_sf
dams_nearest_all_final <- rbind(dams_nearest_all_filtered, Big_Springs_Dam_sf)

save(dams_nearest_all_final, file = "output/models/agnes_k_groups_sf_w_dams.rda")

#Plot and review - 

m8 <- mapview(dams_nearest_all_final, col.regions="black",
              layer.name="Selected Dams", cex=6,
              hide=TRUE, homebutton=FALSE)+
  #mapview(dams, col.regions="gray50", alpha.regions=0.5, cex=3.4, layer.name="All Dams") +
  mapview(mainstems_all, color="steelblue", cex=3, 
          layer.name="NHD Flowlines") +
  mapview(data_k_sf,  zcol="k_5_f", map.types=mapbases,
          layer.name="Thermal Classes",
          col.regions=unique(thermCols$color), 
          alpha.regions=0.8, cex=7,
          hide=FALSE, homebutton=FALSE) 

m8@map %>% leaflet::addMeasure(primaryLengthUnit = "meters")



# Static map for k5 --------------------------------------------------------------

ggplot()+
  geom_sf(data = hydro_regions, fill = NA, color = 'slategray4', size = 1, alpha = 0.4) +
  geom_sf(data = data_k_sf, aes(fill = k_5), pch = 21, size = 1) +
  geom_sf_text(data = hydro_regions, aes(label = HR_NAME), check_overlap = TRUE) +
  annotation_north_arrow(location = "tr", pad_y = unit(0.1, "cm")) +
  annotation_scale()

# Bring in Data -----------------------------------------------------------

load("output/models/annual_cluster_metrics_all_gages.rda")

# join with the groups
ann_metrics_k <- left_join(ann_metrics, agnes_k_groups) %>% 
  mutate(k_3 = as.factor(k_3),
         k_5 = as.factor(k_5))

# ann mean
(gg1 <- ggplot() + geom_boxplot(data=ann_metrics_k, aes(x=k_3, y=ann_mean, group=k_3, fill=k_3), show.legend = FALSE) + 
    theme_classic() + scale_fill_colorblind() +
    labs(x="K", y="Annual Mean Temperature (C)", 
         subtitle = "Annual Mean (k=3)"))

(gg2 <- ggplot() + geom_boxplot(data=ann_metrics_k, aes(x=k_5, y=ann_mean, group=k_5, fill=k_5), show.legend = FALSE) +
    theme_classic() + scale_fill_colorblind() + 
    labs(x="K", y="Annual Mean Temperature (C)",
         subtitle = "Annual Mean (k=5)"))

cowplot::plot_grid(gg1, gg2, nrow = 2)
ggsave("output/figures/boxplot_clusters_agnes_ann_mean.png", width = 8, height = 6, units="in", dpi=300)


# day of ann max
(gg1m <- ggplot() + geom_boxplot(data=ann_metrics_k, aes(x=k_3, y=DOWY, group=k_3, fill=k_3), show.legend = FALSE) +
    theme_classic() + scale_fill_colorblind() + 
    labs(x="K", y="Day of Annual Max. Temp", 
         subtitle = "Day of Annual Max"))

(gg2m <- ggplot() + geom_boxplot(data=ann_metrics_k, aes(x=k_5, y=DOWY, group=k_5, fill=k_5), show.legend = FALSE) +
    theme_classic() + scale_fill_colorblind() + 
    labs(x="K", y="Day of Annual Max. Temp",
         subtitle = "Day of Annual Max"))

cowplot::plot_grid(gg1m, gg2m, nrow = 2)
ggsave("output/figures/boxplot_clusters_agnes_day_ann_max.png", width = 8, height = 6, units="in", dpi=300)

# ann amp
(gg1am <- ggplot() + geom_boxplot(data=ann_metrics_k, aes(x=k_3, y=ann_amp, group=k_3, fill=k_3), show.legend = FALSE) +
  theme_classic() + scale_fill_colorblind() + 
  labs(x="K", y="Annual Amplitude (C)", 
       subtitle = "Annual Amplitude (Ann. Max - Ann Mean)"))

(gg2am <- ggplot() + geom_boxplot(data=ann_metrics_k, aes(x=k_5, y=ann_amp, group=k_5, fill=k_5), show.legend = FALSE) +
    theme_classic() + scale_fill_colorblind() +
  labs(x="K", y="Annual Amplitude (C)", 
       subtitle = "Annual Amplitude (Ann. Max - Ann Mean)"))

cowplot::plot_grid(gg1am, gg2am, nrow = 2)
ggsave("output/figures/boxplot_clusters_agnes_ann_amp.png", width = 8, height = 6, units="in", dpi=300)


## all stacked
cowplot::plot_grid(gg1, gg1am, gg1m, gg2, gg2am, gg2m,  nrow = 2)
ggsave("output/figures/boxplot_clusters_agnes_all.png", width = 11, height = 7, units="in", dpi=300)
ggsave("output/figures/boxplot_clusters_agnes_all.pdf", width = 11, height = 7, units="in", dpi=300)
