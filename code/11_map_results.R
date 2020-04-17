
# Code description --------------------------------------------------------

# This code add the Shasta coordinates to the gage list and maps the cluster results from code 10_calculate_model_metrics_for_classification


# Libraries ---------------------------------------------------------------

library(mapview)
library(sf)
library(tidyverse)
library(ggthemes)
library(ggrepel)
library(ggspatial)

# Load data ---------------------------------------------------------------

hydro_regions <- read_sf("data/shps//DWR_HydrologicRegions-utm11.shp") %>%
  st_transform(4326)

dams <- read_sf("data/shps/CA_OR_dams.shp", quiet = F) %>% st_transform(4326)

all_sites <- read_csv("data/data_review/gage_QA_progress.csv")

sites <- all_sites %>% 
  filter(grepl("QA complete", notes)) %>% 
  rename(station_id = site_id)

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
                        k5_names  = c("stable warm", "reg warm",
                                      "reg cool", "unreg cool",
                                      "stable cold"),
                        color = I(c("#E41A1C", #stable warm
                                    "#FF7F00", #reg warm
                                    "#984EA3", #reg cool
                                    "#4DAF4A", #unreg cool
                                    "#377EB8" #stable cold
                        )))

data_k <- data_k %>% 
  mutate(k_5=factor(k_5,
                    labels = c("stable warm","unreg cool",
                               "reg warm", "reg cool",
                               "stable cold"
                               )))


# how many per group?
table(data_k$k_3)
table(data_k$k_5)

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

# get nearest dams
dams_nearest <- dams[st_nearest_feature(data_k_sf, dams),]

# save out data for future mapping
save(dams, dams_nearest, data_k_sf, file = "output/models/agnes_k_groups_sf_w_dams.rda")

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
m5 <- mapview(dams_nearest, col.regions="black", alpha.regions=0.8,
                layer.name="Dams", cex=2,
                hide=TRUE, homebutton=FALSE)+
  mapview(data_k_sf,  zcol="k_5", map.types=mapbases,
          col.regions=RColorBrewer::brewer.pal(5, "Set1") , 
          burst=TRUE, hide=FALSE, homebutton=FALSE)
  
m5@map %>% leaflet::addMeasure(primaryLengthUnit = "meters")



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
