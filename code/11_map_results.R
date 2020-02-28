
# Code description --------------------------------------------------------

# This code add the Shasta coordinates to the gage list and maps the cluster results from code 10_calculate_model_metrics_for_classification


# Libraries ---------------------------------------------------------------

library(mapview)
library(sf)
library(tidyverse)
library(ggthemes)

# Load data ---------------------------------------------------------------

hydro_regions <- st_read("data/DWR_HydrologicRegions-utm11.shp") %>%
  st_transform(4326)

all_sites <- read_csv("data/data_review/gage_QA_progress.csv")

sites <- all_sites %>% 
  filter(grepl("QA complete", notes)) %>% 
  rename(station_id = site_id)

# Add K_group Data --------------------------------------------------------

# different classification grouping method
load("output/models/agnes_k_groups_final.rda") # agnes
agnes_k_groups <- agnes_k_groups %>% 
  rename(station_id = site_id)


#load("output/models/classification_group_results.rda") # class_groups
unique(agnes_k_groups$station_id)

# find differences between lists:
anti_join(sites, agnes_k_groups) # localities in sites df not in agnes_k_groups
anti_join(agnes_k_groups, sites) # localities in agnes_k_groups not in sites

# join and save
data_k <- left_join(sites, agnes_k_groups)

# how many per group?
table(data_k$k_3)
table(data_k$k_5)

# make spatial ------------------------------------------------------------

data_k_sf <- data_k %>% 
  st_as_sf(coords = c("lon", "lat"), crs = 4326, remove = FALSE)

# map k3
mapview(data_k_sf, zcol="k_3", 
        #col.regions=RColorBrewer::brewer.pal(3, "Set1") , 
        burst=TRUE, hide=FALSE, homebutton=FALSE) #+
  #mapview(hydro_regions, col.regions = NA, legend=FALSE)

# map k5
mapview(data_k_sf,  zcol="k_5",
        col.regions=RColorBrewer::brewer.pal(5, "Set1") , 
        burst=TRUE, hide=FALSE, homebutton=FALSE) #+
  #mapview(hydro_regions, col.regions = NA, legend=FALSE)



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
cowplot::plot_grid(gg1, gg1m, gg1am, gg2, gg2m, gg2am, nrow = 2)
ggsave("output/figures/boxplot_clusters_agnes_all.png", width = 11, height = 7, units="in", dpi=300)
ggsave("output/figures/boxplot_clusters_agnes_all.pdf", width = 11, height = 7, units="in", dpi=300)
