# plot boxplots

library(tidyverse)
library(ggthemes)

# Bring in Data -----------------------------------------------------------

load("output/models/10a_agnes_k_groups_final.rda")
load("output/models/09b_annual_cluster_metrics_all_gages.rda")

# join with the groups
ann_metrics_k <- left_join(ann_metrics, agnes_k_groups, by=c("station_id"="site_id")) %>% 
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
