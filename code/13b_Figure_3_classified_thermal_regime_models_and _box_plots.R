
# Code description --------------------------------------------------------

# Figure 3: Annual thermal regime for each study site, grouped by class, and box plots showing the distribution of annual mean, amplitude, and phase for each class. Figure adapted from Maheu et al. (2016) figure 2.


# Libraries ---------------------------------------------------------------

library(tidyverse)
library(cowplot)

# Figure 3: Classified thermal regimes ----------------------

#Figure 3a: annual thermal regime models, grouped by class
all_sites_model_data <- read_csv(file = "output/models/thermal_regime_models_daily.csv", col_types = list("c", "i", "n", "n"))

load("output/models/agnes_k_groups_v2_w_selected_dams.rda")

classification_group_results <- data_k_sf_v2 %>% 
  select(station_id, k_5, color)

merge_models_and_classes <- left_join(all_sites_model_data, classification_group_results) %>% 
  filter(!is.na(k_5))

(gg_top <- ggplot(data = merge_models_and_classes) + 
  geom_line(aes(x=DOWY, y=model_avg_daily_temp_C, group = station_id, color = color), show.legend = FALSE)+
  facet_grid(cols = vars(k_5)) +
  labs(x = "day of water year", y = "modelled avg daily temp, deg C") +
  theme_minimal()) 

# Figure 3b: box plots of three regime metrics, grouped by class
load("output/models/annual_cluster_metrics_all_gages.rda")

# join with the groups
ann_metrics_k <- left_join(ann_metrics, classification_group_results) %>% 
  mutate(k_5 = as.factor(k_5)) %>% 
  filter(!is.na(k_5))
  

(gg1 <- ggplot() + geom_boxplot(data=ann_metrics_k, aes(x=k_5, y=ann_mean, group=k_5, fill=color), show.legend = FALSE) +
  theme_classic() + 
  labs(y="annual mean, deg C") +
       theme(axis.title.x = element_blank(),axis.text.x = element_text(angle = 45, hjust=1)))

(gg2 <- ggplot() + geom_boxplot(data=ann_metrics_k, aes(x=k_5, y=DOWY, group=k_5, fill=color), show.legend = FALSE) +
    theme_classic() + 
    labs(y="day of annual max") +
    theme(axis.title.x = element_blank(),axis.text.x = element_text(angle = 45, hjust=1)))

(gg3 <- ggplot() + geom_boxplot(data=ann_metrics_k, aes(x=k_5, y=ann_amp, group=k_5, fill=color), show.legend = FALSE) +
    theme_classic() +
    labs(y="annual amplitude, deg C") + 
    theme(axis.title.x = element_blank(),axis.text.x = element_text(angle = 45, hjust=1)))

fig_row_2 <- plot_grid(gg1, gg2, gg3, labels = c("B", "C", "D"), ncol = 3, vjust = -0.25, hjust = -1.5)

plot_grid(gg_top,fig_row_2,labels = c("A"), nrow = 2, rel_heights = c(1.5,1), hjust = -1.5)

ggsave("output/figures/Fig_3_classified_models_and_box_plots.jpeg", width = 8, height = 6.5, units = "in", dpi=300)
