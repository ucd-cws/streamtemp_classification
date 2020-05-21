## Calculate Metrics from Modeled Data for Classification
# following Maheu et al. (2015) 
# - annual mean
# - annual amplitude (annual max - annual mean)
# - day of annual maximum

# Libraries ---------------------------------------------------------------

library(tidyverse)
library(tidylog)

# Get Data ----------------------------------------------------------------

load("output/models/thermal_regime_models_daily.rda")

# do some basic cleaning to drop attributes
model_out <- model_out %>% as.data.frame()


# Calculate Annual Mean & Amplitude By Gage ------------------------------

# annual amplitude is the annual max - annual mean
ann_metrics <- model_out %>% 
  group_by(station_id) %>% 
  summarize(ann_mean = mean(model_avg_daily_temp_C),
            ann_max = max(model_avg_daily_temp_C),
            ann_amp = ann_max - ann_mean)

# visualize:
(gg1 <- ggplot() + 
  geom_point(data=ann_metrics, aes(x=reorder(station_id, ann_amp), y=ann_amp), pch=21, fill="purple2", size=2.5, alpha=0.8) +
    coord_flip() +
    theme_classic(base_family = "Roboto Condensed") + # may need to change this to "Helvetica" or "Arial"
    theme(legend.position = c(.1, 0.8),
          axis.text.y = element_text(size=7),
          panel.grid.major.y = element_line(color="gray80", size=0.1))+
    labs(x="Station ID", 
         y="Annual Amplitude (ann. max - ann. mean) (C)"))
  
(gg2 <- ggplot() +
    geom_point(data=ann_metrics, aes(x=reorder(station_id, ann_mean), y=ann_mean), color="darkblue",size=2.5, alpha=0.8) +
      coord_flip()+
      theme_classic(base_family = "Roboto Condensed") +
      theme(legend.position = c(.1, 0.8),
            axis.text.y = element_text(size=7),
            panel.grid.major.y = element_line(color="gray80", size=0.1))+
      labs(x="", 
           y="Annual Mean (C)"))

library(cowplot)
(pg1 <- plot_grid(gg1, gg2, labels = c("A","B")))

save_plot(pg1, filename = "output/figures/ann_mean_ann_amp_cowplot.png",base_height = 6, dpi=300)

# Calculate Day of Ann Max ------------------------------------------------

ann_max_day <- model_out %>% 
  group_by(station_id) %>% 
  slice(which.max(model_avg_daily_temp_C)) %>% 
  ungroup()

#library(plotly)
#ggplotly(
  ggplot() +
  geom_point(data=ann_max_day, aes(x=reorder(station_id, DOWY), y=DOWY, 
                                   color=model_avg_daily_temp_C)) +
    coord_flip()+
    scale_color_viridis_c("Modeled Avg \n Daily Temp (C)") +
    theme_classic() +
    theme(legend.position = c(.1, 0.8),
          panel.grid.major.y = element_line(color="gray80", size=0.1))+
    labs(x="Station ID", 
         y="Day of Annual Max (Day of Water Year)")
#)
ggsave("output/figures/day_of_ann_max_dowy.png", dpi=300, width = 9, height = 7, units = "in")

# Join Together -----------------------------------------------------------

ann_metrics <- left_join(ann_metrics, ann_max_day)

# should be 77 rows for 77 sites!
nrow(ann_metrics)

# save
save(ann_metrics, file = "output/models/09b_annual_cluster_metrics_all_gages.rda")

# done! 
