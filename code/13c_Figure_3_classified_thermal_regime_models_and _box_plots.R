
# Code description --------------------------------------------------------

# Figure 3: Annual thermal regime for each study site, grouped by class, and box plots showing the distribution of annual mean, amplitude, and phase for each class. Figure adapted from Maheu et al. (2016) figure 2.


# Libraries ---------------------------------------------------------------

library(tidyverse)
library(cowplot)
library(ggthemes)

# Figure 3: Classified thermal regimes ----------------------

#Figure 3a: annual thermal regime models, grouped by class
all_sites_model_data <- read_csv(file = "output/models/thermal_regime_models_daily.csv", col_types = list("c", "i", "n", "n"))

load("output/12_data_k_centdist_damdist.rda")


classification_group_results <- data_k_dist %>% 
  select(station_id, k_5, k5_names, color)

merge_models_and_classes <- left_join(all_sites_model_data, classification_group_results) 

# Annual max metrics for table --------------------------------------------


stable_warm <- merge_models_and_classes %>% 
  filter(k5_names == "1-stable warm")
max(stable_warm$model_avg_daily_temp_C)

reg_warm <- merge_models_and_classes %>% 
  filter(k5_names == "2-variable warm")
mean(max(reg_warm$model_avg_daily_temp_C))

reg_cool <- merge_models_and_classes %>% 
  filter(k5_names == "3-stable cool")
mean(max(reg_cool$model_avg_daily_temp_C))

unreg_cool <- merge_models_and_classes %>% 
  filter(k5_names == "4-variable cool")
mean(max(unreg_cool$model_avg_daily_temp_C))

stable_cold <- merge_models_and_classes %>% 
  filter(k5_names == "5-stable cold")
mean(max(stable_cold$model_avg_daily_temp_C))


# Model Fig (A) -----------------------------------------------------------

class_names <- c(
  "1-stable warm" = "stable warm",
  "2-variable warm" = "variable warm",
  "3-stable cool" = "stable cool",
  "4-variable cool" = "variable cool",
  "5-stable cold" = "stable cold"
)


(gg_top <- ggplot(data = merge_models_and_classes) + 
    geom_line(aes(x=DOWY, y=model_avg_daily_temp_C, group = station_id, color = color), show.legend = FALSE)+
    #facet_grid(cols = vars(k_5)) +
    facet_wrap(vars(k5_names), ncol = 5, labeller = labeller(k5_names = class_names)) +
    labs(x = "day of water year", 
         y = expression("modelled avg daily temp " (degree*C))) +
    theme_clean() + 
    theme(plot.background = element_blank(),
          axis.title.x.bottom = element_text(margin = margin(b=20, t=10))))

# Figure 3b: box plots of three regime metrics, grouped by class
load("output/models/09b_annual_cluster_metrics_all_gages.rda")

# join with the groups
ann_metrics_k <- left_join(ann_metrics, classification_group_results) %>% 
  mutate(k_5 = as.factor(k_5)) 
  
# add colors
thermCols <- with(ann_metrics_k, 
                  data.frame(k_5 = levels(k_5),
                             k5_names = levels(k5_names),
                             color = I(c("#E41A1C", #stable warm
                                         "#FF7F00", #variable warm
                                         "#984EA3", #stable cool
                                         "#4DAF4A", #variable cool
                                         "#377EB8" #stable cold
                             ))))


# Plot Ann Mean -----------------------------------------------------------

# now plot gg1
(gg1 <- ggplot() + 
    geom_boxplot(data=ann_metrics_k, 
                 aes(x=k5_names, y=ann_mean, group=k5_names, fill=k5_names), 
                 show.legend = TRUE) +
    scale_fill_manual("Thermal Classes", values=thermCols$color) +
    theme_classic() +
    annotate("text", y=26.5, x=1, label="stable warm", color="gray40", cex=3)+
    annotate("text", y=10, x=5, label="stable cold", color="gray40", cex=3)+
    labs(y=expression("annual mean " (degree*C)), x="") +
    theme(axis.text.x = element_blank(),
          legend.position = c(0.85,0.8)))

# option b
(gg1 <- ggplot() + 
    geom_boxplot(data=ann_metrics_k, 
                 aes(x=k5_names, y=ann_mean, group=k5_names, fill=k5_names), 
                 show.legend = FALSE) +
    scale_fill_manual("Thermal Classes", values=thermCols$color) +
    theme_classic() +
    annotate("text", y=26.3, x=1, label="stable warm", color=thermCols$color[1], cex=2, fontface=2)+
    annotate("text", y=20, x=2, label="variable\nwarm", color=thermCols$color[2], cex=2, fontface=2)+
    annotate("text", y=16, x=3, label="stable\ncool", color=thermCols$color[3], cex=2, fontface=2)+
    annotate("text", y=14.5, x=4, label="variable\ncool", color=thermCols$color[4], cex=2, fontface=2)+
    annotate("text", y=13, x=5, label="stable cold", color=thermCols$color[5], cex=2, fontface=2)+
    labs(x="thermal class", y=expression("annual mean " (degree*C)), x="") +
    theme(axis.text.x = element_blank(),
          legend.position = c(0.85,0.8)))

# Day of Ann Max -----------------------------------------------------------------

(gg2 <- ggplot() + geom_boxplot(data=ann_metrics_k, aes(x=k5_names, y=DOWY, group=k5_names, fill=color), show.legend = FALSE) +
   theme_classic() + 
   annotate("text", y=340, x=1, label="stable warm", color=thermCols$color[1], cex=2, fontface=2)+
   annotate("text", y=260, x=2, label="variable\nwarm", color=thermCols$color[2], cex=2, fontface=2)+
   annotate("text", y=260, x=3, label="stable\ncool", color=thermCols$color[3], cex=2, fontface=2)+
   annotate("text", y=260, x=4, label="variable\ncool", color=thermCols$color[4], cex=2, fontface=2)+
   annotate("text", y=165, x=5, label="stable cold", color=thermCols$color[5], cex=2, fontface=2)+
   labs(y="day of annual max", x="thermal class") +
   theme(axis.text.x = element_blank()))


# Ann Amplitude -----------------------------------------------------------

(gg3 <- ggplot() + 
   geom_boxplot(data=ann_metrics_k, aes(x=k5_names, y=ann_amp, group=k5_names, fill=color), show.legend = FALSE) +
   theme_classic() +
   annotate("text", y=2.2, x=1, label="stable warm", color=thermCols$color[1], cex=2, fontface=2)+
   annotate("text", y=5, x=2, label="variable\nwarm", color=thermCols$color[2], cex=2, fontface=2)+
   annotate("text", y=6.9, x=3, label="stable\ncool", color=thermCols$color[3], cex=2, fontface=2)+
   annotate("text", y=4, x=4, label="variable\ncool", color=thermCols$color[4], cex=2, fontface=2)+
   annotate("text", y=2.2, x=5, label="stable cold", color=thermCols$color[5], cex=2, fontface=2)+
   labs(x="thermal class",y=expression("annual amplitude " (degree*C)), x="") + 
   theme(axis.text.x = element_blank()))

(fig_row_2 <- plot_grid(gg1, gg2, gg3, labels = c("B", "C", "D"), ncol = 3, vjust = -0.25, hjust = -1.5))

plot_grid(gg_top,fig_row_2, labels = c("A"),
          nrow = 2, rel_heights = c(1.5,1), hjust = -1.5)


#save
ggsave("output/figures/Fig_3_classified_models_and_box_plots.jpeg", width = 9, height = 6.5, units = "in", dpi=600)
