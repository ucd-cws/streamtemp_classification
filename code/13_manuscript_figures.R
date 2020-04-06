
# Code description --------------------------------------------------------

# This code is to create or tweak figures for Willis, Peek, and Rypel (in development).


# Libraries ---------------------------------------------------------------

library(tidyverse)


# Figure 1: Thermal regime model ------------------------------------------

all_sites_model_data <- read_csv(file = "output/models/thermal_regime_models_daily.csv", col_types = list("c", "i", "n", "n"))

ggplot(data = all_sites_model_data) +
  geom_line(aes(x=DOWY, y = mean_temp_C, group=station_id), color="darkblue", alpha=0.5) +
  geom_point(aes(x=DOWY, y = model_avg_daily_temp_C, group=station_id), color="maroon", size=1)+
  labs(x = "julian day", y = "daily mean stream temperature, deg C") +
  theme_classic()

Sac_11390500_model_data <- all_sites_model_data %>% 
  filter(station_id == 11390500)

ggplot(data = Sac_11390500_model_data) +
  geom_line(aes(x=DOWY, y = mean_temp_C, group=station_id), color="darkblue", alpha=0.5) +
  geom_point(aes(x=DOWY, y = model_avg_daily_temp_C, group=station_id), color="maroon", size=1)+
  labs(x = "day of water year", y = "daily mean stream temperature, deg C") +
  theme_classic()

annual_mean <- mean(Sac_11390500_model_data$model_avg_daily_temp_C)
annual_max <- max(Sac_11390500_model_data$model_avg_daily_temp_C)
amplitude <- annual_max - annual_mean
day_of_annual_max <- Sac_11390500_model_data[Sac_11390500_model_data$model_avg_daily_temp_C == annual_max, 2]

#Still want to add a double-headed arrouw to show the amplitude

ggplot(data = Sac_11390500_model_data) +
  geom_line(aes(x=DOWY, y = mean_temp_C, group=station_id), color="darkblue", alpha=0.5) +
  geom_point(aes(x=DOWY, y = model_avg_daily_temp_C, group=station_id), color="maroon", size=1)+
  geom_segment(aes(x = 293, y = 8, xend = 293, yend = annual_max), linetype = "dashed") +
  geom_segment(aes(x = 1, y = annual_mean, xend = 200, yend = annual_mean), linetype = "dashed") +
  geom_segment(aes(x = 1, y = annual_max, xend = 293, yend = annual_max), linetype = "dashed") +
  ylim(8,22) +
  xlim(1,366) +
  labs(x = "day of water year", y = "daily mean stream temperature, deg C") +
  annotate("text", x = 220, y = annual_mean, label = "mean") +
  annotate("text", x = 130, y = annual_mean+2.5, label = "amplitude") +
  annotate("text", x = 315, y = annual_mean-2, label = "phase") +
  theme_classic() +
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())

ggsave("output/figures/Fig_1_thermal_regime_model.jpeg", width = 6, height = 3.5, units = "in", dpi=300)


# Figure 2: Classified thermal regimes and box plots ----------------------

classification_group_results <- read_csv(file = "output/models/classification_group_results.csv")

classification_group_results <- classification_group_results %>% 
  select(station_id, k_5)

merge_models_and_classes <- left_join(all_sites_model_data, classification_group_results)

ggplot(data = merge_models_and_classes) + 
  geom_line(aes(x=DOWY, y=model_avg_daily_temp_C), color="darkblue")+
  facet_wrap(k_5 ~ .)

class_1 <- merge_models_and_classes %>% 
  filter(k_5 == 1)

class_5 <- merge_models_and_classes %>% 
  filter(k_5 == 5)

#Something is wrong here. Big Springs ended up classified with Hot Creek, and Shasta Dam outlet ended up in its own class. Check with Ryan. Also need to fix formatting of color - it's filling, rather than plotting discrete lines.

#Next steps: add box plots
