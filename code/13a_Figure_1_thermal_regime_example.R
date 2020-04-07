
# Code description --------------------------------------------------------

#Figure 1: An example of a thermal regime model fit to observed data. Cluster analysis is based on annual mean, amplitude, and phase metrics for each thermal regime model. Figure adapted from Maheu et al. (2016), figure 1.


# Libraries ---------------------------------------------------------------

library(tidyverse)
library(ggthemes)


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