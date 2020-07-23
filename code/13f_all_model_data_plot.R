
# Code description --------------------------------------------------------

# This code makes a plot of all the annual trend data used to fit thermal regime models. Code developed to make figures for presentations; not intended for manuscript.


# Libraries ---------------------------------------------------------------

library(mapview)
library(sf)
library(tidyverse)
library(tmap)
library(ggrepel)
library(ggspatial)

library(lubridate)
library(plotly)
library(purrr)


# Load data for annual trends ---------------------------------------------------------------

# See code 8 for annual trend data processing

all_annual_trend_data <- readRDS("data/model_data/all_sites_model_data.rds")

ggplotly(
  ggplot() + 
    geom_line(data=all_annual_trend_data, aes(x=DOWY, y=mean_temp_C, color=station_id), show.legend = F) +
    scale_color_viridis_d())

Sac_11390500_model_data <- all_annual_trend_data %>% 
  filter(station_id == 11390500)


# Plot annual trend data --------------------------------------------------

ggplot(data = Sac_11390500_model_data) +
  geom_line(aes(x=DOWY, y = mean_temp_C, group=station_id), color="darkblue", alpha=0.5) +
  #geom_point(aes(x=DOWY, y = model_avg_daily_temp_C, group=station_id), color="maroon", size=1)+
  labs(x = "day of water year", y = "daily mean stream temperature, deg C") +
  theme_classic()

ggplot(data = Sac_11390500_model_data) +
  geom_line(aes(x=DOWY, y = mean_temp_C, group=station_id), color="darkblue", alpha=0.5) +
  ylim(8,22) +
  xlim(1,366) +
  labs(x = "day of water year", 
       y = expression("daily mean stream temperature " (degree*C))) +
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())

# save
ggsave("output/figures/Fig_1a_annual_trend_data.jpeg", width = 6, height = 3.5, units = "in", dpi=300)


# Load data for thermal regime models -------------------------------------

load("output/models/thermal_regime_models_daily.rda")

ggplot(data = model_out) +
  # geom_line(aes(x=DOWY, y = mean_temp_C, group=station_id), color="darkblue", alpha=0.5) +
  geom_point(aes(x=DOWY, y = model_avg_daily_temp_C, group=station_id), color="maroon", size=0.5)+
  labs(x = "julian day", y = expression("daily mean stream temperature " (degree*C))) +
  theme_classic()

#save
ggsave("output/figures/Fig_1b_all_thermal_regime_models.jpeg", width = 6, height = 3.5, units = "in", dpi = 300)
