# Code description  ---------------------------------------------------------------

# This code looks at the results of the cluster analysis and extracts details for each class that are used to describe each thermal regime in the manuscript.

# Libraries ---------------------------------------------------------------

library(tidyverse)
library(tidylog)

# Figure 2a: the x-y plot of clustered thermal regimes --------------------

load("output/models/09b_annual_cluster_metrics_all_gages.rda")
load("output/models/10a_agnes_k_groups_final.rda")

#review data
summary(ann_metrics)

#make a date column, only month and day of water year
ann_metrics$date <- format(as.Date(ann_metrics$DOWY, origin = "2018-10-01"), "%m-%d")

#add thermal regime class numbers, drop cols we don't need
thermal_regime_parameters <- left_join(agnes_k_groups, ann_metrics, by=c("site_id"="station_id")) %>% 
  rename(station_id = site_id) %>% 
  select(station_id, ann_mean, ann_max, DOWY, date, k_5)

#Stable warm features
thermal_regime_parameters[thermal_regime_parameters$k_5=="1",2:5]

#Stable cold features
stable_cold <- thermal_regime_parameters %>% 
  filter(k_5 == 5) %>% 
  summarize(mean_max = mean(ann_max), mean_DOWY = mean(DOWY), annual_mean = mean(ann_mean))

stable_cold$date <- format(as.Date(stable_cold$mean_DOWY, origin = "2018-10-01"), "%m-%d")

# variable warm
variable_warm <- thermal_regime_parameters %>% 
  filter(k_5 == 3) %>% 
  count()
  
variable_warm <- thermal_regime_parameters %>% 
  filter(k_5 == 3) %>%
  summarize(mean_max = mean(ann_max), mean_DOWY = mean(DOWY), annual_mean = mean(ann_mean)) 

variable_warm$date <- format(as.Date(variable_warm$mean_DOWY, origin = "2018-10-01"), "%m-%d")

variable_warm <- thermal_regime_parameters %>% 
  filter(k_5 == 3)

variable_warm$date <- format(as.Date(variable_warm$DOWY, origin = "2018-10-01"), "%m-%d")

max(variable_warm$ann_max)
min(variable_warm$ann_max)

max(variable_warm$ann_mean)
min(variable_warm$ann_mean)

DOWY_max <- max(variable_warm$DOWY)
DOWY_min <- min(variable_warm$DOWY)

format(as.Date(DOWY_max, origin = "2018-10-01"), "%m-%d")
format(as.Date(DOWY_min, origin = "2018-10-01"), "%m-%d")

summary(variable_warm)

format(as.Date(298, origin = "2018-10-01"), "%m-%d")

variable_warm[variable_warm$DOWY == DOWY_max, 1:3]

# stable cool
stable_cool <- thermal_regime_parameters %>% 
  filter(k_5 == 4)

stable_cool$date <- format(as.Date(stable_cool$DOWY, origin = "2018-10-01"), "%m-%d")

summary(stable_cool)

format(as.Date(309, origin = "2018-10-01"), "%m-%d")
format(as.Date(283, origin = "2018-10-01"), "%m-%d")

# variable_cool
variable_cool <- thermal_regime_parameters %>% 
  filter(k_5 == 2)

summary(variable_cool)

format(as.Date(305, origin = "2018-10-01"), "%m-%d")
format(as.Date(min(variable_cool$DOWY), origin = "2018-10-01"), "%m-%d")
format(as.Date(max(variable_cool$DOWY), origin = "2018-10-01"), "%m-%d")
