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


