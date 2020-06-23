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

#add thermal regime class numbers
thermal_regime_parameters <- left_join()

# # join site data with groups
# data_k <- left_join(agnes_k_groups, sites, by=c("site_id"="station_id")) %>% 
#   rename(station_id=site_id) %>% 
#   # drop cols we don't need
#   select(station_id, k_5, site_name:operator)

#make dataframe of only thermal regime parameter values
thermal_regime_parameters <- ann_metrics %>% 
  select(station_id, ann_mean, ann_max, DOWY, date)

