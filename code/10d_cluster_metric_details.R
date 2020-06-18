# Code description  ---------------------------------------------------------------

# This code looks at the results of the cluster analysis and extracts details for each class that are used to describe each thermal regime in the manuscript.

# Libraries ---------------------------------------------------------------

library(tidyverse)
library(tidylog)
library(wateRshedTools) #used to convert julian dates
library(lubridate)


# Figure 2a: the x-y plot of clustered thermal regimes --------------------

load("output/models/09b_annual_cluster_metrics_all_gages.rda")
load("output/models/10a_agnes_k_groups_final.rda")

#make a date column
ann_metrics$date
