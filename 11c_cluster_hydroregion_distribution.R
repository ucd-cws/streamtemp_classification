
# Code description --------------------------------------------------------

# This code is used to determine which hydrologic region each thermal regime occurs in. 


# Libraries ---------------------------------------------------------------

library(tidyverse)


# Load data ---------------------------------------------------------------

# the k groups data 
load("output/11a_agnes_k_5_final_w_centdist.rda") # data_k_sf


# Summarize data ----------------------------------------------------------

thermal_regime_data <- data_k_sf_w_hydro_regions

hyd_region_dist <- thermal_regime_data %>% 
  count(HR_NAME, k5_names)

