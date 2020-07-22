
# Code description --------------------------------------------------------

# This code is used to make Figure 5: a histogram of the distances below dams for the stable cool and variable warm thermal regimes.


# Libraries ---------------------------------------------------------------

# Not sure that we need all these libraries, I just grabbed the ones from code 12

library(sf)
library(tidyverse)
library(tidylog)
library(ggthemes)
library(tidyverse)
# Load data ---------------------------------------------------------------

load("output/12_data_k_centdist_damdist.rda")

# filter out to just reg warm/reg cool; this is code copied from 12_get_dam_distances
regimes_2_3 <- data_k_dist %>% 
  st_drop_geometry() %>% 
  filter(k5_names %in% c("2-variable warm", "3-stable cool")) %>%
  group_by(k_5, k5_names) 

ggplot(regimes_2_3) + 
  geom_histogram(aes(x=cum_len_km, fill=color), 
                            binwidth = 3, bins = 50) +
  labs(x= "distance below dam (km)") +
  scale_fill_manual(name="thermal regime",values = c("#FF7F00","#984EA3"),labels=c("variable warm", "stable cool")) +
  theme_clean()


