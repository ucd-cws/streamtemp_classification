
# Code description --------------------------------------------------------

# Figure 3: Annual thermal regime for each study site, grouped by class, and box plots showing the distribution of annual mean, amplitude, and phase for each class. Figure adapted from Maheu et al. (2016) figure 2.


# Libraries ---------------------------------------------------------------

library(tidyverse)

# Figure 2: Classified thermal regimes ----------------------

load("output/models/classification_group_results.rda")

classification_group_results <- data_k_sf_v2 %>% 
  select(station_id, k_5, color)

merge_models_and_classes <- left_join(all_sites_model_data, classification_group_results) %>% 
  filter(!is.na(k_5))

ggplot(data = merge_models_and_classes) + 
  geom_line(aes(x=DOWY, y=model_avg_daily_temp_C, group = station_id, color = color), show.legend = FALSE)+
  facet_grid(cols = vars(k_5)) +
  theme_classic() 
