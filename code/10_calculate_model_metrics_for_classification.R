## Calculate Metrics from Modeled Data for Classification
# following Maheu et al. (2015) 
# - annual amplitude (annual max - annual mean)
# - day of annual maximum
# - annual mean


# Libraries ---------------------------------------------------------------

library(tidyverse)
library(slider) # for sliding window calculations: https://davisvaughan.github.io/slider/


# Get Data ----------------------------------------------------------------

load("output/models/thermal_regime_models_daily.rda")

# do some basic cleaning to drop attributes
model_out <- model_out %>% as.data.frame()


# Calculate Annual Amplitude By Gage --------------------------------------

# annual amplitude is the annual max - annual mean
ann_amp <- model_out %>% 
  group_by(station_id) %>% 
  summarize(ann_mean = mean(model_avg_daily_temp_C),
            ann_amp = max(model_avg_daily_temp_C) - ann_mean)

# visualize:
ggplot() + 
  geom_point(data=ann_amp, aes(x=station_id, y=ann_amp), pch=21, fill="maroon", size=4) +
  geom_point(data=ann_amp, aes(x=station_id, y=ann_mean), color="darkblue") +
  coord_flip()



# Calculate Day of Ann Max ------------------------------------------------

ann_max_day <- model_out %>% 
  group_by(station_id) %>% 
  slice(which.max(model_avg_daily_temp_C)) %>% 
  ungroup()


