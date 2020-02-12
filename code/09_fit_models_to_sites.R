
# Code description --------------------------------------------------------

# This code takes the compiled model data for each cdec and usgs site, then fits a sine function to model the thermal regime. This model is based on the thermal regime model defined in Caissie et al. () and other relevant literature.


# Libraries ---------------------------------------------------------------

library(tidyverse)
library(lubridate)
library(mosaic)


# Load data ---------------------------------------------------------------

all_sites_model_data <- read_rds("data/model_data/all_sites_model_data.rds")

# create a df for a single site to test the model code.

model_data_AFD <- all_sites_model_data %>% 
  filter(station_id == "AFD")

# Thermal regime model function -------------------------------------------

thermal_regime_model <- fitModel(mean_temp_C~a + b*sin(2*pi/365*(DOWY+c)), data = model_data_AFD)

thermal_regime_model_solved <- function(Julian_Day,a,b,c) {
  a + b*sin(2*pi/365*(Julian_Day+c))
}

coefficients_model <- coef(thermal_regime_model)
a <- coefficients_model[1]
b <- coefficients_model[2]
c <- coefficients_model[3]

thermal_regime_model_solved(model_data_AFD$DOWY,a=a, b=b, c=c)

ggplot(data = model_data_AFD) +
  geom_point(aes(x = DOWY, y = mean_temp_C)) +
  geom_point(aes(x=DOWY, y = thermal_regime_model_solved(model_data_AFD$DOWY,a=a, b=b, c=c)))+
  labs(x = "julian day", y = "daily mean stream temperature, deg C")


### YAY!!! It works. Next steps: 
# Model all sites
# Save model coefficients for each site
# Review Maheu et al. (2015) and ID metrics to extract from each model for classification analysis
