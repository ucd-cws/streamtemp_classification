
# Code description --------------------------------------------------------

# This code takes the compiled model data for each cdec and usgs site, then fits a sine function to model the thermal regime. This model is based on the thermal regime model defined in Caissie et al. () and other relevant literature.


# Libraries ---------------------------------------------------------------

library(tidyverse)
library(lubridate)
library(mosaic)


# Thermal regime model function -------------------------------------------

thermal_regime_model <- function(Julian_Day,a,b,c) {
  a + b*sin(2*pi/365*(Julian_Day+c))
}
