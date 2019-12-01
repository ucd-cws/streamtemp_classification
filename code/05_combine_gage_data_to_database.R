## SUMMARY: Combine USGS & CDEC data and save to database 
## 2019-Dec
## DETAILS: 
# Data downloaded from USGS and CDEC gages will be combined to daily and saved to a SQLITE/GEOPACKAGE database for analysis.


# LIBRARIES ---------------------------------------------------------------

library(sf)
library(tidyverse)
library(mapview)
library(purrr)
library(lubridate)


