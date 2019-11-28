
# Study title and team ----------------------------------------------------

# Title: Classifying California streams for cold-water conservation

# Authors: Ann Willis, Ryan Peek, Steven Sadro, Andrew Rypel, Alexander Forrest

# Project description -----------------------------------------------------

# This code was developed to analyze the thermal regimes of CA streamsm classify them with respect to their overall temperature (e.g., warm, cool, cold) and stability (i.e. cold-stable or cold-variable). The results of this analysis is to identify systems that should be targeted for long-term conservation. The code is outlined so that each section reflects major chunks of analysis.


# Libraries ---------------------------------------------------------------

library(dataRetrieval) #retrieving data from USGS
library(tidyverse) #This library includes packages like ggplot2, dplyr, and tidyr
library(lubridate) #Tools that make working with dates and times easier
library(mosaic) #Fit the sine model to the temperature data
library(trend) #Performs the Mann-Kendall trend test
library(sf) #This helps makes the map
library(mapview) #This helps make the map, too

# Data assembly -----------------------------------------------------------

# Data sources for this study will be observed water temperature data from USGS and CDEC databases. In this chunk, we'll automatically query data from those databases, specifically targeting stations that have temperature data. Data will be reviewed to identify bad data points, data gaps, and length of record. Sites with sufficient data (criteria to be determined, but probably based on length of data record) will then be transformed into daily mean temperature and plotted. Jones and Schmidt (2018) is likely source for establishing minimum record length.

#Make a map to visualize the distribution of sites
#01_filter_out_gages



# Temperature modeling ----------------------------------------------------

# Annual temperature patterns will be modeled using the sine function applied in Cluis (1972) and Caissie et al. (2001); a similar model was determined in Ward (1963). This model can be written using the mosaic package; I already have some code developed for internal data. Things to sort out:

# 1. Whether to average the data so that I'm left with a single year of average water temperature for each day. I'll need to identify how many years are represented for each site. For example, a site with 7 years of data would be described as n = 7. I'd likely have to tabulate this for the manuscript, along with model performance (i.e., fit) metrics. Relevant papers include Maheu et al. (2016), Daigle et al. (2019), and possibly Ptak et al. (2019)

# 2. Metrics to quantify for each stream. See Maheu et al. (2016) and Jones and Schmidt (2018)

# Calculate mean annual temperature, max annual temperature, and day of max annual temperature


# Thermal regime classification -------------------------------------------

# Once temperature models are developed for each stream and metrics for each model quantified, streams are then classified. See Maheu et al. (2016) for classification methods

# Test code with dummy data to replicate MK results from Meals et al. (2011), example 2

MK_test <- read.csv("data/MK_test.csv")
TP <- MK_test$TP_mg.L
mk.test(TP, continuity = TRUE)

# Select streams with a stable mean annual temperature using the Mann-Kendall trend test. The Mann-Kendall trend test is a non-parametric statistical test to select thermal regimes with stable annual mean temperatures (i.e., sites minimally impacted by anthropogenic operations like flow regulation or diversions)



# 

