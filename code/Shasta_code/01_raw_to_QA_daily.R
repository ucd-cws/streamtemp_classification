
# Code description --------------------------------------------------------

# This code imports data from the Shasta River/Big Springs Creek research projects and creates daily data sets that will be incorporated into the statewide analysis. May create separate codes for each site, depending on how much QA is required for each one.


# Libraries ---------------------------------------------------------------

library(tidyverse)
library(lubridate)
library(plotly)


# Import data -------------------------------------------------------------

BSC_raw_2008 <- read_csv("data/Shasta_raw/BSC_longitudinal_2008.csv", col_names = c("datetime", "julian_day", "BSC_dam", "Busk bridge", "Pond_upstream", "Pond_downstream", "Blw_ww", "corral bridge", "LDB", "abv LSC", "lowest_xing", "BSC_mouth"), skip = 1)

BSC_raw_2008 <- BSC_raw_2008 %>% 
  mutate() #reformat dates using lubridate. Look at other codes for formatting language 



# Build raw datasets for BSC sites ----------------------------------------

