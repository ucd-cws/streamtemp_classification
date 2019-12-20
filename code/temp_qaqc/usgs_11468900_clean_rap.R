# review temperatures
# starting from bottom up when sorted alphabetically

library(tidyverse)
library(lubridate)


# Get Data ----------------------------------------------------------------

file_list <- list.files("data/data_review/")

# read in last file:
file_list[[95]]

usgs_minute_11468900 <- read_rds(path = paste0("data/data_review/",file_list[[95]]))


# Plot --------------------------------------------------------------------


