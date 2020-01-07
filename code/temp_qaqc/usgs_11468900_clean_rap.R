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

library(plotly)

# now make an interactive plot of first 1000 values
ggplotly(
  ggplot() + geom_point(data=usgs_minute_11468900[1:1000,], aes(x=dateTime, y=X_00010_00000)))
