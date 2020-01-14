## update 

# review temperatures
# starting from bottom up when sorted alphabetically

library(tidyverse)
library(lubridate)
library(dataRetrieval)

# Get Data ----------------------------------------------------------------

file_list <- list.files("data/data_review/")

# read in last file:
file_list[[95]]

# read in and assign name
usgs_minute_11468900 <- read_rds(path = paste0("data/data_review/",file_list[[95]]))

# fix names using dataRetrieval package
names(usgs_minute_11468900)
names(dataRetrieval::renameNWISColumns(usgs_minute_11468900)) # quick test to see change
# now assign new names
usgs_minute_11468900 <- renameNWISColumns(usgs_minute_11468900)
names(usgs_minute_11468900)


# Check for Error Flags in Data -------------------------------------------

# usgs uses flags in data...check with table()
table(usgs_minute_11468900$Wtemp_Inst_cd)

# filter out all values that aren't "A"
usgs_minute_11468900_filt <- usgs_minute_11468900 %>% filter(Wtemp_Inst_cd=="A")

# Simple Plot -------------------------------------------------------------

# this takes a minute given the number of points
plot(usgs_minute_11468900_filt$dateTime, usgs_minute_11468900_filt$Wtemp_Inst, type="p")

# this takes a few seconds:
ggplot() + geom_point(data=usgs_minute_11468900_filt, 
                      aes(x=dateTime, y=Wtemp_Inst), color="mediumpurple", size=.5, alpha=.3)

# Plotly Plots -------------------------------------------------------------

# investigate more closely with plotly
library(plotly)

# note: this takes longer to plot with lots of values

# now make an interactive plot of first 10000 values
ggplotly(
  ggplot() + geom_point(data=usgs_minute_11468900[1:10000,], aes(x=dateTime, y=Wtemp_Inst))
  )

# or filter to specific year/years, 2 years at a time is best to avoid lag
ggplotly(
  ggplot() + geom_point(data=usgs_minute_11468900 %>% filter(waterYear>2017 & waterYear<2019), aes(x=dateTime, y=Wtemp_Inst))
)





