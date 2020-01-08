
# Code description --------------------------------------------------------

# This code is used to create a dataframe that we'll use to track the QA progress of the gage data.

# load data
load("data/all_gages.rda")
all_gages %>% st_drop_geometry() -> all_gages

gage_QA_progress <- all_gages %>% 
  select()
