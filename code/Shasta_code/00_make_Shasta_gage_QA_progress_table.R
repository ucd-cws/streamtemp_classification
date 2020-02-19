
# Code description --------------------------------------------------------

# This code creates the dataframe of Shasta sites that can be appended to the gage_QA_progress table


# Libraries ---------------------------------------------------------------

library(tidyverse)

# Import gage_progress_QA table -------------------------------------------

gage_QA_progress <- read_csv("data/data_review/gage_QA_progress.csv")
Shasta_sites <- read_csv("data/Shasta/Shasta_raw/Shasta_logger_coordinates.csv", col_names = c("site_id", "UTM_E", "UTM_N", "operator"), skip = 1)


# Format Shasta_sites dataframe to match gage_QA_progress and bind --------

Shasta_sites[c("site_name", "lon", "lat", "reviewer", "completed_Y_N", "notes")] <- NA

Shasta_sites_QA_progress <- Shasta_sites %>% 
  filter(!is.na(site_id)) %>% 
  select(site_id, site_name, lon, lat, operator, reviewer, completed_Y_N, notes)

gage_QA_progress <- rbind(gage_QA_progress, Shasta_sites_QA_progress)

write_csv(gage_QA_progress, path = "data/data_review/gage_QA_progress.csv")
