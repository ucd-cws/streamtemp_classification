
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


# Write Functions ---------------------------------------------------------

# step 1: fit model to observed data
thermal_regime_model <- function(obs_data){
  fitModel(mean_temp_C~a + b*sin(2*pi/365*(DOWY+c)), data = obs_data)
}

# need coefficients from step 1 to go in step 2:
# Step 2: this pulls coefficients in model and models across julian day
thermal_regime_model_solved <- function(obs_data, model) {
  a <- coef(model)[1]
  b <- coef(model)[2]
  c <- coef(model)[3]
  # at this point we should write these out to a csv for future use
  Julian_Day <- obs_data$DOWY
  # now use above to fit below
  a + b*sin(2*pi/365*(Julian_Day+c))
}

# for testing
m1 <- thermal_regime_model(obs_data = all_sites_model_data %>% filter(station_id=="11261100"))

# now pass to second function
df1 <- all_sites_model_data %>% filter(station_id=="11261100")

df1$model_avg_daily_temp_C <- thermal_regime_model_solved(obs_data = df1, model = m1)

# now plot~!
ggplot(data = df1) +
  geom_point(aes(x = DOWY, y = mean_temp_C), color="darkblue") +
  geom_point(aes(x=DOWY, y = model_avg_daily_temp_C), alpha=0.5, size=1)+
  labs(x = "julian day", y = "daily mean stream temperature, deg C")


# Make combined function -----------------------------------------------------

# this leverages the functions above and runs all at once
solve_thermal_model <- function(obs_data){
  mod_out <- thermal_regime_model_solved(obs_data, model=thermal_regime_model(obs_data))
}

# this should return a vector of numbers 1:366 for the modeled data!
# df1 <- all_sites_model_data %>% filter(station_id=="11261100") %>% solve_thermal_model(.)


# Split Data for Model ----------------------------------------------------

model_df <- all_sites_model_data #%>% split(.$station_id) # split by site ID

# check there are 71 sites
all_sites_model_data %>% distinct(station_id) %>% nrow()

# Now Build Loop with Purrr -----------------------------------------------

set.seed(777)

# RUN ON EVERYTHING
model_out <- model_df %>%
  split(.$station_id) %>% # split by site ID
  map(., ~mutate(.data = .x, 
                 model_avg_daily_temp_C = solve_thermal_model(obs_data = .x)))



# Recombine Data And Plot -------------------------------------------------

# flatten back into a dataframe by binding rows
model_out <- model_out %>% bind_rows()


# ggplotly
ggplot(data = model_out) +
  geom_line(aes(x=DOWY, y = mean_temp_C, group=station_id), color="darkblue", alpha=0.5) +
  geom_point(aes(x=DOWY, y = model_avg_daily_temp_C, group=station_id), color="maroon", size=1)+
  labs(x = "julian day", y = "daily mean stream temperature, deg C")

### YAY!!! It works. Next steps: 
# Model all sites
# Save model coefficients for each site
# Review Maheu et al. (2015) and ID metrics to extract from each model for classification analysis
 # - annual amplitude (annual max - annual mean)
 # - day of annual maximum
 # - annual mean



# Save Out ----------------------------------------------------------------

write_csv(model_out, path = "output/models/thermal_regime_models_daily.csv")
save(model_out, file = "output/models/thermal_regime_models_daily.rda")
