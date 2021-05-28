
# Code description --------------------------------------------------------

# This code takes the compiled model data for each cdec and usgs site, then fits a sine function to model the thermal regime. This model is based on the thermal regime model defined in Caissie et al. () and other relevant literature.

# Libraries ---------------------------------------------------------------

library(tidyverse)
library(lubridate)
library(mosaic)

# Load data ---------------------------------------------------------------

all_sites_model_data <- read_rds("data/model_data/all_sites_model_data.rds")

# how many sites?
all_sites_model_data %>% distinct(station_id) %>% nrow() # 77 sites

# Write Functions ---------------------------------------------------------

# step 1: fit model to observed data
thermal_regime_model <- function(obs_data){
  fitModel(DailyMean~a + b*sin(2*pi/365*(DOWY+c)), data = obs_data)
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


# Now Build Loop with Purrr -----------------------------------------------

set.seed(777)

# RUN ON EVERYTHING
model_out <- model_df %>%
  split(.$station_id) %>% # split by site ID
  map(., ~mutate(.data = .x, 
                 model_avg_daily_temp_C = solve_thermal_model(obs_data = .x))
      )

# SUPER FAST!!!

# Recombine Data And Plot -------------------------------------------------

# flatten back into a dataframe by binding rows
model_out <- model_out %>% bind_rows()


# ggplotly
ggplot(data = model_out) +
  geom_line(aes(x=DOWY, y = mean_temp_C, group=station_id), color="darkblue", alpha=0.5) +
  geom_point(aes(x=DOWY, y = model_avg_daily_temp_C, group=station_id), color="maroon", size=1)+
  labs(x = "julian day", y = "daily mean stream temperature, deg C") +
  theme_classic()

### YAY!!! It works.

# Save Out ----------------------------------------------------------------

write_csv(model_out, path = "output/models/thermal_regime_models_daily.csv")
save(model_out, file = "output/models/thermal_regime_models_daily.rda")

# Analyze model fit and residuals -----------------------------------------

# what kind of object is this?
class(m1)

# summary of the model object
summary(m1)
# full list of residuals by day!
residuals(summary(m1))
# coefficents
coef(summary(m1))

# to save all these summary stats we can write to object and then access:
out <- summary(m1)

names(out)

# residual standard error (or really standard deviation) is positive square root of the mean square error
# "sigma" = resid standard error, we can use the following:
summary(m1)$sigma

# Purrr over our data and get sigma --------------------------------------------

sigma_out <- model_df %>%
  split(.$station_id) %>% # split by site ID
  map(., ~mutate(.data = .x, 
                 sigma = summary(thermal_regime_model(.x))$sigma)
  )

# flatten back into a dataframe by binding rows
sigma_out <- sigma_out %>% bind_rows() 

# make a single dataframe with just site and sigma
sigma_df <- sigma_out %>% select(station_id, sigma) %>% 
  distinct(station_id, .keep_all = TRUE)

# make sure all sites represented (n=77)
length(unique(sigma_out$station_id)) == nrow(sigma_df)


# Plot! -------------------------------------------------------------------

library(ggthemes)

# bring in classification data:
load("output/12_data_k_centdist_damdist.rda")
data_k_dist <- data_k_dist %>% sf::st_drop_geometry()

# join with sigma
sigma_df <- sigma_df %>% left_join(., data_k_dist, by="station_id")

table(sigma_df$k_5)

sigma_0_1 <- sigma_df %>% 
  filter(sigma < 1.0)

# stacked histogram
ggplot(data=sigma_df) + 
  geom_histogram(aes(x=sigma), bins = 40) + #can plot by regime using aes(x=sigma, fill=color)
  labs(x= "sigma") +
  theme_clean()

# faceted histogram
# ggplot(data=sigma_df) + 
#   geom_histogram(aes(x=sigma, fill=color), bins=40) +
#   labs(x= "sigma") +
#   facet_grid(k5_names~.)+
#   theme_clean()

ggsave("output/figures/09a_fig_residual_standard_error_vert.png", width = 8, height = 6, units = "in", dpi = 600)

write_csv(sigma_df, path = "output/models/thermal_regime_models_daily_w_sigma.csv")
save(model_out, file = "output/models/thermal_regime_models_daily_w_sigma.rda")
