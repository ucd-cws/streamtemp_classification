## Calculate Metrics from Modeled Data for Classification
# following Maheu et al. (2015) 
# - annual mean
# - annual amplitude (annual max - annual mean)
# - day of annual maximum

# Libraries ---------------------------------------------------------------

library(tidyverse)
library(tidylog)
#library(slider) # for sliding window calculations: https://davisvaughan.github.io/slider/

# Get Data ----------------------------------------------------------------

load("output/models/thermal_regime_models_daily.rda")

# do some basic cleaning to drop attributes
model_out <- model_out %>% as.data.frame()


# Calculate Annual Mean & Amplitude By Gage ------------------------------

# annual amplitude is the annual max - annual mean
ann_metrics <- model_out %>% 
  group_by(station_id) %>% 
  summarize(ann_mean = mean(model_avg_daily_temp_C),
            ann_max = max(model_avg_daily_temp_C),
            ann_amp = ann_max - ann_mean)

# visualize:
ggplot() + 
  geom_point(data=ann_metrics, aes(x=station_id, y=ann_amp), pch=21, fill="maroon", size=4) +
  geom_point(data=ann_metrics, aes(x=station_id, y=ann_mean), color="darkblue") +
  coord_flip()

# Calculate Day of Ann Max ------------------------------------------------

ann_max_day <- model_out %>% 
  group_by(station_id) %>% 
  slice(which.max(model_avg_daily_temp_C)) %>% 
  ungroup()

library(plotly)
ggplotly(ggplot() +
  geom_point(data=ann_max_day, aes(x=station_id, y=DOWY, 
                                   color=model_avg_daily_temp_C)) +
    coord_flip()+
    scale_color_viridis_c())


# Join Together -----------------------------------------------------------

ann_metrics <- left_join(ann_metrics, ann_max_day)
#save(ann_metrics, file = "output/models/annual_cluster_metrics_all_gages.rda")

# done! 

# CLUSTERING: Scale & Create Dist Matrix ------------------------------------------------

library(cluster)    # clustering algorithms
library(factoextra) # clustering visualization
library(dendextend) # for comparing two dendrograms

# first double check for NAs
summary(ann_metrics)

# then scale data (mean of zero, sd =1)
ann_metrics_s <- ann_metrics %>% 
  select(station_id, ann_mean, ann_amp, DOWY) %>% # select only metrics to model
  dplyr::mutate_at(vars(ann_mean:DOWY), .funs = scale) %>%
  tibble::column_to_rownames(var = "station_id")

# create Euclidean dissimilarity/distance matrix
d1 <- dist(ann_metrics_s, method = "euclidean")


# HCLUST: {hclust} ------------------------------------------------------------------

# Hierarchical clustering using Complete Linkage
hc1 <- hclust(d1, method = "ward.D2")

# PLOT Dendro
plot(hc1, cex = 0.6, hang = -1)

# see how many clusters with k = 5 (totally arbitrary)
rect.hclust(hc1, k = 5, border = viridis::viridis(5))


# HCLUST: {agnes} -------------------------------------------------------

# use agnes to get agglomeative coefficient
# closer to 1 is stronger clusterings
hc2 <- agnes(d1, method = "complete")

hc2$ac

# methods to assess
m <- c( "average", "single", "complete", "ward")
names(m) <- c( "average", "single", "complete", "ward")

# function to compute coefficient
ac <- function(x) {
  agnes(d1, method = x)$ac
}

# so wards is best option here...
map_dbl(m, ac)

# rerun with just ward
hc3 <- agnes(d1, method = "ward")

# plot and add clusters
pltree(hc3, cex = 0.6, hang = -1, main = "Dendrogram of agnes: Wards") 
rect.hclust(hc1, k = 5, border = viridis::viridis(5))



# Clustering Stats --------------------------------------------------------

library(fpc)


