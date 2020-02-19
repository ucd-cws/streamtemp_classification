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
(gg1 <- ggplot() + 
  geom_point(data=ann_metrics, aes(x=reorder(station_id, ann_amp), y=ann_amp), pch=21, fill="purple2", size=2.5, alpha=0.8) +
    coord_flip() +
    theme_classic(base_family = "Roboto Condensed") + # may need to change this to "Helvetica" or "Arial"
    theme(legend.position = c(.1, 0.8),
          axis.text.y = element_text(size=7),
          panel.grid.major.y = element_line(color="gray80", size=0.1))+
    labs(x="Station ID", 
         y="Annual Amplitude (ann. max - ann. mean) (C)"))
  
(gg2 <- ggplot() +
    geom_point(data=ann_metrics, aes(x=reorder(station_id, ann_mean), y=ann_mean), color="darkblue",size=2.5, alpha=0.8) +
      coord_flip()+
      theme_classic(base_family = "Roboto Condensed") +
      theme(legend.position = c(.1, 0.8),
            axis.text.y = element_text(size=7),
            panel.grid.major.y = element_line(color="gray80", size=0.1))+
      labs(x="", 
           y="Annual Mean (C)"))

library(cowplot)
(pg1 <- plot_grid(gg1, gg2, labels = c("A","B"), label_fontfamily = "Roboto Condensed"))
#save_plot(pg1, filename = "output/figures/ann_mean_ann_amp_cowplot.png",base_height = 6, dpi=300)

# Calculate Day of Ann Max ------------------------------------------------

ann_max_day <- model_out %>% 
  group_by(station_id) %>% 
  slice(which.max(model_avg_daily_temp_C)) %>% 
  ungroup()

#library(plotly)
#ggplotly(
  ggplot() +
  geom_point(data=ann_max_day, aes(x=reorder(station_id, DOWY), y=DOWY, 
                                   color=model_avg_daily_temp_C)) +
    coord_flip()+
    scale_color_viridis_c("Modeled Avg \n Daily Temp (C)") +
    theme_classic() +
    theme(legend.position = c(.1, 0.8),
          panel.grid.major.y = element_line(color="gray80", size=0.1))+
    labs(x="Station ID", 
         y="Day of Annual Max (Day of Water Year)")
#)
#ggsave("output/figures/day_of_ann_max_dowy.png", dpi=300, width = 9, height = 7, units = "in")

# Join Together -----------------------------------------------------------

ann_metrics <- left_join(ann_metrics, ann_max_day)
#save(ann_metrics, file = "output/models/annual_cluster_metrics_all_gages.rda")

# done! 

# CLUSTERING: Scale & Create Dist Matrix ------------------------------------------------

library(cluster)    # clustering algorithms
library(factoextra) # clustering visualization/stats

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

## Agglomerative Nesting Clustering

# Hierarchical clustering using Complete Linkage
hc1 <- hclust(d1, method = "ward.D2")

# PLOT Dendro
plot(hc1, cex = 0.6, hang = -1)

# see how many clusters with k = 5 (totally arbitrary)
rect.hclust(hc1, k = 5, border = viridis::viridis(5))

# try with k=3
plot(hc1, cex = 0.6, hang = -1, main="Dendrogram {hclust}: Wards")
rect.hclust(hc1, k = 3, border = viridis::viridis(3))

# HCLUST: {agnes} -------------------------------------------------------

# Agglomerative Nesting Clustering

# use agnes to get agglomerative coefficient
# closer to 1 is stronger clusterings
hc2 <- agnes(d1, method = "ward")
hc2$ac

# try diff methods to assess
m <- c( "average", "single", "complete", "ward")
names(m) <- c( "average", "single", "complete", "ward")

# function to compute coefficient
ac <- function(x) {
  agnes(d1, method = x)$ac
}

# so wards is still best option here...
map_dbl(m, ac)

# plot and add clusters
pltree(hc2, cex = 0.6, hang = -1, main = "Dendrogram {agnes}: Wards") 
rect.hclust(hc2, k = 5, border = viridis::viridis(5))

# try with k=3
pltree(hc2, cex = 0.6, hang = -1, main = "Dendrogram {agnes}: Wards") 
rect.hclust(hc2, k = 3, border = viridis::viridis(3))

# the groups out
hc2_grps <- cutree(hc2, k=5) # try k=5
hc2_grps <- cutree(hc2, k=3) # try k=3
table(hc2_grps)

ggclust2 <- fviz_cluster(list(data=d1, cluster=hc2_grps))
ggclust2 + theme_classic() +
  labs(title = "Clusters for CA Thermal Regimes (k=3)")
ggsave("output/figures/pc_agnes_k3.png", width = 8, height = 6, units="in", dpi=300)

# HCLUST: {NbClust} --------------------------------------------------------

# use Hopkins Stat to assess data uniformity
# if data highly uniform, clustering not an ideal approach
# small values of $hopkins_stat indicate uniform data (normalized deviation from uniform data)
# higher values mean the data is not uniform and suitable for clustering...some use a threshold
# of H > 0.5 means "suitable for clustering" but this seems a bit vague/contentious.
factoextra::get_clust_tendency(ann_metrics_s, 
                               n = round(.9*nrow(ann_metrics_s), 0), # take 90% of data
                               seed = 123)
# so value of our data is: 0.906 (so very non uniform!)

# now check for ideal K using 'NbClust()'
library(NbClust)
NbClust(ann_metrics_s, distance = "euclidean", method = "ward.D2")
# so based on this, a k=3 is best

# HCLUST: {diana} ---------------------------------------------------------

## Trying with the DIANA model (Divisive Analysis Clustering)
hc3 <- diana(d1, metric = "euclidean", stand = FALSE)
# plot
pltree(hc3, cex=0.6, hang=-1, "Dendrogram {diana}: Euclidean")
rect.hclust(hc3, k=5, border = viridis::viridis(5))

library(ggdendro)
ggdendrogram(hc3, rotate=TRUE) + theme_classic(base_family = "Roboto Condensed")

# look at cluster divisive coefficient: 
hc3$dc # very high, means good clusters

# cut the clusters
hc3_grps <- cutree(hc3, k=5)
table(hc3_grps) # gives the groups

# visualize:
ggclust3 <- fviz_cluster(list(data=d1, cluster=hc3_grps))
ggclust3 + theme_classic() +
  labs(title = "Clusters for CA Thermal Regimes (k=5)")
ggsave("output/figures/pc_diana_k3.png", width = 8, height = 6, units="in", dpi=300)


# HCLUST: Stats  -----------------------------------------------------------

# calculating the Calinski-Harabasz Index

# custom functions adopted from here:https://github.com/ethen8181/machine-learning/blob/master/clustering_old/clustering/clustering_functions.R, and following Zumel and Mount (2014)

# calculate distances
Distance <- function(cluster)
{
  # the center of the cluster, mean of all the points
  center <- colMeans(cluster)
  
  # calculate the summed squared error between every point and 
  # the center of that cluster 
  distance <- apply( cluster, 1, function(row)
  {
    sum( ( row - center )^2 )
  }) %>% sum()
  
  return(distance)
}

# function to calc total within SS (measure of each point to centroid of cluster)
WSS <- function( data, groups ) 
{
  k <- max(groups)
  
  # loop through each groups (clusters) and obtain its 
  # within sum squared error 
  total <- lapply( 1:k, function(k)
  {
    # extract the data point within the cluster
    cluster <- subset( data, groups == k )
    
    distance <- Distance(cluster)
    return(distance)
  }) %>% unlist()
  
  return( sum(total) )
}

# using above, add ability to calculate the CH Index
CHCriterion <- function( data, kmax, clustermethod, ...  )
{
  if( !clustermethod %in% c( "kmeanspp", "hclust" ) )
    stop( "method must be one of 'kmeanspp' or 'hclust'" )
  
  # total sum squared error (independent with the number of cluster k)
  tss <- Distance( cluster = data )
  
  # initialize a numeric vector storing the score
  wss <- numeric(kmax)
  
  # k starts from 2, cluster 1 is meaningless
  if( clustermethod == "kmeanspp" )
  {
    for( k in 2:kmax )
    {
      results <- Kmeanspp( data, k, ... )
      wss[k]  <- results$tot.withinss 
    }		
  }else # "hclust"
  {
    d <- dist( data, method = "euclidean" )
    clustering <- hclust( d, ... )
    for( k in 2:kmax )
    {
      groups <- cutree( clustering, k )
      wss[k] <- WSS( data = data, groups =  groups )
    }
  }		
  
  # between sum of square
  bss <- tss - wss[-1]
  
  # cluster count start from 2! 
  numerator <- bss / ( 1:(kmax-1) )
  denominator <- wss[-1] / ( nrow(data) - 2:kmax )
  
  criteria <- data.frame( k = 2:kmax,
                          CHIndex = numerator / denominator,
                          wss = wss[-1] )
  
  # convert to long format for plotting 
  criteria_long <- gather( criteria, "index", "value", -1 )
  
  plot <- ggplot( criteria_long, aes( k, value, color = index ) ) + 
    geom_line() + geom_point( aes( shape = index ), size = 3 ) +
    facet_wrap( ~ index, scale = "free_y" ) + 
    guides( color = FALSE, shape = FALSE )
  
  return( list( data = criteria, 
                plot = plot ) )
}


# APPLY to our Data using the ward method
kcriteria <- CHCriterion(data = ann_metrics_s, kmax=8,
                         clustermethod = "hclust", method="ward.D2")

kcriteria$data
kcriteria$plot
# based on this, could probably justify either 3 or 5...

# HCLUST: {fpc} -----------------------------------------------------------

library(fpc)

# set the desired number of clusters
k_try <- 5

# try 
cboot5 <- clusterboot(data = d1, 
                        clustermethod = hclustCBI,
                        method="ward.D2", 
                        k=k_try,
                        B = 1000 # number of resamples/bootstraps
                        )

cboot5_grps <- cboot5$result$partition # vector of cluster labels
print.clboot(cboot5) # look at all outputs
table(cboot5_grps) # table of how many per group
cboot5$bootmean # cluster stabilities (Jaccard Scores)
# so group 3 is highly stable, then group 4 and then group 1. Group 5 is less so


## Now try with 3
k_try <- 3

# try 
cboot3 <- clusterboot(data = d1, 
                      clustermethod = hclustCBI,
                      method="ward.D2", 
                      k=k_try,
                      B = 1000 # number of resamples/bootstraps
)

cboot3_grps <- cboot3$result$partition # vector of cluster labels
print.clboot(cboot3) # look at Jaccard Scores here
table(cboot3_grps)
cboot3$bootmean # highly stable


# cluster stats
cluster.stats(d1, clustering = cboot5_grps)
