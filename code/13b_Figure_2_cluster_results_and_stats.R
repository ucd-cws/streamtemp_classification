# Code description  ---------------------------------------------------------------

# Figure 2: A plot of the clustered thermal regimes and associated statistical metrics. 

# I'm not sure how to recreate the figures without re-running the clustering code, so I've copied the lines of code from 10 that pertain to agnes and CH Index/wss work.

# Libraries ---------------------------------------------------------------

library(tidyverse)
library(tidylog)
library(cluster)    # clustering algorithms
library(factoextra) # clustering visualization/stats
library(cowplot)
library(grid)
library(ggplotify)


# Figure 2a: the x-y plot of clustered thermal regimes --------------------

load("output/models/annual_cluster_metrics_all_gages.rda")
load("output/models/agnes_k_groups_final.rda")

# DROP CANAL SITE ---------------------------------------------------------

# note: this site: BW-12 IMPORT TO BUTTE CREEK is a canal and should be dropped
agnes_k_groups <- agnes_k_groups %>% filter(site_id != "BBW")

ann_metrics <- ann_metrics %>% filter(station_id != "BBW")

# CLUSTERING: Scale & Create Dist Matrix ------------------------------------------------

# first double check for NAs
summary(ann_metrics)

# then scale data (mean of zero, sd =1)
ann_metrics_s <- ann_metrics %>% 
  select(station_id, ann_mean, ann_amp, DOWY) %>% # select only metrics to model
  dplyr::mutate_at(vars(ann_mean:DOWY), .funs = scale) %>%
  tibble::column_to_rownames(var = "station_id")

# create Euclidean dissimilarity/distance matrix
d1 <- dist(ann_metrics_s, method = "euclidean")

# HCLUST: {agnes} -------------------------------------------------------

# Agglomerative Nesting Clustering

# use agnes to get agglomerative coefficient
# closer to 1 is stronger clusterings
# try diff methods to assess
m <- c( "average", "single", "complete", "ward")
names(m) <- c( "average", "single", "complete", "ward")

# function to compute coefficient
ac <- function(x) {
  agnes(d1, method = x)$ac
}

# so wards is best option here...
# Ward’s minimum variance criterion minimizes the total within-cluster variance
map_dbl(m, ac)

# use Wards with agnes:
hc <- agnes(d1, method = "ward")
hc$ac # the agglomerative coefficient

# plot and add clusters
pltree(hc, cex = 0.6, hang = -1, main = "Dendrogram {agnes}: Wards") 
rect.hclust(hc, k = 5, border = viridis::viridis(5))

# save the groups out
hc_grps_k5 <- cutree(hc, k=5) # try k=5
table(hc_grps_k5)

# Make a Cluster PCA Plot
ggclust2_k5 <- fviz_cluster(list(data=d1, cluster=hc_grps_k5))

ggclust2_k5 + theme_classic() +
  labs(title = "Clusters for CA Thermal Regimes (k=5)")

#ggsave("output/figures/pc_agnes_k5.png", width = 8, height = 6, units="in", dpi=300)

# Identify best K ---------------------------------------------------------

# nice overview here: https://bradleyboehmke.github.io/HOML/hierarchical.html#determining-optimal-clusters

# elbow methods:
# elbow method: define clusters such that the total within-cluster variation is minimized, so choose a k that minimizes the total within-cluster sum of squares (a measure of the compactness of the clustering).

# use existing scaled data to determine best k
p1 <- fviz_nbclust(ann_metrics_s, FUN = hcut, method = "wss", 
                   k.max = 8) +
  ggtitle("Elbow method")

# Silhouette method (Rousseeuw 1987)
p2 <- fviz_nbclust(ann_metrics_s, FUN = hcut, method = "silhouette", 
                   k.max = 8) +
  ggtitle("Silhouette method")

# Gap statistic (Tibshirani, Walther, and Hastie 2001)
p3 <- fviz_nbclust(ann_metrics_s, FUN = hcut, method = "gap_stat", 
                   k.max = 8) +
  ggtitle("Gap statistic")

# Display plots side by side
(p4 <-cowplot::plot_grid(p1, p2, p3, nrow = 1, labels = c("B","C","D")))



# HCLUST: Stats  -----------------------------------------------------------

# calculating the Calinski-Harabasz Index: best K is one that corresponds to the greatest value of the index

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


# Make final plots --------------------------------------------------------


plot_pc_k5 <- as.ggplot(ggclust2_k5 + theme_classic() + #also tried as.grob(); no luck
  labs(title = "Clusters for CA Thermal Regimes (k=5)"))

k_stats <- as.data.frame(kcriteria$data)

(plot_CHIndex <- ggplot() + geom_line(data = k_stats, aes(x = k, y = CHIndex, color = "coral1"), size = 1) +
  geom_point(data = k_stats, aes(x = k, y = CHIndex, color = "coral1"), size = 2) +
  scale_x_continuous(breaks = seq(min(k_stats$k), max(k_stats$k), by = 1)) +
  theme_classic() +
  theme(legend.position = "none"))

(plot_wss <- ggplot() + geom_line(data = k_stats, aes(x = k, y = wss), color = "deepskyblue1", size = 1) +
  geom_point(data = k_stats, aes(x = k, y = wss), color = "deepskyblue1", size = 2) +
  scale_x_continuous(breaks = seq(min(k_stats$k), max(k_stats$k), by = 1)) +
  theme_classic() +
  theme(legend.position = "none"))

fig_row_2 <- plot_grid(plot_CHIndex, plot_wss, labels = c("B", "C"), nrow = 1)

#ggsave("output/figures/Fig_2_cluster_results_and_stats.jpeg", width = 5, height = 4, units="in", dpi = 300)

plot_grid(plot_pc_k5, fig_row_2, labels = c("A"), nrow = 2, rel_heights = c(1.5,1))

plot_grid(plot_pc_k5, p4, labels = c("A"), nrow = 2, rel_heights = c(1.5,1))
ggsave("output/figures/Fig_2_cluster_results_and_stats_v2.jpeg", width = 10, height = 8, units="in", dpi = 300)

#Couldn't figure out how to recreate the pc_agnes_k5 plot in a way that would allow me to combine it with the CHIndex and wss plots using cowplot. For now, I'm just using the standalone plot (saved on line 79) for the paper.