## Clustering by thermal regimes

# Libraries ---------------------------------------------------------------

library(tidyverse)
library(tidylog)
library(cluster)    # clustering algorithms
library(factoextra) # clustering visualization/stats

# Load Data ---------------------------------------------------------------

load("output/models/annual_cluster_metrics_all_gages.rda")

# CLUSTERING: Scale & Create Dist Matrix ------------------------------------------------

# first double check for NAs, total rows=77
summary(ann_metrics)

# then scale data (mean of zero, sd =1)
ann_metrics_s <- ann_metrics %>% 
  select(station_id, ann_mean, ann_amp, DOWY) %>% # select only metrics to model
  dplyr::mutate_at(vars(ann_mean:DOWY), .funs = scale) %>%
  tibble::column_to_rownames(var = "station_id")

# create Euclidean dissimilarity/distance matrix
d1 <- dist(ann_metrics_s, method = "euclidean")

# 01. {factoextra} Check data uniformity ----------------------------------

# use Hopkins Stat to assess data uniformity:
## if data highly uniform, clustering not an ideal approach
## small values of $hopkins_stat indicate uniform data (normalized deviation from uniform data)
## higher values mean the data is not uniform and suitable for clustering...some use a threshold
## roughtly, if H > 0.5, then "suitable for clustering" but this seems a bit vague/contentious.

factoextra::get_clust_tendency(ann_metrics_s, 
                               # take 90% of data
                               n = round(.9*nrow(ann_metrics_s), 0), 
                               seed = 123)
# so value of our data is: 0.89 (so very non-uniform!, and thus good for clustering

# 02. {NbClust} Check for Ideal K -----------------------------------------

# now check for ideal K using 'NbClust()'
library(NbClust)

(clust_stats <- NbClust(ann_metrics_s, distance = "euclidean", method = "ward.D2"))

# so based on this, a consensus of k=3 is best, but can justify k=5 based on 2 methods. (Silhouette and PtBiserial?)

dev.off() # reset graphics

# 03. {fpc} Cluster Stability --------------------------------------------

library(fpc)

# test stability of k=5

# try with hclust k=5
cboot5 <- clusterboot(data = d1, 
                      clustermethod = hclustCBI,
                      method="ward.D2",
                      seed = 333,
                      k=5,
                      B = 100 # number of resamples/bootstraps
)
print.clboot(cboot5) # look at all outputs

# try with hclust k=3
cboot3 <- clusterboot(data = d1, 
                      clustermethod = hclustCBI,
                      method="ward.D2",
                      seed = 333,
                      k=3,
                      B = 100 # number of resamples/bootstraps
)
print.clboot(cboot3) # look at all outputs

# try with hclust k=6
cboot6 <- clusterboot(data = d1, 
                      clustermethod = hclustCBI,
                      method="ward.D2",
                      seed = 333,
                      k=6,
                      B = 100 # number of resamples/bootstraps
)
print.clboot(cboot6) # look at all outputs


# still would go with k=5
cboot5_grps <- cboot5$result$partition # vector of cluster labels
table(cboot5_grps) # table of how many per group
cboot5$bootmean # cluster stabilities (Jaccard Scores)
# so group 3 is highly stable, then group 1 and then group 2

# Save out groups based on bootstrapping
# make into a dataframe
cboot5_df <- as.data.frame(cboot5_grps) %>% 
  rename(k_5=cboot5_grps) %>% 
  mutate(station_id = rownames(.)) %>% 
  select(station_id, k_5)


# cluster stats
cluster.stats(d1, clustering = cboot5_grps)

# 04. Additional Stats to pick K  ----------------------------------------

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
#ggsave("output/figures/wss_and_ch_index_for_clusters.png", width = 10, height = 8, units="in", dpi = 300)

## Additional Stats
# nice overview here: https://bradleyboehmke.github.io/HOML/hierarchical.html#determining-optimal-clusters

# WSS::elbow methods: define clusters such that the total within-cluster variation is minimized, so choose a k that minimizes the total within-cluster sum of squares (a measure of the compactness of the clustering).
p1 <- fviz_nbclust(ann_metrics_s, FUN = hcut, method = "wss", 
                   k.max = 8) + 
  #geom_vline(xintercept = 5, linetype=3, col="gray") +
  ggtitle("WSS/Elbow method")

# Silhouette method (Rousseeuw 1987)
p2 <- fviz_nbclust(ann_metrics_s, FUN = hcut, method = "silhouette", 
                   k.max = 8) +
  ggtitle("Silhouette method")

# Display plots side by side
(p4 <-cowplot::plot_grid(p1, p2, nrow = 1, labels = c("B","C")))


# HCLUST: {hclust} ------------------------------------------------------------------

## Agglomerative Nesting Clustering

# Hierarchical clustering using Complete Linkage
hc1 <- hclust(d1, method = "ward.D2")

# PLOT Dendrograms

# k=5
plot(hc1, cex = 0.6, hang = -1)
# see how many clusters with k = 5 (totally arbitrary)
rect.hclust(hc1, k = 5, border = viridis::viridis(5))

# k=6
plot(hc1, cex = 0.6, hang = -1)
rect.hclust(hc1, k = 6, border = viridis::viridis(6))

# k=3
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
pltree(hc2, cex = 0.6, hang = -1, main = "Dendrogram {agnes}: Wards, k=5") 
rect.hclust(hc2, k = 5, border = viridis::viridis(5))

# try with k=3
pltree(hc2, cex = 0.6, hang = -1, main = "Dendrogram {agnes}: Wardsm k=6") 
rect.hclust(hc2, k = 6, border = viridis::viridis(3))

# try with k=3
pltree(hc2, cex = 0.6, hang = -1, main = "Dendrogram {agnes}: Wards, k=3") 
rect.hclust(hc2, k = 3, border = viridis::viridis(3))

# the groups out
hc2_grps_k6 <- cutree(hc2, k=6) # try k=5
table(hc2_grps_k6)
hc2_grps_k5 <- cutree(hc2, k=5) # try k=5
hc2_grps_k3 <- cutree(hc2, k=3) # try k=3

# bind together and save
agnes_k_groups <- tibble(site_id = ann_metrics$station_id, k_3=hc2_grps_k3, k_5=hc2_grps_k5, k_6=hc2_grps_k6)
save(agnes_k_groups, file="output/models/agnes_k_groups_final.rda")

# k=6
ggclust2_k6 <- fviz_cluster(list(data=d1, cluster=hc2_grps_k6))
ggclust2_k6 + theme_classic() +
  labs(title = "Clusters for CA Thermal Regimes (k=6)")
ggsave("output/figures/pc_agnes_k6.png", width = 8, height = 6, units="in", dpi=300)

# k=5
ggclust2_k5 <- fviz_cluster(list(data=d1, cluster=hc2_grps_k5))
ggclust2_k5 + theme_classic() +
  labs(title = "Clusters for CA Thermal Regimes (k=5)")
ggsave("output/figures/pc_agnes_k5.png", width = 8, height = 6, units="in", dpi=300)

# plot k=3
ggclust2_k3 <- fviz_cluster(list(data=d1, cluster=hc2_grps_k3))
ggclust2_k3 + theme_classic() +
  labs(title = "Clusters for CA Thermal Regimes (k=3)")
ggsave("output/figures/pc_agnes_k3.png", width = 8, height = 6, units="in", dpi=300)



# Save out Group Bootstrap k's --------------------------------------------

# save # output/models/agnes_k_groups_final.rda
write_csv(cboot5_df, path="output/models/classification_group_results.csv")

save(cboot5_df, file = "output/models/classification_group_results.rda")
