# Code description  ---------------------------------------------------------------

# This code is the cleaned up version of the cluster algorithms test, and includes the agnes, CH Index, and wss analyses. It also includes a calculation of distance from centroid for each class member in classes 2, 3, and 4 to analyze weak members that might shift classes.

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

# calc clusters and get means
tst1 <- factoextra::hkmeans(ann_metrics_s, 5)
hkmeans_tree(tst1, viridis::viridis(5))
fviz_cluster(tst1, show.clust.cent = T, ggtheme = theme_bw())

tst1$centers

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

# the groups out
hc2_grps_k5 <- cutree(hc2, k=5) # try k=5
table(hc2_grps_k5)

# plot
ggclust2_k5 <- fviz_cluster(list(data=d1, cluster=hc2_grps_k5))

ggclust2_k5 + theme_classic() +
  labs(title = "Clusters for CA Thermal Regimes (k=5)")



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


# Plot distance to centroid for each member in classes 2 and 4 --------

member_locations <- as.data.frame(ggclust2_k5$data)

member_locations_2_4 <- member_locations %>% 
  filter(cluster != "1") %>% 
  filter(cluster != "5") %>% 
  filter(cluster != "3")

#Extract centers of each cluster





#Add a column that calculates distance between class centroid and member point




