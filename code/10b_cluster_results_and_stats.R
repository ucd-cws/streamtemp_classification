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

load("output/models/09b_annual_cluster_metrics_all_gages.rda")
load("output/models/10a_agnes_k_groups_final.rda")

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
hc2 <- agnes(d1, method = "ward")
hc2$ac

# plot and add clusters
pltree(hc2, cex = 0.6, hang = -1, main = "Dendrogram {agnes}: Wards") 
rect.hclust(hc2, k = 5, border = viridis::viridis(5))

# the groups out
hc2_grps_k5 <- cutree(hc2, k=5) # try k=5
table(hc2_grps_k5)

# plot without labels
ggclust2_k5 <- fviz_cluster(list(data=d1, cluster=hc2_grps_k5), geom="point")

# plot with text labels
ggclust2_k5 <- fviz_cluster(list(data=d1, cluster=hc2_grps_k5), geom=c("point", "text"))


fviz_pca_var(prcomp(ann_metrics_s))


# gg pca plot
ggclust2_k5 + theme_classic() +
  labs(title = "Clusters for CA Thermal Regimes (k=5)") +
  guides(fill = guide_legend(
    override.aes = aes(label = "")))

# thermColor scale
thermCols <- data.frame(k5_group_id = c(1,3,4,2,5),
                        k5_names  = c("stable warm", "variable warm",
                                      "stable cool", "variable cool",
                                      "stable cold"),
                        color = I(c("#E41A1C", #stable warm
                                    "#FF7F00", #variable warm
                                    "#984EA3", #stable cool
                                    "#4DAF4A", #variable cool
                                    "#377EB8" #stable cold
                        )))

# check
ggclust2_k5 + theme_classic() +
  scale_fill_manual("Thermal \nClasses", values=thermCols$color, 
                    labels=thermCols$k5_names)+
  scale_color_manual("Thermal \nClasses", values=thermCols$color,
                     labels=thermCols$k5_names)+
  scale_shape_manual("Thermal \nClasses", values=c(15,16,17,18,8),
                     labels=thermCols$k5_names)+
  labs(title = "Clusters for CA Thermal Regimes (k=5)") +
  guides(fill = guide_legend(
    override.aes = aes(label = "")))

ggsave(filename = "output/figures/pc_agnes_k5_no_labels.png", width = 11, height = 8, units = "in", dpi=300, )

# HCLUST: Stats  -----------------------------------------------------------

# now check for ideal K using 'NbClust()'
library(NbClust)
NbClust(ann_metrics_s, distance = "euclidean", method = "ward.D2")
# so based on this, a k=3 is best, but 5 methods provided k=5

library(fpc)

# set the desired number of clusters
k_try <- 5

# try 
cboot5 <- clusterboot(data = d1, 
                      clustermethod = hclustCBI,
                      method="ward.D2", 
                      k=k_try,
                      seed = 1234,
                      B = 1000 # number of resamples/bootstraps
)

cboot5_grps <- cboot5$result$partition # vector of cluster labels
print.clboot(cboot5) # look at all outputs
table(cboot5_grps) # table of how many per group
cboot5$bootmean # cluster stabilities (Jaccard Scores)

# get a ton of stats based on the cluster groups
cluster.stats(d1, clustering = cboot5_grps)

# Selecting K -------------------------------------------------------------


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

# Gap statistic (Tibshirani, Walther, and Hastie 2001)
# p3 <- fviz_nbclust(ann_metrics_s, FUN = hcut, method = "gap_stat", 
#                    k.max = 8) +
#   ggtitle("Gap statistic")

# Display plots side by side
(p4 <-cowplot::plot_grid(p1, p2, nrow = 1, labels = c("B","C")))


# Plot distance to centroid for each member in classes 2 and 4 --------

member_locations <- as.data.frame(ggclust2_k5$data)

mean_x <- mean(member_locations$x)
mean_y <- mean(member_locations$y)
member_locations$dist_to_centroid <- sqrt((mean_x-member_locations$x)^2 + (mean_y-member_locations$y)^2)
head(member_locations)

member_locations_2_4 <- member_locations %>% 
  filter(cluster != "1") %>% 
  filter(cluster != "5") %>% 
  filter(cluster != "3")

# check numbers:
table(member_locations_2_4$cluster)

#Extract centers of each cluster
class_2 <- member_locations_2_4 %>% 
  filter(cluster == "2")

mean_x_2 <- mean(class_2$x)
mean_y_2 <- mean(class_2$y)

class_4 <- member_locations_2_4 %>% 
  filter(cluster == "4")

mean_x_4 <- mean(class_4$x)
mean_y_4 <- mean(class_4$y)

#Add a column that calculates distance between class centroid and member point
class_2$dist_to_centroid <- sqrt((mean_x_2-class_2$x)^2 + (mean_y_2-class_2$y)^2)

class_2 <- class_2 %>% 
  arrange(desc(dist_to_centroid))

class_4$dist_to_centroid <- sqrt((mean_x_2-class_4$x)^2 + (mean_y_2-class_4$y)^2)

class_4 <- class_4 %>% 
  arrange(desc(dist_to_centroid))

# bind together
# class_cent_df <- bind_rows(class_2, class_4) %>% 
#   rename(station_id=name)

class_cent_df <- member_locations %>% 
  rename(station_id = name)

# Map Results -------------------------------------------------------------

all_sites <- read_csv("data/data_review/gage_QA_progress.csv")

sites <- all_sites %>% 
  filter(grepl("QA complete", notes)) %>% 
  rename(station_id = site_id)

# join site data with groups
data_k <- left_join(agnes_k_groups, sites, by=c("site_id"="station_id")) %>% 
  rename(station_id=site_id) %>% 
  # drop cols we don't need
  select(station_id, k_5, site_name:operator)

# make spatial
data_k_sf <- data_k %>% 
  st_as_sf(coords = c("lon", "lat"), crs = 4326, remove = FALSE)

# map
library(sf)

# join w spatial data:
data_sf <- left_join(data_k_sf, class_cent_df[,c(1,5,6)], by=c("station_id"))

# save out dist_to_centroids
save(class_cent_df, file = "output/models/10b_dist_to_centroids_class2_4.rda")

save(class_cent_df, file = "output/models/10b_dist_to_centroids_all_classes.rda")



# hist of distances
hist(class_cent_df$dist_to_centroid)

# PCA
ann_metrics_v2 <- ann_metrics %>% 
  left_join(., class_cent_df[,c(1,5,6)]) %>% 
  select(station_id, ann_mean, ann_amp, DOWY, dist_to_centroid, cluster) %>% # select only metrics to model
  dplyr::mutate_at(vars(ann_mean:dist_to_centroid), .funs = scale) %>%
  tibble::column_to_rownames(var = "station_id") %>% 
  mutate(cluster = as.numeric(cluster))

fviz_pca_var(prcomp(ann_metrics_v2))

# so majority are around 3-4

# make map of sites less than 3
library(mapview)
mapview(data_sf, zcol="k_5", col.regions=c("#E41A1C", "#FF7F00", "#984EA3", "#4DAF4A", "#377EB8"), legend=F, cex=4) +
  mapview(data_sf %>% 
            filter(dist_to_centroid <15), 
          zcol="dist_to_centroid", cex="dist_to_centroid")
