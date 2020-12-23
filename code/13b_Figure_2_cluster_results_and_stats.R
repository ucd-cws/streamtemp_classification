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

load("output/models/09b_annual_cluster_metrics_all_gages.rda")
load("output/models/10a_agnes_k_groups_final.rda")

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

# plot without labels
ggclust2_k5 <- fviz_cluster(list(data=d1, cluster=hc_grps_k5), geom="point", xlab = "PC 1 (59.8%)", ylab = "PC 2 (28.8%)")

# plot with text labels
ggclust2_k5_w_lab <- fviz_cluster(list(data=d1, cluster=hc_grps_k5), geom=c("point", "text"))

# gg pca plot
ggclust2_k5 + theme_classic() +
  labs(title = "Clusters for CA Thermal Regimes (k=5)") +
  guides(fill = guide_legend(
    override.aes = aes(label = "")))

# thermColor scale
thermCols <- data.frame(k5_group_id = c(1,3,4,2,5),
                        k5_names  = c("1", "2",
                                      "3", "4",
                                      "5"),
                        color = I(c("#E41A1C", #1: stable warm
                                    "#FF7F00", #2: variable warm
                                    "#984EA3", #3: stable cool
                                    "#4DAF4A", #4: variable cool
                                    "#377EB8" #5
                        )))

# check
plot_pc_k5 <- ggclust2_k5 + theme_classic() +
  scale_fill_manual("Thermal \nClasses", values=thermCols$color, 
                    labels=thermCols$k5_names)+
  scale_color_manual("Thermal \nClasses", values=thermCols$color,
                     labels=thermCols$k5_names)+
  scale_shape_manual("Thermal \nClasses", values=c(15,16,17,18,8),
                     labels=thermCols$k5_names)+
  labs(title = "Clusters for CA Thermal Regimes (k=5)") +
  guides(fill = guide_legend(
    override.aes = aes(label = "")))

plot_pc_k5

ggsave(filename = "output/figures/pc_agnes_k5_no_labels.png", width = 11, height = 8, units = "in", dpi=600, )

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
#p3 <- fviz_nbclust(ann_metrics_s, FUN = hcut, method = "gap_stat", 
#                   k.max = 8) +
#  ggtitle("Gap statistic")

# Display plots side by side
(p4 <-cowplot::plot_grid(p1, p2, nrow = 1, labels = c("B","C")))


# Make final plots --------------------------------------------------------

fig_row_2 <- plot_grid(plot_CHIndex, plot_wss, labels = c("B", "C"), nrow = 1)

plot_grid(plot_pc_k5, p4, labels = c("A"), nrow = 2, rel_heights = c(1.5,1,1), rel_widths = c(1,.5,.5))

ggsave("output/figures/Fig_2_cluster_results_and_stats_v2.jpeg", width = 10, height = 8, units="in", dpi = 600)
