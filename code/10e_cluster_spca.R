# Code description  ---------------------------------------------------------------

# PCA and sPCA of cluster + metric data

# Libraries ---------------------------------------------------------------

library(tidyverse)
library(tidylog)
library(factoextra) # clustering visualization/stats
library(cowplot)
library(broom)

# Data --------------------

# metrics
load("output/models/09b_annual_cluster_metrics_all_gages.rda") # ann_metrics

# clusters
load("output/models/10a_agnes_k_groups_final.rda") # agnes_k_groups


# Wrangle -----------------------------------------------------------------

# pick k5 clusters
agnes_k <- agnes_k_groups %>% 
  rename(station_id = site_id) %>% 
  select(station_id, k_5)


# thermColor scale
thermCols <- data.frame(k5_group_id = c(1,3,4,2,5),
                        k5_names  = as.factor(c("1-stable warm", "2-variable warm",
                                                "3-stable cool", "4-variable cool",
                                                "5-stable cold")),
                        color = I(c("#E41A1C", #stable warm
                                    "#FF7F00", #variable warm
                                    "#984EA3", #stable cool
                                    "#4DAF4A", #variable cool
                                    "#377EB8" #stable cold
                        )))

# join w data
agnes_k <- left_join(agnes_k, thermCols, by=c("k_5"="k5_group_id")) %>% 
  select(station_id, k_5, k5_names, color)

# add to ann_metrics
ann_metrics_k <- left_join(ann_metrics, agnes_k, by="station_id")

# PCA ------------------------------------------------

# first double check for NAs
summary(ann_metrics_k)

# then scale data (mean of zero, sd =1)
ann_metrics_pca <- ann_metrics_k %>% 
  select(station_id, ann_mean, ann_amp, DOWY) %>% # select only metrics to model
  select(where(is.numeric)) %>% # get only numeric and scale
  scale() %>% 
  prcomp()

ann_metrics_pca


## PCA Plot: PC1 v PC2 -----------------------------------------------------

# add PC coordinates and PLOT: PC1 vs P2
ann_metrics_pca %>% 
  augment(ann_metrics_k) %>% 
  ggplot(aes(.fittedPC1, .fittedPC2)) +
  geom_point(aes(shape=k5_names, color=k5_names), alpha=0.8, size=4.5) + 
  theme_classic() +
  scale_color_manual("Thermal \nClasses", values=thermCols$color,
                     labels=thermCols$k5_names)+
  scale_shape_manual("Thermal \nClasses", values=c(15,16,17,18,8),
                     labels=thermCols$k5_names)+
  labs(title = "PCA of CA Thermal Regimes (k=5)",
       x="PC1", y="PC2") +
  guides(fill = guide_legend(
    override.aes = aes(label = "")))

## PCA Plot: PC2 v PC3 -----------------------------------------------------

# add PC coordinates and PLOT: PC2 vs P3
ann_metrics_pca %>% 
  augment(ann_metrics_k) %>% 
  ggplot(aes(.fittedPC2, .fittedPC3)) +
  geom_point(aes(shape=k5_names, color=k5_names), alpha=0.8, size=4.5) + 
  theme_classic() +
  scale_color_manual("Thermal \nClasses", values=thermCols$color,
                     labels=thermCols$k5_names)+
  scale_shape_manual("Thermal \nClasses", values=c(15,16,17,18,8),
                     labels=thermCols$k5_names)+
  labs(title = "PCA for CA Thermal Regimes (k=5)",
       x="PC2", y="PC3") +
  guides(fill = guide_legend(
    override.aes = aes(label = "")))


## PCA Plot: PC1 v PC3 -----------------------------------------------------

# add PC coordinates and PLOT: PC1 vs P3
ann_metrics_pca %>% 
  augment(ann_metrics_k) %>% 
  ggplot(aes(.fittedPC1, .fittedPC3)) +
  geom_point(aes(shape=k5_names, color=k5_names), alpha=0.8, size=4.5) + 
  theme_classic() +
  scale_color_manual("Thermal \nClasses", values=thermCols$color,
                     labels=thermCols$k5_names)+
  scale_shape_manual("Thermal \nClasses", values=c(15,16,17,18,8),
                     labels=thermCols$k5_names)+
  labs(title = "PCA for CA Thermal Regimes (k=5)",
       x="PC1", y="PC3") +
  guides(fill = guide_legend(
    override.aes = aes(label = "")))


## Look at Rotation Matrix -------------------------------------------------

# from claus wilke's slides (SD375)

arrow_style <- arrow(
  angle = 20, length = grid::unit(8, "pt"),
  ends = "first", type = "closed"
)

# PC1 v PC2
ann_metrics_pca %>%
  # extract rotation matrix
  tidy(matrix = "rotation") %>%
  pivot_wider(
    names_from = "PC", values_from = "value",
    names_prefix = "PC"
  ) %>%
  ggplot(aes(PC1, PC2)) +
  geom_segment(
    xend = 0, yend = 0,
    arrow = arrow_style
  ) +
  geom_text(aes(label = column), hjust = 1) +
  xlim(-1.5, 0.5) + ylim(-1, 1) + 
  coord_fixed()

# PC2 v PC3
ann_metrics_pca %>%
  # extract rotation matrix
  tidy(matrix = "rotation") %>%
  pivot_wider(
    names_from = "PC", values_from = "value",
    names_prefix = "PC"
  ) %>%
  ggplot(aes(PC2, PC3)) +
  geom_segment(
    xend = 0, yend = 0,
    arrow = arrow_style
  ) +
  geom_text(aes(label = column), hjust = 1) +
  xlim(-1.5, 0.5) + ylim(-1, 1) + 
  coord_fixed()


## Plot Variance Explained -------------------------------------------------

ann_metrics_pca %>%
  # extract eigenvalues
  tidy(matrix = "eigenvalues") %>%
  ggplot(aes(PC, percent)) + 
  geom_col() + 
  scale_x_continuous(
    # create one axis tick per PC
    breaks = 1:6
  ) +
  scale_y_continuous(
    name = "variance explained",
    # format y axis ticks as percent values
    label = scales::label_percent(accuracy = 1)
  )


# Sparse PCA ------------------------------------------------

# see here: https://github.com/erichson/spca
library(sparsepca)
 
# first double check for NAs
summary(ann_metrics_k)

# then scale data (mean of zero, sd =1)
ann_metrics_spca <- ann_metrics_k %>% 
  select(ann_mean, ann_amp, DOWY) %>% # select only metrics
  scale() #%>% 
ann_metrics_spca <- rspca(ann_metrics_spca, k = 3, alpha=1e-4, beta=1e-1, verbose=1, max_iter=1000, tol=1e-4, center=TRUE, scale=TRUE)

# compare with regular PCA
summary(ann_metrics_spca) 
summary(ann_metrics_pca)

# very comparable. SPCA is a bit more conservative, but explains most of the variation in the data.

## SPCA Plot: PC1 v PC2 -----------------------------------------------------

# add PC coordinates and PLOT: PC1 vs P2
ann_metrics_spca$scores %>% as.data.frame() %>% 
  bind_cols(ann_metrics_k)  %>% 
  ggplot(aes(V1, V2)) +
  geom_point(aes(shape=k5_names, color=k5_names), alpha=0.8, size=4.5) + 
  theme_classic() +
  scale_color_manual("Thermal \nClasses", values=thermCols$color,
                     labels=thermCols$k5_names)+
  scale_shape_manual("Thermal \nClasses", values=c(15,16,17,18,8),
                     labels=thermCols$k5_names)+
  labs(title = "SPCA of CA Thermal Regimes (k=5)",
       x="PC1", y="PC2") +
  guides(fill = guide_legend(
    override.aes = aes(label = "")))

## SPCA Plot: PC2 v PC3 -----------------------------------------------------

# add PC coordinates and PLOT: PC2 vs P3
ann_metrics_spca$scores %>% as.data.frame() %>% 
  bind_cols(ann_metrics_k)  %>% 
  ggplot(aes(V2, V3)) +
  geom_point(aes(shape=k5_names, color=k5_names), alpha=0.8, size=4.5) + 
  theme_classic() +
  scale_color_manual("Thermal \nClasses", values=thermCols$color,
                     labels=thermCols$k5_names)+
  scale_shape_manual("Thermal \nClasses", values=c(15,16,17,18,8),
                     labels=thermCols$k5_names)+
  labs(title = "SPCA of CA Thermal Regimes (k=5)",
       x="PC2", y="PC3") +
  guides(fill = guide_legend(
    override.aes = aes(label = "")))

## SPCA Plot: PC1 v PC3 -----------------------------------------------------

# add PC coordinates and PLOT: PC1 vs P3
ann_metrics_spca$scores %>% as.data.frame() %>% 
  bind_cols(ann_metrics_k)  %>% 
  ggplot(aes(V1, V3)) +
  geom_point(aes(shape=k5_names, color=k5_names), alpha=0.8, size=4.5) + 
  theme_classic() +
  scale_color_manual("Thermal \nClasses", values=thermCols$color,
                     labels=thermCols$k5_names)+
  scale_shape_manual("Thermal \nClasses", values=c(15,16,17,18,8),
                     labels=thermCols$k5_names)+
  labs(title = "SPCA of CA Thermal Regimes (k=5)",
       x="PC1", y="PC3") +
  guides(fill = guide_legend(
    override.aes = aes(label = "")))

## Look at Rotation Matrix -------------------------------------------------

arrow_style <- arrow(
  angle = 20, length = grid::unit(8, "pt"),
  ends = "first", type = "closed"
)

# PC1 v PC2
ann_metrics_spca$transform %>% as.data.frame() %>%
  bind_cols(., "column"=c("ann_mean","ann_amp", "DOWY")) %>% 
  rename(PC1=V1, PC2=V2, PC3=V3) %>% 
  ggplot(aes(PC1, PC2)) +
  geom_segment(
    xend = 0, yend =  0,
    arrow = arrow_style
  ) +
  geom_text(aes(label = column), hjust = 1) +
  xlim(-0.5, 1) + ylim(-1, 1) + 
  coord_fixed()

# PC2 v PC3
ann_metrics_spca$transform %>% as.data.frame() %>%
  bind_cols(., "column"=c("ann_mean","ann_amp", "DOWY")) %>% 
  rename(PC1=V1, PC2=V2, PC3=V3) %>% 
  ggplot(aes(PC2, PC3)) +
  geom_segment(
    xend = 0, yend =  0,
    arrow = arrow_style
  ) +
  geom_text(aes(label = column), hjust = 1) +
  xlim(-0.5, 1) + ylim(-1, 1) + 
  coord_fixed()

# PC1 v PC3
ann_metrics_spca$transform %>% as.data.frame() %>%
  bind_cols(., "column"=c("ann_mean","ann_amp", "DOWY")) %>% 
  rename(PC1=V1, PC2=V2, PC3=V3) %>% 
  ggplot(aes(PC1, PC3)) +
  geom_segment(
    xend = 0, yend =  0,
    arrow = arrow_style
  ) +
  geom_text(aes(label = column), hjust = 1) +
  xlim(-0.5, 1) + ylim(-1, 1) + 
  coord_fixed()


## Plot Variance Explained -------------------------------------------------

ann_metrics_spca$eigenvalues %>% as.data.frame() %>% 
  janitor::clean_names() %>% 
  rename(eig = x) %>% 
  mutate(prop_explain = eig / sum(eig),
         percent = prop_explain*100, 
         tot_percent = percent + lag(percent, default = 46.24092),
         PC = c("PC1", "PC2", "PC3")) %>% 
  ggplot(aes(PC, percent)) + 
  geom_col() + 
  scale_x_discrete(
    # create one axis tick per PC
    breaks = 1:3
  ) +
  scale_y_continuous(
    name = "variance explained",
    # format y axis ticks as percent values
    label = scales::label_percent(scale = 1, accuracy = 1)
  )
