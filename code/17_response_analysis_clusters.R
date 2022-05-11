# get elevation data and then PCA and explanatory variables per metrics:

# Libraries ---------------------------------------------------------------

library(tidyverse)
library(glue) # pasting things together
library(elevatr)
library(sf)
library(mapview)
mapviewOptions(fgb = FALSE)

# Load Data ----------------------------------------------------------------

# metrics
#load("output/models/09b_annual_cluster_metrics_all_gages.rda") # ann_metrics

# metadata
metadat <- read_csv("output/all_sites_metadata_model_results.csv") %>% 
  st_as_sf(coords=c("lon","lat"), crs=4326, remove=FALSE)

# clusters
#load("output/models/10a_agnes_k_groups_final.rda") # agnes_k_groups

# dams w DOR CDOR
#dams_dat <- readRDS("output/12b_dams_final_deg_of_reg.rds")

# ALL MERGED
dat <- read_rds("data/ann_clust_metrics_w_dor_metrics.rds") %>% 
  filter(cite=="willis")

#fill N in DOR and CDOR with zeros
dat <- dat %>% 
  mutate(
  DOR = case_when(
    is.na(DOR) ~ 0,
    TRUE ~ DOR),
  CDOR = case_when(
    is.na(CDOR) ~ 0,
    TRUE ~ CDOR)
)

# join w sf
dat <- left_join(metadat %>% select(-c(ann_mean:DOWY)), dat)

mapview(dat, zcol="k5_names")

# Get Elevation for Each Point:  -----------------------------------------

# adj points
dat_elev <- elevatr::get_elev_point(dat)

write_csv(dat_elev, "output/all_site_descriptors.csv")

hist(dat_elev$elevation, breaks = 125, col="cyan4", main="Elevation for Sites", xlab="Elevation (meters)")

# PCA OF DATA -------------------------------------------------------------

#library(tidylog)
library(factoextra) # clustering visualization/stats
library(cowplot)
library(broom)

# Get Colors --------------------------------------------------------------

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
dat_elev <- left_join(dat_elev, thermCols, by=c("k5_names"))

# make non sf version
dat_out <- dat_elev %>% st_drop_geometry()

# PCA ------------------------------------------------

# first double check for NAs
summary(dat_out)

# then scale data (mean of zero, sd =1)
ann_pca <- dat_out %>% 
  # select only metrics to model
  #select(station_id, ann_mean, ann_amp, water_day) %>% 
  # add others: 
  select(station_id, ann_mean, ann_amp, water_day, elevation, DOR, CDOR) %>% 
  select(where(is.numeric)) %>% # get only numeric and scale
  scale() %>% 
  prcomp()

# look at percent explained:
ann_pca %>%
  # extract eigenvalues
  tidy(matrix = "eigenvalues")

# scree plot
fviz_eig(ann_pca, addlabels = TRUE)

# pca plot
fviz_pca(ann_pca)

ggsave(filename = "output/figures/2022_scaled_pca_response_to_comment.png", width = 8, height = 10, dpi = 600, bg = "white")
ggsave(filename = "output/figures/2022_scaled_pca_response_to_comment.pdf", width = 8, height = 10)
# get var contributing
var <- get_pca_var(ann_pca)
head(var$contrib, 8)


## from claus wilke's slides (SD375)
# for rotational matrix
arrow_style <- arrow(
  angle = 20, length = grid::unit(8, "pt"),
  ends = "first", type = "closed"
)

## PCA Plot: PC1 v PC2 -----------------------------------------------------

# make dataframe
ann_pca_rot <- ann_pca %>%
  # extract rotation matrix
  tidy(matrix = "rotation") %>%
  pivot_wider(
    names_from = "PC", values_from = "value",
    names_prefix = "PC"
  )

# add PC coordinates and PLOT: PC1 vs P2
(p1p2 <- ann_pca %>% 
  augment(dat_out) %>% 
   ggplot(aes(.fittedPC1, .fittedPC2, label=station_id)) +
   geom_text(data=. %>% filter(station_id %in% c("CLP", "WRM", "SHD", "BSC_spring", "10265150")), nudge_y = -0.2, color="gray")+
 geom_point(aes(shape=k5_names, color=k5_names), alpha=0.8, size=4.5) + 
   theme_classic() +
   scale_color_manual("Thermal \nClasses", values=thermCols$color,
                      labels=thermCols$k5_names)+
   scale_shape_manual("Thermal \nClasses", values=c(15,16,17,18,8),
                      labels=thermCols$k5_names)+
   labs(title = "PCA of CA Thermal Regimes (k=5)",
        caption="This includes variables DOR, CDOR, elev & 3 temp metrics",
        x=glue::glue("PC1 ({round(((ann_pca$sdev[1]^2) / (sum(ann_pca$sdev^2))*100),1)}%)"), y=glue::glue("PC2 ({round(((ann_pca$sdev[2]^2) / (sum(ann_pca$sdev^2))*100),1)}%)")) +
   guides(fill = guide_legend(
     override.aes = aes(label = ""))))

#plotly::ggplotly(p1p2) to figure out outliers

ggsave(filename = "output/figures/pca_pc1_v_pc2_w_dor_cdor_elev.png",
       width = 9, height = 7, dpi=300)

## PCA Plot: PC2 v PC3 -----------------------------------------------------

# add PC coordinates and PLOT: PC2 vs P3
ann_pca %>% 
  augment(dat_out) %>% 
  ggplot(aes(.fittedPC2, .fittedPC3)) +
  geom_point(aes(shape=k5_names, color=k5_names), alpha=0.8, size=4.5) + 
  theme_classic() +
  scale_color_manual("Thermal \nClasses", values=thermCols$color,
                     labels=thermCols$k5_names)+
  scale_shape_manual("Thermal \nClasses", values=c(15,16,17,18,8),
                     labels=thermCols$k5_names)+
  labs(title = "PCA for CA Thermal Regimes (k=5)",
       caption="This includes variables DOR, CDOR, elev & 3 temp metrics",
       x="PC2", y="PC3") +
  guides(fill = guide_legend(
    override.aes = aes(label = "")))


## PCA Plot: PC1 v PC3 -----------------------------------------------------

# add PC coordinates and PLOT: PC1 vs P3
ann_pca %>% 
  augment(dat_out) %>% 
  ggplot(aes(.fittedPC1, .fittedPC3)) +
  geom_point(aes(shape=k5_names, color=k5_names), alpha=0.8, size=4.5) + 
  theme_classic() +
  scale_color_manual("Thermal \nClasses", values=thermCols$color,
                     labels=thermCols$k5_names)+
  scale_shape_manual("Thermal \nClasses", values=c(15,16,17,18,8),
                     labels=thermCols$k5_names)+
  labs(title = "PCA for CA Thermal Regimes (k=5)",
       caption="This includes variables DOR, CDOR, elev & 3 temp metrics",
       x="PC1", y="PC3") +
  guides(fill = guide_legend(
    override.aes = aes(label = "")))


## Look at Rotation Matrix -------------------------------------------------

# PC1 v PC2
ann_pca %>%
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
  xlim(-1.5, 1.5) + ylim(-1, 1) + 
  coord_fixed() +
  theme_bw()

# PC2 v PC3
ann_pca %>%
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
  xlim(-1.5, 1.5) + ylim(-1, 1) + 
  coord_fixed() +
  theme_bw()


## Plot Variance Explained -------------------------------------------------

fviz_pca_var(ann_pca) +
  theme(plot.background = element_rect(fill="white"))

ggsave(filename = "output/figures/pca_ann_metrics_w_dor_elev_rotation_matrix.png", width = 10, height = 8, units = "in", dpi=600, bg="white")

ggsave(filename = "output/figures/pca_ann_metrics_w_dor_elev_rotation_matrix.pdf", width = 10, height = 8, dpi=600)

ann_pca %>%
  # extract eigenvalues
  tidy(matrix = "eigenvalues") %>%
  ggplot(aes(PC, percent)) +
  geom_col() +
  scale_x_continuous(
    # create one axis tick per PC
    breaks = 1:6
  ) +
  scale_y_continuous(
    name = "variance explained per PC",
    # format y axis ticks as percent values
    label = scales::label_percent(accuracy = 1)
  )




## Get Variance Explained -------------------------------------------------

# get var contributing
var <- get_pca_var(ann_pca)

head(var$contrib, 6)
