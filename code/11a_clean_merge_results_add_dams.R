
# Code description --------------------------------------------------------

# This code add the Shasta coordinates to the gage list and maps the cluster results from code 10_calculate_model_metrics_for_classification


# Libraries ---------------------------------------------------------------

library(mapview)
library(sf)
library(tidyverse)
library(ggthemes)
library(ggrepel)
library(ggspatial)

#added these packages to interactively select dams:
library(mapedit)
library(leaflet)
library(leafpm)

# Load data ---------------------------------------------------------------

hydro_regions <- read_sf("data/shps//DWR_HydrologicRegions-utm11.shp") %>%
  st_transform(4326) %>% 
  rmapshaper::ms_simplify(keep = .01) # simplify
# check size
#pryr::object_size(hydro_regions)

dams <- read_sf("data/shps/CA_OR_dams.shp", quiet = F) %>% st_transform(4326)

all_sites <- read_csv("data/data_review/gage_QA_progress.csv")

sites <- all_sites %>% 
  filter(grepl("QA complete", notes)) %>% 
  rename(station_id = site_id)

# get rivers
load("output/12_selected_nhd_mainstems_for_gages.rda")

# Add K_group Data --------------------------------------------------------

# different classification grouping method
load("output/models/agnes_k_groups_final.rda") # agnes
agnes_k_groups <- agnes_k_groups %>% 
  rename(station_id = site_id)


load("output/models/classification_group_results.rda") # class_groups
unique(agnes_k_groups$station_id)

# find differences between lists:
anti_join(sites, agnes_k_groups) # localities in sites df not in agnes_k_groups
anti_join(agnes_k_groups, sites) # localities in agnes_k_groups not in sites

# join and save
data_k <- left_join(agnes_k_groups, sites) %>% 
  # drop cols we don't need
  select(station_id, k_5, site_name:operator)


# Add Colors and Group Labels ---------------------------------------------

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
data_k <- left_join(data_k, thermCols, by=c("k_5"="k5_group_id")) %>% 
  select(station_id, k_5, k5_names, color, site_name:operator)

# how many per group?
table(data_k$k_5)
table(data_k$k5_names)


# Add Centroid Distance Data -------------------------------------------------

# add the centroid dist data:
load("output/models/dist_to_centroids_class2_4.rda")
class_cent_df <- class_cent_df %>% mutate(station_id = as.character(name)) %>% 
  select(-name)

# now join with full data set
data_k <- left_join(data_k, class_cent_df)

# make spatial
data_k_sf <- data_k %>% 
  st_as_sf(coords = c("lon", "lat"), crs = 4326, remove = FALSE)

## SAVE?
save(data_k_sf, file = "output/models/11_agnes_k_5_final_w_centdist.rda")

# clean up
rm(sites, all_sites, cboot5_df, agnes_k_groups, data_k)


# FILTER TO CA DAMS --------------------------------------------------------------------

# clip to only dams in CA
ca <- USAboundaries::us_boundaries(type="state", states="ca")
dams <- dams[ca,] # now clip (spatial join to only ca)

# get nearest dams
dams_nearest <- dams[st_nearest_feature(data_k_sf, dams),]

#filter dams that are irrelevant to the stream segment
dams_nearest_filtered <- dams_nearest %>% 
  filter(!OBJECTID %in% c(656, 613, 733, 655, 689, 739, 715, 589, 719, 218, 543, 670, 71, 665, 792, 213, 692, 199, 133, 731, 684, 761, 833, 676))

# save out data for future mapping
save(dams, dams_nearest_filtered, file = "output/models/11_ca_dams_nearest_k_sites.rda")

# setup some basemaps
mapbases <- c("Stamen.TonerLite","OpenTopoMap", "CartoDB.PositronNoLabels", "OpenStreetMap",
              "Esri.WorldImagery", "Esri.WorldTopoMap","Esri.WorldGrayCanvas"
)
mapviewOptions(basemaps=mapbases)

# map k5
m5 <- mapview(dams_nearest_filtered, col.regions="black",
                layer.name="Selected Dams", cex=6,
                hide=TRUE, homebutton=FALSE)+
  mapview(dams, col.regions="gray50", alpha.regions=0.5, cex=3.4, layer.name="All Dams") +
  mapview(mainstems_all, color="steelblue", cex=3, 
          layer.name="NHD Flowlines") +
  mapview(data_k_sf,  zcol="k5_names", map.types=mapbases,
          layer.name="Thermal Classes",
          col.regions=unique(thermCols$color), 
          alpha.regions=0.8, cex=7,
          hide=FALSE, homebutton=FALSE) 

m5@map %>% leaflet::addMeasure(primaryLengthUnit = "meters")


# Select dams to add to dams_nearest_filtered (manually with mapedit) -------------

#dams_selected <- selectFeatures(dams, map = m5) #Commented out once features were selected to preserve object

#dams_nearest_all <- rbind(dams_nearest_filtered, dams_selected)

#Review selected dams
m5b <- mapview(dams_nearest_all, col.regions="black",
              layer.name="Selected Dams", cex=6,
              hide=TRUE, homebutton=FALSE)+
  mapview(dams, col.regions="gray50", alpha.regions=0.5, cex=3.4, layer.name="All Dams") +
  mapview(mainstems_all, color="steelblue", cex=3, 
          layer.name="NHD Flowlines") +
  mapview(data_k_sf,  zcol="k_5_f", map.types=mapbases,
          layer.name="Thermal Classes",
          col.regions=unique(thermCols$color), 
          alpha.regions=0.8, cex=7,
          hide=FALSE, homebutton=FALSE) 

m5b@map %>% leaflet::addMeasure(primaryLengthUnit = "meters")

#filter out downstream or unrelated dams
dams_nearest_all_filtered <- dams_nearest_all %>% 
  filter(!OBJECTID %in% c(98, 837, 868, 96, 592, 5, 720))

#update map
m5c <- mapview(dams_nearest_all_filtered, col.regions="black",
              layer.name="Selected Dams", cex=6,
              hide=TRUE, homebutton=FALSE)+
  mapview(dams, col.regions="gray50", alpha.regions=0.5, cex=3.4, layer.name="All Dams") +
  mapview(mainstems_all, color="steelblue", cex=3, 
          layer.name="NHD Flowlines") +
  mapview(data_k_sf,  zcol="k_5_f", map.types=mapbases,
          layer.name="Thermal Classes",
          col.regions=unique(thermCols$color), 
          alpha.regions=0.8, cex=7,
          hide=FALSE, homebutton=FALSE) 

m5c@map %>% leaflet::addMeasure(primaryLengthUnit = "meters")

#Add New Melones
New_Melones <- selectFeatures(dams, map = m5c)

dams_nearest_all_filtered <-  rbind(dams_nearest_all_filtered, New_Melones)

#Add Big Springs Dam
Big_Springs_Dam = editMap(m5c)

Big_Springs_Dam_object <- Big_Springs_Dam$finished

Big_Springs_Dam_sf <- Big_Springs_Dam_object %>% 
  select(geometry) %>% 
  mutate(NAME = "Big Springs Dam", COUNTY = "Siskiyou", RIVER = "Big Springs Creek", OBJECTID = "NA", NID = "NA", HEIGHT_FT = "NA", STOR_AF = "NA", BASIN_SQMI = "NA", STO_m3 = "NA", STO_10_6m3 = "NA", NEAR_FID = "NA", NEAR_DIST = "NA", damname = "NA", damheight = "NA", nidstorage = "NA", file_nbr = "NA", basin_nbr = "NA", location = "NA", latitude = "NA", longitude = "NA", inspdate = "NA")

Big_Springs_Dam_sf <- Big_Springs_Dam_sf %>% 
  select(OBJECTID, NID, NAME, COUNTY, RIVER, HEIGHT_FT, STOR_AF, BASIN_SQMI, STO_m3, STO_10_6m3, NEAR_FID, NEAR_DIST, damname, damheight, nidstorage, file_nbr, basin_nbr, location, latitude, longitude,inspdate, geometry)

# Save final dams list ----------------------------------------------------


# save out data for future mapping
save(dams_nearest_all_filtered, file = "output/models/11_dams_nearest_all_filtered.rda")
save(Big_Springs_Dam_sf, file = "output/models/11_Big_Springs_Dam.rda")

#Try to combine dams_nearest_all_filtered and Big_Springs_Dam_sf
dams_nearest_all_final <- rbind(dams_nearest_all_filtered, Big_Springs_Dam_sf)

save(dams_nearest_all_final, file = "output/models/11_dams_nearest_all_final.rda")

#Plot and review - 

m5d <- mapview(dams_nearest_all_final, col.regions="black",
              layer.name="Selected Dams", cex=6,
              hide=TRUE, homebutton=FALSE)+
  #mapview(dams, col.regions="gray50", alpha.regions=0.5, cex=3.4, layer.name="All Dams") +
  mapview(mainstems_all, color="steelblue", cex=3, 
          layer.name="NHD Flowlines") +
  mapview(data_k_sf,  zcol="k5_names", map.types=mapbases,
          layer.name="Thermal Classes",
          col.regions=unique(thermCols$color), 
          alpha.regions=0.8, cex=7,
          hide=FALSE, homebutton=FALSE) 

m5d@map %>% leaflet::addMeasure(primaryLengthUnit = "meters")


# Static map for k5 --------------------------------------------------------------

ggplot()+
  geom_sf(data = hydro_regions, fill = NA, color = 'slategray4', size = 1, alpha = 0.4) +
  geom_sf(data = data_k_sf, aes(fill = k5_names), pch = 21, size = 3) +
  scale_fill_manual("Thermal Class", values = thermCols$color)+
  geom_sf_text(data = hydro_regions, aes(label = HR_NAME), check_overlap = TRUE, color="black") +
  annotation_north_arrow(location = "tr", pad_y = unit(0.1, "cm"), ) +
  annotation_scale() +
  labs(x="", y="")+
  coord_sf(label_axes = "----")+
  #hrbrthemes::theme_modern_rc(grid = FALSE) +
  theme(panel.background = element_blank(),
        legend.key = element_rect(fill="transparent"))
  #theme_classic(base_family = "Roboto Condensed")
