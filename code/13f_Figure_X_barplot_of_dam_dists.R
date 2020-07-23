# Bar Plot of Dam Distances
library(sf)
library(tidylog)
library(ggthemes)
library(hrbrthemes)
library(tidyverse)


# Load Data ----------------------------------------------------------------

load("output/12_data_k_centdist_damdist.rda")

#max distance to stable cool point
stable_cool <- data_k_dist %>% 
  filter(k5_names == "3-stable cool") %>% 
  filter(!is.na(cum_len_km))

max_dist_stable_cool <- max(stable_cool$cum_len_km)

stable_cool[stable_cool$cum_len_km == max_dist_stable_cool, 1]
stable_cool[stable_cool$station_id == "JLF", 18]
stable_cool[stable_cool$station_id == "NFH", 18]

variable_warm <- data_k_dist %>% 
  filter(k5_names == "2-variable warm") %>% 
  filter(!is.na(cum_len_km)) %>% 
  arrange(cum_len_km)



# Set Color Palette -------------------------------------------------------

# assign color palette based on classifications:
thermCols <- data.frame(k5_group_id = c(1:5),
                        k5_names  = c("1-stable warm", "2-variable warm",
                                      "3-stable cool", "4-variable cool",
                                      "5-stable cold"),
                        color = I(c("#E41A1C", #stable warm
                                    "#FF7F00", #variable warm
                                    "#984EA3", #stable cool
                                    "#4DAF4A", #variable cool
                                    "#377EB8" #stable cold
                        )))


# Get Counts --------------------------------------------------------------

# get group counts
data_k_dist %>% st_drop_geometry %>% 
  group_by(k_5, k5_names) %>% tally() %>% 
  left_join(., thermCols) -> grp_cnts



# Plot Mean Distance to Dam Per Group -------------------------------------

# now look at mean dist to dam per group
data_k_dist %>% st_drop_geometry %>% 
  group_by(k_5) %>%  
  summarize(mean_dam_dist_km = mean(cum_len_km, na.rm=TRUE)) %>%
  left_join(grp_cnts) %>%
  filter(!is.na(mean_dam_dist_km)) %>% #iew()
  ggplot() + geom_col(aes(x= k5_names, y=mean_dam_dist_km, fill=color), alpha=0.8) + hrbrthemes::theme_ft_rc() +
  labs(x="Thermal Class", y="Mean Distance to U/S Dam (km)")

#ggsave("output/figures/13e_mean_dist_to_dam_by_k5.png", width = 8, height = 6, units = "in", dpi = 300)

# Plot Histogram of Dist To Dam for Group 2 & 3 ---------------------------


# filter out to just reg warm/reg cool
data_k_dist %>% st_drop_geometry() %>% 
  filter(k5_names %in% c("2-variable warm", "3-stable cool")) %>%
  #group_by(k_5, k5_names) %>% 
  ggplot() + 
  geom_histogram(aes(x=cum_len_km, fill=color), alpha=0.65, color=NA, binwidth = 3, bins = 50) +
  geom_histogram(aes(x=cum_len_km, color=color), fill=NA, lwd=0.1, binwidth = 3, bins = 50) +
  facet_grid(k5_names~.) + 
  scale_x_continuous(breaks=seq(0,100,10))+
  labs(x= "Distance below dam (km)", y="Count") +
  theme_clean(base_size = 14)

ggsave("output/figures/13e_hist_dist_to_dam_by_stable_cool_variable_warm_facet_vertical.png", width = 8, height = 6, units = "in", dpi = 300)

# now add some annotation?
library(ggrepel)

# make clean dataset
dat_2_3 <- data_k_dist %>% st_drop_geometry() %>% 
  filter(k5_names %in% c("2-variable warm", "3-stable cool"), !is.na(cum_len_km))

# plot
ggplot(data=dat_2_3) + 
  geom_histogram(aes(x=cum_len_km, fill=color), alpha=0.65, color=NA, binwidth = 3, bins = 50) +
  geom_histogram(aes(x=cum_len_km, color=color), fill=NA, lwd=0.1, binwidth = 3, bins = 50) +
  facet_grid(k5_names~.) + 
  scale_x_continuous(breaks=seq(0,100,10))+
  labs(x= "Distance below dam (km)", y="Count") +
  theme_clean(base_size = 14) +
  theme(panel.grid.major.y = element_line(color=alpha("gray", 0.65))) +
  geom_text_repel(data=dat_2_3 %>% filter(station_id=="SCQ"), 
                  aes(x = 1, y=1, label="Tule R - SCQ\n(Success)"), size=3, 
                  point.padding=0.2, nudge_x = 0.5, nudge_y = 1, arrow=arrow(length = unit(0.05, "npc"))) +
  geom_text_repel(data=dat_2_3 %>% filter(station_id=="BBQ"), size=3,
                  aes(x = 3, y=1, label="Stoney Ck - BBQ \n(Black Butte)"), 
                  point.padding=0.2, nudge_x = 12, nudge_y = .5, arrow=arrow(length = unit(0.05, "npc"))) +
  geom_text_repel(data=dat_2_3 %>% filter(station_id=="ORA"), size=3,
                  aes(x = 48, y=2, label="Stanislaus R - ORA\n(New Melones)"),
                  point.padding=0.2, nudge_x = 2, nudge_y = 1, arrow=arrow(length = unit(0.05, "npc"))) +
  geom_text_repel(data=dat_2_3 %>% filter(station_id=="11467000"), size=3,
                  aes(x = 42, y=2, label="Dry Ck - USGS\n(Warm Springs)"),
                  point.padding=0.2, nudge_x = 2, nudge_y = 1, arrow=arrow(length = unit(0.05, "npc"))) +
  geom_text_repel(data=dat_2_3 %>% filter(station_id=="GRF"), size=3,
                  aes(x = 63, y=2, label="San Joaquin R - GRF (Friant)"),
                  point.padding=0.2, nudge_x = 2, nudge_y = 1, arrow=arrow(length = unit(0.05, "npc"))) +
  geom_text_repel(data=dat_2_3 %>% filter(station_id=="FWQ"), size=3,
                aes(y = 5, x=1, label="San Joaquin R - FWQ (Friant)\nDry Ck - WRM (Warm Springs)"), 
                point.padding=0.2, nudge_x = 15, arrow=arrow(length = unit(0.04, "npc")))+
  geom_text_repel(data=dat_2_3 %>% filter(station_id=="11303000"), size=3,
                  aes(x = 84, y=1, label="Stanislaus R - USGS\n(New Melones)"),
                  point.padding=0.2, nudge_x = -3, nudge_y = 1, arrow=arrow(length = unit(0.05, "npc")))

ggsave("output/figures/13e_hist_dist_to_dam_by_stable_cool_variable_warm_facet_vertical_labeled.png", width = 8.5, height = 6, units = "in", dpi = 300)

# try unfaceted:
# data_k_dist %>% st_drop_geometry() %>% 
#   filter(k5_names %in% c("2-variable warm", "3-stable cool")) %>%
#   group_by(k_5, k5_names) %>% 
#   ggplot() + 
#   geom_histogram(aes(x=cum_len_km, fill=color), color=NA, alpha=0.65, binwidth = 3, bins = 50) +
#   geom_histogram(aes(x=cum_len_km, color=color), fill=NA, lwd=0.1, binwidth = 3, bins = 50) +
#   scale_x_continuous(breaks=seq(0,100,10))+
#   labs(y="Count", x="Distance downstream of dam (km)") +
#   theme_clean()
# 
# ggsave("output/figures/13e_hist_dist_to_dam_by_stable_cool_variable_warm_stacked.png", width = 8, height = 6, units = "in", dpi = 300)
