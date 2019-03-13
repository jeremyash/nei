library(tidyverse)
library(skimr)
library(patchwork)
library(readxl)
library(sp)
library(rgeos)
library(raster)
library(rgdal)
# library(scales)
# library(units)
# library(viridis)
library(tidycensus)
library(sf)


#----------------------------------------------------------------------------

########################################
## LOAD DATA
########################################
planning <- readOGR("gis/planning_area")

county <- readOGR("gis/cb_2017_us_county_5m")
county <- spTransform(county, CRS = proj4string(planning))

states <- readOGR("gis/states")
states <- spTransform(states, CRS = proj4string(planning))

wayne <- readOGR("gis/wayne_nf")
wayne <- spTransform(wayne, CRS = proj4string(planning))

wayne_simple <- readOGR("gis/wayne_nf_simple")
wayne_simple <- spTransform(wayne_simple, CRS = proj4string(planning))

#----------------------------------------------------------------------------

########################################
## INTERSECT FOREST, STATE AND COUNTIES
########################################

# state_county <- intersect(county, states)
# plot(state_county)
# saveRDS(state_county, file="data/state_county")

wayne_200km <- gBuffer(wayne_simple, width = 200000)
# plot(planning)
# plot(wayne_200km)
# plot(wayne, add = TRUE)
saveRDS(wayne_200km, file = "data/wayne_airshed.RDS")


airshed_counties <- intersect(wayne_200km, county)
plot(airshed_counties)  

airshed_geoid <- airshed_counties@data %>% 
  dplyr::select(GEOID) %>%
  mutate(GEOID = as.character(GEOID)) %>% 
  distinct() %>% 
  pull(GEOID)
  


county_airshed <- county  
county_airshed <- county_airshed[county_airshed@data$GEOID %in% airshed_geoid, ]

writeOGR(county_airshed,
         dsn = "gis/wayne_airshed",
         layer = "wayne_airshed",
         driver = "ESRI Shapefile")

saveRDS(county_airshed, "data/airshed_counties.RDS")


plot(county_airshed)
plot(planning, add = TRUE, col = "darkred")
  
  
#----------------------------------------------------------------------------

########################################
## PLOTTING
########################################

states_latlong <- spTransform(states, CRS("+proj=longlat +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +no_defs"))
planning_latlong <- spTransform(planning, CRS("+proj=longlat +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +no_defs"))
county_airshed_latlong <- spTransform(county_airshed, CRS("+proj=longlat +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +no_defs"))
wayne_latlong <- spTransform(wayne_simple, CRS("+proj=longlat +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +no_defs"))


# #fips codes
# fips <- fips_codes %>% 
#   dplyr::select(state_code, state_name) %>% 
#   distinct() 
# 

# subset states
state_fips <- county_airshed_latlong@data %>% 
  dplyr::select(STATEFP) %>% 
  mutate_if(is.factor, as.character) %>% 
  distinct() %>% 
  pull(STATEFP)
  
states_sub_latlong <- states_latlong[states_latlong@data$STATEFP %in% state_fips,]




state_df <- broom::tidy(states_latlong)
state_sub_df <- broom::tidy(states_sub_latlong)
# saveRDS(states_sub_latlong, "data/state_sub_spdf.RDS")
planning_df <- broom::tidy(planning_latlong)  
airshed_df <- broom::tidy(county_airshed_latlong)
wayne_df <- broom::tidy(wayne_latlong)


# airshed map
ggplot(state_sub_df, aes(long, lat, group = group)) +
  geom_polygon(fill = NA, color = "grey15", size = 1) +
  geom_polygon(aes(long, lat, group = group, color = "cadetblue3"), 
               fill = "white",
               alpha = 0.9,
               size = 0.8, 
               show.legend = TRUE,
               data=airshed_df) +
  geom_polygon(aes(long, lat, group = group,  fill = "darkgoldenrod1"),
               color = NA,
               alpha = 0.8,
               show.legend = TRUE,
               data = planning_df) +
  geom_polygon(aes(long, lat, group = group, fill = "darkgreen"),
               color = NA,
               show.legend = TRUE,
               data = wayne_df) +
  scale_color_manual(values = c("cadetblue3"), 
                    labels = c("Airshed (124 mile buffer)"),
                               name = NULL) +
  scale_fill_manual(values = c("darkgoldenrod1", "darkgreen"), 
                    labels = c("Planning Area", "Wayne NF"),
                    name = NULL) +
  theme_minimal() +
  coord_map() +
  labs(title = "Planning Area and Airshed") +
  theme(axis.text = element_blank(),
        axis.title = element_blank(),
        panel.grid.major = element_blank(),
        plot.title = element_text(size = 20),
        legend.position = "bottom",
        legend.text = element_text(size = 14)) +
  guides(fill = guide_legend(order = 1, reverse = TRUE),
         color = guide_legend(order = 2, override.aes = list(shape = 0,
                                                             size = 6,
                                                             color = "cadetblue3",
                                                             fill = "white")))


ggsave(filename = "figures/airshed_200km.jpg", 
       height = 4, 
       width = 7, 
       units = "in")
 #---------------------------------------------------------------------------

########################################
## larger airshed
########################################

state_air <- crop(states_latlong, extent(-106.5, -66.5, 25, 49.2))
state_air_df <- broom::tidy(state_air)


# airshed map
ggplot(state_air_df, aes(long, lat, group = group)) +
  geom_polygon(fill = NA, color = "grey15", size = 1) +
  geom_polygon(aes(long, lat, group = group, color = "cadetblue3"), 
               fill = "white",
               alpha = 0.9,
               size = 0.8, 
               show.legend = TRUE,
               data=airshed_df) +
  geom_polygon(aes(long, lat, group = group,  fill = "darkgoldenrod1"),
               color = NA,
               alpha = 0.8,
               show.legend = TRUE,
               data = planning_df) +
  geom_polygon(aes(long, lat, group = group, fill = "darkgreen"),
               color = NA,
               show.legend = TRUE,
               data = wayne_df) +
  scale_color_manual(values = c("cadetblue3"), 
                     labels = c("Airshed (200km buffer)"),
                     name = NULL) +
  scale_fill_manual(values = c("darkgoldenrod1", "darkgreen"), 
                    labels = c("Planning Area", "Wayne NF"),
                    name = NULL) +
  theme_minimal() +
  coord_map() +
  theme(axis.text = element_blank(),
        axis.title = element_blank(),
        panel.grid.major = element_blank(),
        legend.position = "bottom",
        legend.text = element_text(size = 14),
        plot.margin = margin(0,0,0,0)) +
  guides(fill = guide_legend(order = 1, reverse = TRUE),
         color = guide_legend(order = 2, override.aes = list(shape = 0,
                                                             size = 6,
                                                             color = "cadetblue3",
                                                             fill = "white")))

ggsave(filename = "figures/ladco_airshed.jpg", 
       height = 4, 
       width = 7, 
       units = "in")
