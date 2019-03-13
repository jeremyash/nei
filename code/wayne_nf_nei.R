## SPATIAL
library(sp)
library(rgeos)
library(raster)
library(rgdal)
library(maptools)

## DATA MANAGEMENT
library(tidyverse)
library(skimr)
library(patchwork)
library(readxl)
# library(zoo)

# PLOTTING
library(scales)
library(units)
library(viridis)
library(extrafont)
library(grid)
library(tidycensus)
library(gtable)

#----------------------------------------------------------------------------
########################################
## functions + font misc
########################################


theme_map <- function(...) {
  theme_minimal() +
    theme(
      text = element_text(color = "black"),
      plot.title = element_text(face = "bold"),
      axis.line = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      # panel.grid.minor = element_line(color = "#ebebe5", size = 0.2),
      panel.grid.major = element_line(color = "#ebebe5", size = 0.2),
      panel.grid.minor = element_blank(),
      plot.background = element_rect(fill = "white", color = NA), 
      panel.background = element_rect(fill = "white", color = NA), 
      legend.background = element_rect(fill = "white", color = NA),
      panel.border = element_blank(),
      ...
    )
}




#----------------------------------------------------------------------------


########################################
## LOAD DATA
########################################
unit <- readOGR("../fs_admin/data/wayne_nf")


county <- readOGR("gis/cb_2017_us_county_5m")
county <- spTransform(county, CRS = proj4string(unit))

states <- readOGR("gis/states")
states <- spTransform(states, CRS = proj4string(unit))


#----------------------------------------------------------------------------

########################################
## INTERSECT FOREST, STATE AND COUNTIES
########################################

# state_county <- intersect(county, states)
# plot(state_county)
# saveRDS(state_county, file="data/state_county")

unit_buffer <- gBuffer(unit, width = 200000)


unit_counties <- raster::intersect(unit_buffer, county)
plot(unit_counties)  

unit_geoid <- unit_counties@data %>% 
  dplyr::select(GEOID) %>%
  mutate(GEOID = as.character(GEOID)) %>% 
  distinct() %>% 
  pull(GEOID)

county_unit <- county  
county_unit <- county_unit[county_unit@data$GEOID %in% unit_geoid, ]


#----------------------------------------------------------------------------

########################################
## PLOTTING
########################################

states_latlong <- spTransform(states, CRS("+proj=longlat +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +no_defs"))
unit_latlong <- spTransform(unit, CRS("+proj=longlat +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +no_defs"))
unit_county_latlong <- spTransform(county_unit, CRS("+proj=longlat +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +no_defs"))



# #fips codes
# fips <- fips_codes %>% 
#   dplyr::select(state_code, state_name) %>% 
#   distinct() 
# 

# subset states
state_fips <- unit_county_latlong@data %>% 
  dplyr::select(STATEFP) %>% 
  mutate_if(is.factor, as.character) %>% 
  distinct() %>% 
  pull(STATEFP)

states_sub_latlong <- states_latlong[states_latlong@data$STATEFP %in% state_fips,]




state_df <- broom::tidy(states_latlong)
state_sub_df <- broom::tidy(states_sub_latlong)
buffer_df <- broom::tidy(unit_county_latlong)
unit_df <- broom::tidy(unit_latlong)


# airshed map
ggplot(state_sub_df, aes(long, lat, group = group)) +
  geom_polygon(fill = NA, color = "grey15", size = 0.6) +
  geom_polygon(aes(long, lat, group = group, color = "cadetblue3"), 
               fill = "white",
               alpha = 0.9,
               size = 0.6, 
               show.legend = TRUE,
               data=buffer_df) +
  geom_polygon(aes(long, lat, group = group, fill = "darkgreen"),
               color = NA,
               show.legend = TRUE,
               data = unit_df) +
  scale_color_manual(values = c("cadetblue3"), 
                     labels = c("200km buffer"),
                     name = NULL) +
  scale_fill_manual(values = c("darkgreen"), 
                    labels = c("wayne NF"),
                    name = NULL) +
  theme_minimal() +
  coord_map() +
  # labs(title = "Planning Area and Airshed") +
  theme(axis.text = element_blank(),
        axis.title = element_blank(),
        panel.grid.major = element_blank(),
        plot.title = element_text(size = 12),
        legend.position = "bottom",
        legend.text = element_text(size = 10)) +
  guides(fill = guide_legend(order = 1, reverse = TRUE),
         color = guide_legend(order = 2, override.aes = list(shape = 0,
                                                             size = 6,
                                                             color = "cadetblue3",
                                                             fill = NA)))


ggsave(filename = "figures/wayne_nf_buffer_map.jpg", 
       height = 3, 
       width = 4.2, 
       units = "in")
#---------------------------------------------------------------------------



########################################
## load data
########################################

# fips codes
fips <- fips_codes %>%
  dplyr::select(state_code, Address = state) %>%
  distinct()

# buffer
county_df <- county_unit@data %>% 
  mutate_if(is.factor, as.character) %>% 
  dplyr::select(state_code = STATEFP, County = NAME) %>% 
  left_join(., fips, by = "state_code") %>% 
  dplyr::select(-state_code)
county_airshed_sf <- sf::st_as_sf(county_unit)


# nei data by year
nei_08 <- read_csv("raw_data/nei_2008.csv") %>% 
  mutate(year = rep("2008", n())) %>% 
  right_join(., county_df, by = c("County", "Address"))

nei_11 <- read_csv("raw_data/nei_2011.csv")%>% 
  mutate(year = rep("2011", n())) %>% 
  right_join(., county_df, by = c("County", "Address"))

nei_14 <- read_csv("raw_data/nei_2014.csv") %>% 
  mutate(year = rep("2014", n())) %>% 
  right_join(., county_df, by = c("County", "Address"))


nei_dat <- bind_rows(nei_08, nei_11, nei_14)
colnames(nei_dat)[7] <- "Units"

sort(unique(nei_dat$Pollutant))

# nei data
poll_of_interest <- c("PM10 Primary (Filt + Cond)",
                      "PM2.5 Primary (Filt + Cond)",
                      "Nitrogen Oxides",
                      "Sulfur Dioxide")
# 
# poll_of_interest <- c("PM10 Filterable",
#                       "PM2.5 Filterable",
#                       "Nitrogen Oxides",
#                       "Sulfur Dioxide")


# standardize sectors within pollutants
poll_sectors <- nei_dat %>% 
  filter(Pollutant %in% poll_of_interest) %>% 
  dplyr::select(Pollutant, year, Sector) %>% 
  distinct() %>% 
  group_by(Pollutant, Sector) %>% 
  summarise(n_years = n()) %>% 
  ungroup() 
  

# summed emissions by county
nei_summ_county <- nei_dat %>% 
  mutate(emissions_tons = ifelse(Units == "LBS", Emissions*0.0005, Emissions)) %>% 
  group_by(year, Address, County, Pollutant) %>% 
  summarise(total_emissions_tons = sum(emissions_tons)) %>% 
  ungroup() %>%
  filter(Pollutant %in% poll_of_interest) %>% 
  spread(Pollutant, total_emissions_tons) %>% 
  left_join(., fips, by = "Address")


colnames(nei_summ_county)[4:7] <- c("nox",
                        "pm10",
                        "pm2_5",
                        "so2")


nei_summ_airshed <- nei_summ_county %>% 
  gather(pollutant, emissions, nox:so2) %>% 
  group_by(year, pollutant) %>% 
  summarise(total_emissions = sum(emissions)) %>% 
  ungroup() %>% 
  mutate(year = paste("x", year, sep = "")) %>% 
  spread(year, total_emissions) %>% 
  mutate(percent_change = ((x2014-x2008)/x2008)*100)
# 
# write_csv(nei_summ_airshed,
#           "data/nei_summ_airshed.csv")



#----------------------------------------------------------------------------



########################################
## change over time
########################################


nei_change <- nei_summ_county %>% 
  mutate(year = paste("x_", year, sep = "")) %>% 
  gather(Pollutant, total_emissions_tons, nox:so2) %>% 
  spread(year, total_emissions_tons) %>% 
  # mutate(delta_08_14 = x_2014 - x_2008) %>% 
  # dplyr::select(-x_2008, -x_2011, -x_2014) %>% 
  # spread(Pollutant, delta_08_14) %>% 
  mutate(delta_11_14 = x_2014 - x_2011) %>% 
  dplyr::select(-x_2008, -x_2011, -x_2014) %>% 
  spread(Pollutant, delta_11_14) %>% 
  rename(NAME = County, STATEFP = state_code)

unit_county_delta <- county_unit 
unit_county_delta@data <- left_join(unit_county_delta@data, nei_change, by = c("NAME", "STATEFP"))
unit_county_delta_sf <-  sf::st_as_sf(unit_county_delta)


unit_states_sf <- sf::st_as_sf(states_sub_latlong)




#nox
ggplot(unit_county_delta_sf, aes(nox)) +
  geom_histogram()

# plotting info for all time periods
no_classes <- 8
pretty_quants <- c(-45000, -1000, -500, 0, 500, 1000, 10300)

labels_scale <- c(">1000", "1000", "500", "0",  "-500","-1000")
# here I actually create a new 
# variable on the dataset with the quantiles
unit_county_delta_sf$nox_quantiles <- cut(unit_county_delta_sf$nox, 
                                        breaks = pretty_quants, 
                                        labels = rev(labels_scale), 
                                        include.lowest = T)
unit_county_delta_sf$nox_quantiles <- factor(unit_county_delta_sf$nox_quantiles, levels = levels(unit_county_delta_sf$nox_quantiles))

brks_scale <- levels(unit_county_delta_sf$nox_quantiles)

col_pal <- brewer_pal(palette = "RdYlBu")(6)

nox_plot <- ggplot() +
  geom_sf( fill = NA, color = "grey20", data = unit_states_sf) +
  geom_sf( fill = NA, color = "grey20", data = county_airshed_sf) +
  geom_sf(aes(fill = nox_quantiles), data = unit_county_delta_sf) +
  theme_map() +
  theme(legend.position = "bottom",
        plot.margin = unit(c(0.2,.2,.2,.2), "cm"),
        panel.spacing = unit(c(-.1,0.2,.2,0.2), "cm"),
        panel.border = element_blank()) +
  labs(title = expression(paste("Change in ", NO[x], " Emissions", sep = ""))) +
  scale_fill_manual(
    # in manual scales, one has to define colors, well, manually
    # I can directly access them using viridis' magma-function
    values = rev(col_pal),
    breaks = rev(brks_scale),
    name = "tons",
    drop = FALSE,
    labels = labels_scale,
    guide = guide_legend(
      direction = "horizontal",
      keyheight = unit(2.5, units = "mm"),
      keywidth = unit(12, units = "mm"),
      title.position = 'left',
      # I shift the labels around, the should be placed 
      # exactly at the right end of each legend key
      title.hjust = 0.5,
      label.hjust = 1,
      nrow = 1,
      byrow = T,
      # also the guide needs to be reversed
      reverse = T,
      label.position = "bottom"
    )
  ) +
  theme(plot.title  = element_text(size = 12, face = "bold"),
        legend.title = element_text(size = 12, vjust = 1),
        legend.text = element_text(size = 10),
        panel.grid.major = element_line(color = "white"),
        panel.grid.minor = element_line(color = "white"))

# 
# add_min_legend <- function(p){
#   p_grob <- ggplotGrob(p)
#   legend <- gtable_filter(p_grob, "guide-box")
#   legend_grobs <- legend$grobs[[1]]$grobs[[1]]
#   
#   # # grab the last label so we can also shift its position
#   legend_last_label <- gtable_filter(legend_grobs, "label-3-8")
#   legend_last_label$grobs[[1]]$x <- unit(2, units = "cm")
#   
#   # finally, I need to create a new label for the minimum value 
#   new_first_label <- legend_last_label$grobs[[1]]
#   new_first_label[["children"]][[1]][["children"]][[1]][["label"]] <- "<-1000"
#   new_first_label$x <- 0 #unit(-5, units = "cm")
#   new_first_label[["children"]][[1]][["children"]][[1]][["hjust"]] <- 0.6
#   
#   legend_grobs <- gtable_add_grob(legend_grobs, 
#                                   new_first_label, 
#                                   t = 4, 
#                                   l = 3, 
#                                   name = "label-3-2", 
#                                   clip = "off")
#   legend$grobs[[1]]$grobs[1][[1]] <- legend_grobs
#   p_grob$grobs[p_grob$layout$name == "guide-box"][[1]] <- legend
#   
#   # the plot is now drawn using this grid function
#   grid.newpage()
#   grid.draw(p_grob)
# }
# 
# 
# 
# jpeg("figures/unit_nf_nox_change_11_14.jpg", height = 3, width = 4.2, units  = "in", res = 300)
# 
# add_min_legend(nox_plot)
# 
# dev.off()

ggsave(filename = "figures/wayne_nf_nox_change.jpg",
       plot = nox_plot,
       height = 3,
       width = 4.2,
       units = "in")

#so2
ggplot(unit_county_delta_sf, aes(so2)) +
  geom_histogram()

# plotting info for all time periods
no_classes <- 8
pretty_quants <- c(-45000, -1000, -500, 0, 500, 1000, 10300)

labels_scale <- c(">1000", "1000", "500", "0",  "-500","-1000")
# here I actually create a new 
# variable on the dataset with the quantiles
unit_county_delta_sf$so2_quantiles <- cut(unit_county_delta_sf$so2, 
                                        breaks = pretty_quants, 
                                        labels = rev(labels_scale), 
                                        include.lowest = T)
unit_county_delta_sf$so2_quantiles <- factor(unit_county_delta_sf$so2_quantiles, levels = levels(unit_county_delta_sf$so2_quantiles))

brks_scale <- levels(unit_county_delta_sf$so2_quantiles)


col_pal <- brewer_pal(palette = "RdYlBu")(6)



so2_plot <- ggplot() +
  geom_sf( fill = NA, color = "grey20", data = unit_states_sf) +
  geom_sf( fill = NA, color = "grey20", data = county_airshed_sf) +
  geom_sf(aes(fill = so2_quantiles), data = unit_county_delta_sf) +
  theme_map() +
  theme(legend.position = "bottom",
        plot.margin = unit(c(0.2,.2,.2,.2), "cm"),
        panel.spacing = unit(c(-.1,0.2,.2,0.2), "cm"),
        panel.border = element_blank()) +
  labs(title = expression(paste("Change in ", SO[2], " Emissions", sep = ""))) +
  scale_fill_manual(
    # in manual scales, one has to define colors, well, manually
    # I can directly access them using viridis' magma-function
    values = rev(col_pal),
    breaks = rev(brks_scale),
    name = "tons",
    drop = FALSE,
    labels = labels_scale,
    guide = guide_legend(
      direction = "horizontal",
      keyheight = unit(2.5, units = "mm"),
      keywidth = unit(12, units = "mm"),
      title.position = 'left',
      # I shift the labels around, the should be placed 
      # exactly at the right end of each legend key
      title.hjust = 0.5,
      label.hjust = 1,
      nrow = 1,
      byrow = T,
      # also the guide needs to be reversed
      reverse = T,
      label.position = "bottom"
    )
  ) +
  theme(plot.title  = element_text(size = 12, face = "bold"),
        legend.title = element_text(size = 12, vjust = 1),
        legend.text = element_text(size = 10),
        panel.grid.major = element_line(color = "white"),
        panel.grid.minor = element_line(color = "white"))


# jpeg("figures/so2_change_11_14.jpg", height = 4, width = 7, units  = "in", res = 300)
# 
# add_min_legend(so2_plot)
# 
# dev.off()

ggsave(filename = "figures/wayne_nf_so2_change.jpg",
       plot = so2_plot,
       height = 3,
       width = 4.2,
       units = "in")


# #pm10
# ggplot(unit_county_delta_sf, aes(pm10)) +
#   geom_histogram()
# 
# # plotting info for all time periods
# no_classes <- 8
# pretty_quants <- c(-45000, -1000, -500, 0, 500, 1000, 10300)
# 
# labels_scale <- c(">1000", "1000", "500", "0",  "-500","-1000")
# # here I actually create a new 
# # variable on the dataset with the quantiles
# unit_county_delta_sf$pm10_quantiles <- cut(unit_county_delta_sf$pm10, 
#                                            breaks = pretty_quants, 
#                                            labels = rev(labels_scale), 
#                                            include.lowest = T)
# unit_county_delta_sf$pm10_quantiles <- factor(unit_county_delta_sf$pm10_quantiles, levels = levels(unit_county_delta_sf$pm10_quantiles))
# 
# brks_scale <- levels(unit_county_delta_sf$pm10_quantiles)
# 
# 
# col_pal <- brewer_pal(palette = "RdYlBu")(6)
# 
# 
# 
# pm10_plot <- ggplot() +
#   geom_sf( fill = NA, color = "grey20", data = unit_states_sf) +
#   geom_sf( fill = NA, color = "grey20", data = county_airshed_sf) +
#   geom_sf(aes(fill = pm10_quantiles), data = unit_county_delta_sf) +
#   theme_map() +
#   theme(legend.position = "bottom",
#         plot.margin = unit(c(0.2,.2,.2,.2), "cm"),
#         panel.spacing = unit(c(-.1,0.2,.2,0.2), "cm"),
#         panel.border = element_blank()) +
#   labs(title = expression(paste("Change in ", PM[10], " Emissions", sep = ""))) +
#   scale_fill_manual(
#     # in manual scales, one has to define colors, well, manually
#     # I can directly access them using viridis' magma-function
#     values = rev(col_pal),
#     breaks = rev(brks_scale),
#     name = "tons",
#     drop = FALSE,
#     labels = labels_scale,
#     guide = guide_legend(
#       direction = "horizontal",
#       keyheight = unit(5, units = "mm"),
#       keywidth = unit(20, units = "mm"),
#       title.position = 'left',
#       # I shift the labels around, the should be placed 
#       # exactly at the right end of each legend key
#       title.hjust = 0.5,
#       label.hjust = 1,
#       nrow = 1,
#       byrow = T,
#       # also the guide needs to be reversed
#       reverse = T,
#       label.position = "bottom"
#     )
#   ) +
#   theme(plot.title  = element_text(size = 20, face = "bold"),
#         legend.title = element_text(size = 18, vjust = 1),
#         legend.text = element_text(size = 12)) 
# 
# 
# jpeg("figures/pm10_change_11_14.jpg", height = 4, width = 7, units  = "in", res = 300)
# 
# add_min_legend(pm10_plot)
# 
# dev.off()




#pm2.5
ggplot(unit_county_delta_sf, aes(pm2_5)) +
  geom_histogram()

# plotting info for all time periods
no_classes <- 8
pretty_quants <- c(-45000, -1000, -500, 0, 500, 1000, 10300)

labels_scale <- c(">1000", "1000", "500", "0",  "-500","-1000")
# here I actually create a new 
# variable on the dataset with the quantiles
unit_county_delta_sf$pm2_5_quantiles <- cut(unit_county_delta_sf$pm2_5, 
                                            breaks = pretty_quants, 
                                            labels = rev(labels_scale), 
                                            include.lowest = T)
unit_county_delta_sf$pm2_5_quantiles <- factor(unit_county_delta_sf$pm2_5_quantiles, levels = levels(unit_county_delta_sf$pm2_5_quantiles))

brks_scale <- levels(unit_county_delta_sf$pm2_5_quantiles)


col_pal <- brewer_pal(palette = "RdYlBu")(6)



pm2_5_plot <- ggplot() +
  geom_sf( fill = NA, color = "grey20", data = unit_states_sf) +
  geom_sf( fill = NA, color = "grey20", data = county_airshed_sf) +
  geom_sf(aes(fill = pm2_5_quantiles), data = unit_county_delta_sf) +
  theme_map() +
  theme(legend.position = "bottom",
        plot.margin = unit(c(0.2,.2,.2,.2), "cm"),
        panel.spacing = unit(c(-.1,0.2,.2,0.2), "cm"),
        panel.border = element_blank()) +
  labs(title = expression(paste("Change in ", PM[2.5], " Emissions", sep = ""))) +
  scale_fill_manual(
    # in manual scales, one has to define colors, well, manually
    # I can directly access them using viridis' magma-function
    values = rev(col_pal),
    breaks = rev(brks_scale),
    name = "tons",
    drop = FALSE,
    labels = labels_scale,
    guide = guide_legend(
      direction = "horizontal",
      keyheight = unit(2.5, units = "mm"),
      keywidth = unit(12, units = "mm"),
      title.position = 'left',
      # I shift the labels around, the should be placed 
      # exactly at the right end of each legend key
      title.hjust = 0.5,
      label.hjust = 1,
      nrow = 1,
      byrow = T,
      # also the guide needs to be reversed
      reverse = T,
      label.position = "bottom"
    )
  ) +
  theme(plot.title  = element_text(size = 12, face = "bold"),
        legend.title = element_text(size = 12, vjust = 1),
        legend.text = element_text(size = 10),
        panel.grid.major = element_line(color = "white"),
        panel.grid.minor = element_line(color = "white"))


# jpeg("figures/so2_change_11_14.jpg", height = 4, width = 7, units  = "in", res = 300)
# 
# add_min_legend(so2_plot)
# 
# dev.off()

ggsave(filename = "figures/wayne_nf_pm_2_5_change.jpg",
       plot = pm2_5_plot,
       height = 3,
       width = 4.2,
       units = "in")

########################################
## change over time in facet plot
########################################

# net data
nei_change <- nei_summ_county %>% 
  mutate(year = paste("x_", year, sep = "")) %>% 
  gather(Pollutant, total_emissions_tons, nox:so2) %>% 
  spread(year, total_emissions_tons) %>% 
  # mutate(delta_08_14 = x_2014 - x_2008) %>% 
  # dplyr::select(-x_2008, -x_2011, -x_2014) %>% 
  # spread(Pollutant, delta_08_14) %>% 
  mutate(delta_11_14 = x_2014 - x_2011) %>% 
  dplyr::select(-x_2008, -x_2011, -x_2014) %>% 
  rename(NAME = County, STATEFP = state_code) %>% 
  spread(Pollutant, delta_11_14)
  
  
# join to make sf  
unit_county_delta <- county_unit 
unit_county_delta@data <- left_join(unit_county_delta@data, nei_change, by = c("NAME", "STATEFP"))
unit_county_delta_sf <-  sf::st_as_sf(unit_county_delta)

  
# plotting info for all time periods
no_classes <- 8
pretty_quants <- c(-45000, -1000, -500, 0, 500, 1000, 10300)
labels_scale <- c(">1000", "1000", "500", "0",  "-500","-1000")


# gather pollutants and filter out PM10
unit_county_delta_sf <- unit_county_delta_sf %>% 
  gather(pollutant, emissions, nox:so2) %>% 
  filter(pollutant != "pm10")


# here I actually create a new 
# variable on the dataset with the quantiles
unit_county_delta_sf$emissions_quantiles <- cut(unit_county_delta_sf$emissions, 
                                         breaks = pretty_quants, 
                                         labels = rev(labels_scale), 
                                         include.lowest = T)
unit_county_delta_sf$emissions_quantiles <- factor(unit_county_delta_sf$emissions_quantiles, levels = levels(unit_county_delta_sf$emissions_quantiles))


# factor pollutants
unit_county_delta_sf$pollutant <- factor(unit_county_delta_sf$pollutant,
                                        levels = c("nox", "so2", "pm2_5"),
                                        labels = c("NO[x]", "SO[2]", "PM[2.5]"))

# plotting details
brks_scale <- levels(unit_county_delta_sf$emissions_quantiles)
col_pal <- brewer_pal(palette = "RdYlBu")(6)
unit_states_sf <- sf::st_as_sf(states_sub_latlong)


# faceted plot___long
ggplot() +
  geom_sf( fill = NA, color = "grey20", data = unit_states_sf) +
  geom_sf( fill = NA, color = "grey20", data = county_airshed_sf) +
  geom_sf(aes(fill = emissions_quantiles), data = unit_county_delta_sf) +
  theme_map() +
  facet_wrap(~pollutant, 
             ncol = 1,
             strip.position = "left",
             labeller = label_parsed) +
  theme(legend.position = "bottom") +
  # plot.margin = unit(c(0.2,.2,.2,.2), "cm"),
  #       panel.spacing = unit(c(-.1,0.2,.2,0.2), "cm"),
  #       panel.border = element_blank()) +
  labs(title = "Change in Emissions (2008-2014)") +
  scale_fill_manual(
    # in manual scales, one has to define colors, well, manually
    # I can directly access them using viridis' magma-function
    values = rev(col_pal),
    breaks = rev(brks_scale),
    name = "tons",
    drop = FALSE,
    labels = labels_scale,
    guide = guide_legend(
      direction = "horizontal",
      keyheight = unit(4, units = "mm"),
      keywidth = unit(11, units = "mm"),
      title.position = 'left',
      # I shift the labels around, the should be placed 
      # exactly at the right end of each legend key
      title.hjust = 0.5,
      label.hjust = 1,
      nrow = 1,
      byrow = T,
      # also the guide needs to be reversed
      reverse = T,
      label.position = "bottom")) +
   theme(plot.title  = element_text(size = 12, face = "bold", hjust = 1),
        legend.title = element_text(size = 12, vjust = 1),
        legend.text = element_text(size = 8),
        panel.grid.major = element_line(color = "white"),
        panel.grid.minor = element_line(color = "white"),
        strip.text.y = element_text(size = 12,  margin = margin( b = 0, t = 0), angle = 180),
        panel.spacing = unit(0.1, "lines"))

ggsave(filename = "figures/wayne_nf_nei_change_long.jpg",
       height = 8,
       width = 3.7,
       units = "in")




#############################################################################
## nei x pollutant by sector
#############################################################################


# nei data
poll_of_interest <- c("PM10 Primary (Filt + Cond)",
                      "PM2.5 Primary (Filt + Cond)",
                      "Nitrogen Oxides",
                      "Sulfur Dioxide")

nei_poll_county_sector <- nei_dat %>% 
  filter(Pollutant %in% poll_of_interest) %>% 
  group_by(year, Sector, Pollutant) %>% 
  summarise(total_emissions_tons = sum(Emissions)) %>% 
  ungroup() %>%
  mutate(xyear = paste("x", year, sep = "")) %>% 
  dplyr::select(-year) %>% 
  spread(xyear, total_emissions_tons) %>% 
  mutate(delta_08_14 = x2014 - x2008) %>% 
  group_by(Pollutant) %>%
  arrange(Pollutant, desc(delta_08_14)) %>% 
  ungroup()
  
  
write_csv(nei_poll_county_sector,
          "data/wayne_nei_pollutant_sector.csv")

































