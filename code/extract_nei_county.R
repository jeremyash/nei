library(tidyverse)
library(skimr)
library(patchwork)
library(readxl)
# library(sp)
# library(rgeos)
# library(raster)
library(rgdal)
# library(scales)
# library(units)
# library(viridis)
library(tidycensus)


#----------------------------------------------------------------------------


########################################
## functions
########################################

subset_nei_data <- function(COUNTY_DF) {
  
  # nei data by year
  nei_08_sub <- nei_2008 %>% 
    mutate(year = rep("2008", n())) %>% 
    right_join(., COUNTY_DF, by = c("County", "Address"))
  
  nei_11_sub <- nei_2011 %>% 
    mutate(year = rep("2011", n())) %>% 
    right_join(., COUNTY_DF, by = c("County", "Address"))
  
  nei_14_sub <- nei_2014 %>% 
    mutate(year = rep("2014", n())) %>% 
    right_join(., COUNTY_DF, by = c("County", "Address"))
  
  nei_dat <- bind_rows(nei_08_sub, nei_11_sub, nei_14_sub)
  colnames(nei_dat)[7] <- "Units"
  return(nei_dat)
}


#----------------------------------------------------------------------------


########################################
## load nei data and relevant subsetting data
########################################

# nei data
nei_2014 <- read_csv("raw_data/nei_2014.csv")
nei_2011 <- read_csv("raw_data/nei_2011.csv")
nei_2008 <- read_csv("raw_data/nei_2008.csv")


# fips codes
fips <- fips_codes %>%
  dplyr::select(state_code, Address = state) %>%
  distinct()
  

# data to join to
r9_county <- readOGR("gis/region_9_county")
r9_county_df <- r9_county@data %>% 
  mutate_if(is.factor, as.character) %>% 
  dplyr::select(state_code = STATEFP, County = NAME) %>% 
  left_join(., fips, by = "state_code") %>% 
  dplyr::select(-state_code)

#----------------------------------------------------------------------------


########################################
## subset nei data to counties
########################################

r9_nei_dat <- subset_nei_data(r9_county_df)
saveRDS(r9_nei_dat, "data/r9_nei_dat.RDS")


poll_of_interest <- c("PM10 Primary (Filt + Cond)",
                      "PM2.5 Primary (Filt + Cond)",
                      "Nitrogen Oxides",
                      "Sulfur Dioxide")


nei_summ_county <- r9_nei_dat %>% 
  mutate(emissions_tons = ifelse(Units == "LBS", Emissions*0.0005, Emissions)) %>% 
  group_by(year, Address, County, Pollutant) %>% 
  summarise(total_emissions_tons = sum(emissions_tons)) %>% 
  ungroup() %>%
  filter(Pollutant %in% poll_of_interest)
saveRDS(nei_summ_county, "data/r9_nei_dat_summary.RDS")


nei_summ_region <- nei_summ_county %>% 
  group_by(year, Pollutant) %>% 
  summarise(total_emissions_tons = sum(total_emissions_tons)) %>% 
  ungroup()



#----------------------------------------------------------------------------


