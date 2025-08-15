# ---------------------------
##
## Script name: 01.3_wetlandBenefitsTradeoffs_dataPrep.R 
##
## Purpose of script: Summarize wetland bird richness and measure surrounding wetland area
##
## Author: James Paterson, Ash Pidwerbesky
##
## Date Created: 2025-05-02
## Date modifed: 2025-05-02
## 
## Email: j_paterson@ducks.ca
##
## ---------------------------


# Description: this script takes the cleaned biodiversity detection data, sums verified species richness, 
# creates table S3, measures wetland area, and saves the final csv (113 sites with all response variables) that cleans previous versions

# Load Libraries
library(readxl)
library(dplyr)
library(sf)
library(ggplot2)
library(ggpubr)
library(gridExtra)
library(units)
library(ggpmisc)

##### 1. Load bird detections and summarize wetland bird richness data ------------------------------------

# Load cleaned and organized detections
# Only wetland bird species, only sites in study, only species detected at a site at least 2 days, only species verified present at a site
load("Data/wetlandBirdDetections_CAAF_2025-04-15.RData")

# Make species-site table
speciesSitesTable <- wetlandBirdDetections %>%
  group_by(Common.name, Scientific.name, group, SITE_ID) %>%
  summarize(num_days = length(unique(date))) %>%
  ungroup() %>%
  # filter(num_days >= 2) %>%
  mutate(Common.name.SITE_ID = paste0(Common.name, "-", SITE_ID),
         province = substr(SITE_ID, 1, 2))

# Re-create Table S3 (species, the number of sites they are at)
speciesSitesTableS3 <- speciesSitesTable %>%
  group_by(Common.name, Scientific.name, group) %>%
  summarize(numSites = n(),
            provinces = paste(unique(province), collapse = ", "))

# Save table S3
write.csv(speciesSitesTableS3,
          paste0("Outputs/tableS3_speciesSites_", Sys.Date(), ".csv"))

# How many sites?
length(unique(speciesSitesTable$SITE_ID)) # There are 57 unique sites

# How many total species?
length(unique(wetlandBirdDetections$Common.name)) # 53 species

# How many by year?
wetlandBirdDetections %>%
  group_by(year) %>%
  summarize(species = n_distinct(Common.name))

#       year species
#       <dbl>   <int>
#   1   2022      52
#   2   2023      41

# How many sites per year?
wetlandBirdDetections %>%
  group_by(year) %>%
  summarize(species = n_distinct(SITE_ID))

# How many species per wetland?
siteWetlandBirdRichness <- wetlandBirdDetections %>%
  ungroup() %>%
  group_by(SITE_ID) %>%
  summarize(richnessConfirmed = length(unique(Common.name)))

# Load SW's data and link
caafData <- read_xlsx("Data/wetland_tradeoff_df_2025-07-28.xlsx", na = "NA") # sites from SW


# Left join to confirmed richness data
caafSitesData <- caafData %>%
  left_join(siteWetlandBirdRichness,
            by = c("site_id" = "SITE_ID")) 

# Results % in different groups
birdGroupSummary <- speciesSitesTableS3 %>%
  ungroup() %>%
  group_by(group) %>%
  summarize(species_num = n(),
            species_perc = species_num/length(unique(wetlandBirdDetections$Common.name)) * 100)

##### Testing relationship between confirmed and naive richness ---------------
# Load SW's data and link
caafSites_SW_apr25 <- read.csv("Data/wetland_tradeoff_df_apr25.csv") # sites from SW

# Previous version that identifies the 65 sites in this study.
caafSitesVector <- read.csv("Data/site_df_SW.csv") %>%
  pull(site_id)

# Left join to confirmed richness data
caafSitesData_apr25 <- caafSites_SW_apr25 %>%
  left_join(siteWetlandBirdRichness,
            by = c("site_id" = "SITE_ID")) %>%
  filter(site_id %in% caafSitesVector)

# Plot previous 2 versions of richness to new one
plot(richnessConfirmed~wet_bird_richness, caafSitesData_apr25)
abline(0,1)
# plot(richnessConfirmed~richness_adj, caafSitesData)
# Both strongly correlated

summary(lm(richnessConfirmed~wet_bird_richness, caafSitesData_apr25))
# summary(lm(richnessConfirmed~richness_adj, caafSitesData))

##### 2. Measure Wetland Area ------------------------------------------------------------

# Wetland data: shapefile with CWI/ABMI wetland data in a 20km buffer around each wetland
wetlandData <- st_read("Data/Wetlands_20k_buffer.shp")

#2. Use CWI data to measure the amount of wetland around the sample site

# Convert sites dataframe into an sf object 

caafSites_sf <- caafSitesData %>% 
  st_as_sf( coords = c("longtitude_x", "latitude_y"), crs = 4326)

# Transform to Canada Equal Area Albers Projection (ESRI:102001)
caafSites_sf <- st_transform(caafSites_sf, crs = "ESRI:102001") # original crs = 32613, UTM Zone 13N (highly correlated but different)
wetlandData <- st_transform(wetlandData, crs = "ESRI:102001")

# Create buffers: 250m, 500m, 1km, 5km

buffer_250m <- st_buffer(caafSites_sf, dist = 250)
buffer_500m <- st_buffer(caafSites_sf, dist = 500)
buffer_1km <- st_buffer(caafSites_sf, dist = 1000)
buffer_5km <- st_buffer(caafSites_sf, dist = 5000)

# Intersect buffers with wetland to get the wetlands within each buffer 

wetlands_250m <- st_intersection(buffer_250m, wetlandData)
wetlands_500m <- st_intersection(buffer_500m, wetlandData)
wetlands_1km <- st_intersection(buffer_1km, wetlandData)
wetlands_5km <- st_intersection(buffer_5km, wetlandData)

# Calculate area of wetlands within each buffer 

wetlands_250m <- wetlands_250m %>% 
  mutate(area_wetland = st_area(geometry))
wetlands_500m <- wetlands_500m %>% 
  mutate(area_wetland = st_area(geometry))
wetlands_1km <- wetlands_1km %>% 
  mutate(area_wetland = st_area(geometry))
wetlands_5km <- wetlands_5km %>% 
  mutate(area_wetland = st_area(geometry))

# Summarize the area for each buffer 

caafWetlands_250m <- wetlands_250m %>% 
  group_by(site_id) %>% 
  summarize(wetlandArea_250m = sum(area_wetland)) %>% 
  st_drop_geometry()

caafWetlands_500m <- wetlands_500m %>% 
  group_by(site_id) %>% 
  summarize(wetlandArea_500m = sum(area_wetland))%>% 
  st_drop_geometry()

caafWetlands_1km <- wetlands_1km %>% 
  group_by(site_id) %>% 
  summarize(wetlandArea_1km = sum(area_wetland))%>% 
  st_drop_geometry()

caafWetlands_5km <- wetlands_5km %>% 
  group_by(site_id) %>% 
  summarize(wetlandArea_5km = sum(area_wetland))%>% 
  st_drop_geometry() 

# Join wetland 

caafWetlands <- caafWetlands_250m %>% 
  left_join(caafWetlands_500m, by = "site_id") %>% 
  left_join(caafWetlands_1km, by = "site_id") %>% 
  left_join(caafWetlands_5km, by = "site_id") %>% 
  #  left_join(richnessCompare, by = "SITE_ID") %>% 
  #  right_join(SiteList, by = "SITE_ID") %>%
  drop_units() 


##### 3. Clean old variables and save file for analyses --------------------------------------------------

caafSitesData <- caafSitesData %>%
  # Remove old area calculations and richness estimates
  dplyr::select(-wetlandArea_250m, -wetlandArea_500m, -wetlandArea_1km, -wetlandArea_5km) %>%
  # Rename wetland bird richness
  left_join(.,
            caafWetlands, 
            by = "site_id")

# Save .csv for rest of analyses
write.csv(caafSitesData, paste0("Data/wetland_tradeoff_df_", Sys.Date(), ".csv"))
# Current version "Data/wetland_tradeoff_df_2025-05-02.csv"

caafSitesData %>%
  filter(!is.na(wetlandBirdRichness)) %>%
  pull(wetlandBirdRichness) %>%
  length(.)
# 57 sites

# Summarize wetland bird richness
caafSitesData %>%
  filter(!is.na(wetlandBirdRichness)) %>%
  pull(wetlandBirdRichness) %>%
  summary(.)
