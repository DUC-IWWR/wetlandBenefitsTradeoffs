## ---------------------------
##
## Script name: 01.1_CAAFsitesBirdNetDataOrg.R 
##
## Purpose of script: Combine data from BirdNET detections into detection file for analyses
##
## Author: James Paterson
##
## Date Created: 2023-03-09
## Date modifed: 2025-05-02
## 
## Email: j_paterson@ducks.ca
##
## ---------------------------

# Load Libraries
library(dplyr)
library(magrittr)
library(ggplot2)
library(readxl)

##### 1. BirdNET Species List ----------------------------------------------------

# Load species list for prairie Canada (based on biodiversity mapping project)
bird_species <- read.csv(file = "Data/prairieBiodiversityAllSpecies.csv") %>%
  filter(group == "birds")
amphibian_species <- read.csv(file = "Data/prairieBiodiversityAllSpecies.csv") %>%
  filter(group == "amphibians") %>%
  mutate(genus = ifelse(genus == "Hyla",
                        "Dryophytes", genus),
         # common_name = ifelse(common_name == "Boreal Chorus Frog",
         #                      "Striped Chorus Frog",
         #                      common_name),
         # species = ifelse(species == "maculata",
         #                  "triseriata", species)
         )

# For bird habitat guilds, use Avian Conservation Assessment Database 
acad <- read.csv("Data/ACAD Global 2021.02.05-filtered.csv") %>%
  # Fix Common.Names before filter ("-" issues), plus a few name "updates"
  mutate(Common.Name = ifelse(Common.Name == "Northern Pygmy-Owl",
                              "Northern Pygmy-owl",
                              Common.Name),
         Common.Name = ifelse(Common.Name == "Black-crowned Night-Heron",
                              "Black-crowned Night-heron",
                              Common.Name),
         Common.Name = ifelse(Common.Name == "Eastern Screech-Owl",
                              "Eastern Screech-owl",
                              Common.Name),
         Common.Name = ifelse(Common.Name == "Eastern Whip-poor-will",
                              "Eastern Whip-Poor-Will",
                              Common.Name),
         Common.Name = ifelse(Common.Name == "Eastern Wood-Pewee",
                              "Eastern Wood-pewee",
                              Common.Name),
         Common.Name = ifelse(Common.Name == "Eurasian Collared-Dove",
                              "Eurasian Collared-dove",
                              Common.Name),
         Common.Name = ifelse(Common.Name == "Canada Jay",
                              "Gray Jay",
                              Common.Name),
         Common.Name = ifelse(Common.Name == "Greater Sage-Grouse",
                              "Greater Sage-grouse",
                              Common.Name),
         Common.Name = ifelse(Common.Name == "MacGillivray's Warbler",
                              "Macgillivray's Warbler",
                              Common.Name),
         Common.Name = ifelse(Common.Name ==  "McCown's Longspur",
                              "Thick-billed Longspur",
                              Common.Name),
         Common.Name = ifelse(Common.Name ==  "Western Wood-Pewee",
                              "Western Wood-pewee",
                              Common.Name)) %>%
  filter(Common.Name %in% bird_species$common_name) %>%
  # str_split creates a list, use sapply to output part of string before ":"
  mutate(habitat = sapply(stringr::str_split(Primary.Breeding.Habitat, pattern = ":"),
                          FUN = function(x) return(x[[1]][1])),
         # Fix some habitats (secondary breeding habitat in a separate column)
         habitat = ifelse(habitat %in% c("Wetlands Aerial", "Coasts"),
                          "Wetlands",
                          habitat),
         habitat = ifelse(habitat == "Forest Aerial",
                          "Forests",
                          habitat),
         habitat = ifelse(habitat == "Open Country Aerial",
                          "Open Country",
                          habitat),
         # There are 3 "Tundra" species that have different secondary breeding spots White-Tailed Ptarmigan (true Tundra),
         # Short-eared Owl (grassland), and White-crowned sparrow (Tundra)
         habitat = ifelse(Common.Name == "Short-eared Owl",
                          "Grasslands",
                          habitat) )
# Should be same row number as bird_species

# Get summary of Primary.Breeding.Habitat
acad_habitat_summary <- acad %>%
  group_by(habitat, Group) %>% # Primary.Breeding.Habitat
  summarize(total = n())

# Use simple breeding habitats (those before ":" to group associations, and "Group" for more specific (e.g., waterfowl, landbird, shorebird))


##### 2. Load 2022 Data -------------------------------------------------

# Read BirdNET output
birdnet_files <- list.files(path = "Data/birdNetOutput2022", pattern = ".csv", recursive = TRUE)
  
birdnet_file_list <- list()

# Loop through
for(i in 1:length(birdnet_files)){ # length(birdnet_files)
  birdnet_file_list[[i]] <- read.csv(file = paste0("Data/birdNetOutput2022/", birdnet_files[i])) %>%
    mutate(filename = birdnet_files[i],
           unique_id = stringr::str_split(birdnet_files[i], "/")[[1]][1],
           file_part2 = stringr::str_split(birdnet_files[i], "/")[[1]][2],
           date = as.Date(substr(file_part2, 10, 17), format = "%Y%m%d")) %>%
    # Catch cases (majority) where .csv is in subfolder for a download (the norm after first few manual sites)
    # In those cases, fix file_part2 and re-calculate date from file_part2
    mutate(file_part2 = ifelse(is.na(date),
                               stringr::str_split(birdnet_files[i], "/")[[1]][4],
                               file_part2),
           date = as.Date(substr(file_part2, 10, 17), format = "%Y%m%d"),
           file_part2 = ifelse(is.na(date),
                               stringr::str_split(birdnet_files[i], "/")[[1]][3],
                               file_part2),
           date = as.Date(substr(file_part2, 10, 17), format = "%Y%m%d"))
    
}

  
# Join into one dataframe
birdnet_detections <- do.call(rbind.data.frame, birdnet_file_list)

# Check species not on prairie breeding bird list or amphibian species' list
birdnet_other <- birdnet_detections %>%
  filter(!Common.name %in% c(bird_species$common_name, amphibian_species$common_name)) 

birdnet_otherspecies <- birdnet_detections %>%
  filter(!Common.name %in% c(bird_species$common_name, amphibian_species$common_name)) %>%
  filter(Confidence >= 0.75) %>%
  group_by(Common.name) %>%
  summarize(total = n(),
            max_confidence = max(Confidence),
            mean_confidence = mean(Confidence))

# Filter to high-confidence detections, and fix some common name issues
birdnet_detections <- birdnet_detections %>%
  filter(Confidence >= 0.75) %>%
  # Fixing some common name issues
  mutate(Common.name = ifelse(Common.name == "Black-crowned Night-Heron",
                              "Black-crowned Night-heron",
                              Common.name),
         # Paraguayan Snipe = Wilson's Snipe (only snipe))
         Common.name = ifelse(Common.name == "Paraguayan Snipe",
                              "Wilson's Snipe",
                              Common.name),
         # Common Snipe = Wilson's Snipe (only snipe))
         Common.name = ifelse(Common.name == "Common Snipe",
                              "Wilson's Snipe",
                              Common.name),
         # Upland Chorus Frog = Boreal Chorus Frog
         Common.name = ifelse(Common.name == "Upland Chorus Frog",
                              "Boreal Chorus Frog",
                              Common.name),
         # Striped = Western Chorus Frog = same call as Boreal Chorus Frog
         Common.name = ifelse(Common.name == "Striped Chorus Frog",
                              "Boreal Chorus Frog",
                              Common.name),
         Common.name = ifelse(Common.name == "Brimley's Chorus Frog",
                              "Boreal Chorus Frog",
                              Common.name),
         Common.name = ifelse(Common.name == "Spotted Chorus Frog",
                              "Boreal Chorus Frog",
                              Common.name),
         Common.name = ifelse(Common.name == "Southern Chorus Frog",
                              "Boreal Chorus Frog",
                              Common.name),
         # Now "LeConte's Sparrow in species list
         # Common.name = ifelse(Common.name == "LeConte's Sparrow",
         #                      "Le Conte's Sparrow",
         #                      Common.name),
         Common.name = ifelse(Common.name == "Eastern Wood-Pewee",
                              "Eastern Wood-pewee",
                              Common.name),
         # Now "LeConte's Sparrow in species list
         # Common.name = ifelse(Common.name == "LeConte's Sparrow",
         #                      "Le Conte's Sparrow",
         #                      Common.name),
         # American Toad = Canadian Toad
         Common.name = ifelse(Common.name == "American Toad",
                              "Canadian Toad",
                              Common.name),
         # Greater Sage-Grouse = Greater Sage-grouse
         Common.name = ifelse(Common.name == "Greater Sage-Grouse",
                              "Greater Sage-grouse",
                              Common.name),
         #Eurasian Collared-Dove = Eurasian Collared-dove)
         Common.name = ifelse(Common.name == "Eurasian Collared-Dove",
                              "Eurasian Collared-dove",
                              Common.name),
         # Yellow-billed Magpie = Black-billed Magpie
         Common.name = ifelse(Common.name == "Yellow-billed Magpie",
                              "Black-billed Magpie",
                              Common.name),
         #Chihuahuan Raven = Common Raven
         Common.name = ifelse(Common.name == "Chihuahuan",
                              "Common Raven",
                              Common.name) ) %>%
  # Filter to species in the prairies
  filter(Common.name %in% c(bird_species$common_name, amphibian_species$common_name))


# Non-breeders likely detected on ARUs:
# ,"Dunlin", "Harris's Sparrow", "American Tree Sparrow", "American Pipit", "Tundra Swan", "Whimbrel",
# "Snow Bunting", "Cackling Goose", "Snow Goose", "Lapland Longspur", "Smith's Longspur"))


# Get a unique list of species (all detection)
birdnet_species_summary <- birdnet_detections %>%
  # filter(Confidence >= 0.75) %>%
  # filter(Common.name %in% c(bird_species$common_name, amphibian_species$common_name)) %>%
  group_by(Common.name) %>%
  summarize(total = n(),
            mean_confidence = mean(Confidence))

birdnet_detections_2022 <- birdnet_detections

# Save filtered version as an R object to save time
save(birdnet_detections_2022, 
     file = "Data/birdnet2022CAAFsitesDetections_2024-01-16.RData")


##### 3. 2023 Field Sites --------------------------------------------------------


# Read BirdNET predictions
birdnet_files <- list.files(path = "Data/birdNetOutput2023", pattern = ".csv", recursive = TRUE)

birdnet_file_list <- list()

# Loop through
for(i in 1:length(birdnet_files)){ # length(birdnet_files)
  birdnet_file_list[[i]] <- read.csv(file = paste0("Data/birdNetOutput2023/", birdnet_files[i])) %>%
    mutate(filename = birdnet_files[i],
           unique_id = stringr::str_split(birdnet_files[i], "/")[[1]][1],
           file_part2 = stringr::str_split(birdnet_files[i], "/")[[1]][2],
           date = as.Date(substr(file_part2, 10, 17), format = "%Y%m%d")) %>%
    # Catch cases (majority) where .csv is in subfolder for a download (the norm after first few manual sites)
    # In those cases, fix file_part2 and re-calculate date from file_part2
    mutate(file_part2 = ifelse(is.na(date),
                               stringr::str_split(birdnet_files[i], "/")[[1]][4],
                               file_part2),
           date = as.Date(substr(file_part2, 10, 17), format = "%Y%m%d"),
           file_part2 = ifelse(is.na(date),
                               stringr::str_split(birdnet_files[i], "/")[[1]][3],
                               file_part2),
           date = as.Date(substr(file_part2, 10, 17), format = "%Y%m%d"))
  
}


# Join into one dataframe
birdnet_detections <- do.call(rbind.data.frame, birdnet_file_list)

# Check species not on prairie breeding bird list or amphibian species' list
birdnet_other <- birdnet_detections %>%
  filter(!Common.name %in% c(bird_species$common_name, amphibian_species$common_name)) 

# Make a table to identify more species that may be mixed-up between North American and corresponding species on other continents
birdnet_otherspecies <- birdnet_detections %>%
  
  filter(!Common.name %in% c(bird_species$common_name, amphibian_species$common_name),
        Confidence >= 0.75) %>%
  group_by(Common.name) %>%
  summarize(total = n(),
            max_confidence = max(Confidence),
            mean_confidence = mean(Confidence)) %>%
  # Sort from most to fewest detections
  arrange(-total)


# Save all detections (even low confidence and outside species list)
save(birdnet_detections, 
     file = "Data/birdNet2023CAAFsitesDetections_all_2024-01-09.RData")

# Join together
birdnet_detections <- birdnet_detections %>%
  filter(Confidence >= 0.75) %>%
  # Fixing some common name issues
  mutate(Common.name = ifelse(Common.name == "Black-crowned Night-Heron",
                              "Black-crowned Night-heron",
                              Common.name),
         # Paraguayan Snipe = Wilson's Snipe (only snipe))
         Common.name = ifelse(Common.name == "Paraguayan Snipe",
                              "Wilson's Snipe",
                              Common.name),
         # Common Snipe = Wilson's Snipe (only snipe))
         Common.name = ifelse(Common.name == "Common Snipe",
                              "Wilson's Snipe",
                              Common.name),
         # Upland Chorus Frog = Boreal Chorus Frog
         Common.name = ifelse(Common.name == "Upland Chorus Frog",
                              "Boreal Chorus Frog",
                              Common.name),
         # Striped = Western Chorus Frog = same call as Boreal Chorus Frog
         Common.name = ifelse(Common.name == "Striped Chorus Frog",
                              "Boreal Chorus Frog",
                              Common.name),
         Common.name = ifelse(Common.name == "Brimley's Chorus Frog",
                              "Boreal Chorus Frog",
                              Common.name),
         Common.name = ifelse(Common.name == "Spotted Chorus Frog",
                              "Boreal Chorus Frog",
                              Common.name),
         Common.name = ifelse(Common.name == "Southern Chorus Frog",
                              "Boreal Chorus Frog",
                              Common.name),
         # Now "LeConte's Sparrow in species list
         # Common.name = ifelse(Common.name == "LeConte's Sparrow",
         #                      "Le Conte's Sparrow",
         #                      Common.name),
         Common.name = ifelse(Common.name == "Eastern Wood-Pewee",
                              "Eastern Wood-pewee",
                              Common.name),
         # Now "LeConte's Sparrow in species list
         # Common.name = ifelse(Common.name == "LeConte's Sparrow",
         #                      "Le Conte's Sparrow",
         #                      Common.name),
         # American Toad = Canadian Toad
         Common.name = ifelse(Common.name == "American Toad",
                              "Canadian Toad",
                              Common.name),
         # Greater Sage-Grouse = Greater Sage-grouse
         Common.name = ifelse(Common.name == "Greater Sage-Grouse",
                              "Greater Sage-grouse",
                              Common.name),
         #Eurasian Collared-Dove = Eurasian Collared-dove)
         Common.name = ifelse(Common.name == "Eurasian Collared-Dove",
                              "Eurasian Collared-dove",
                              Common.name),
         # Yellow-billed Magpie = Black-billed Magpie
         Common.name = ifelse(Common.name == "Yellow-billed Magpie",
                              "Black-billed Magpie",
                              Common.name),
         #Chihuahuan Raven = Common Raven
         Common.name = ifelse(Common.name == "Chihuahuan",
                              "Common Raven",
                              Common.name) ) %>%
  # Filter to species in the prairies
  filter(Common.name %in% c(bird_species$common_name, amphibian_species$common_name))

# Non-breeders likely detected on ARUs:
# ,"Dunlin", "Harris's Sparrow", "American Tree Sparrow", "American Pipit", "Tundra Swan", "Whimbrel",
# "Snow Bunting", "Cackling Goose", "Snow Goose", "Lapland Longspur", "Smith's Longspur"))


# Get a unique list of species (all detection)
birdnet_species_summary <- birdnet_detections %>%
  filter(Confidence >= 0.75) %>%
  # filter(Common.name %in% c(bird_species$common_name, amphibian_species$common_name)) %>%
  group_by(Common.name) %>%
  summarize(total = n(),
            mean_confidence = mean(Confidence),
            max_confidence = max(Confidence))

birdnet_detections_2023 <- birdnet_detections

# Save filtered version as an R object to save time
save(birdnet_detections_2023, 
     file = "Data/birdNet2023CAAFsitesDetections_2024-01-16.RData")


# # If doing extra cleaning, load:
# load(file = "Data/birdNet2023CAAFsitesDetections_filtered_2024-01-09.RData") # filtered data (>0.75, just prairie breeders)
# load(file = "Data/birdNet2023CAAFsitesDetections_all_2024-01-09.RData") # all data (more rows)




##### 4. Combining all observations (2022 and 2023) ------------------------------

# Load BCRC data (habitat type, location, etc.)
sites_df <- read.csv("Data/BCRC_CAAF_sites_02Jan2024.csv") %>%
  mutate(treatment = LANDUSE,
         treatment = ifelse(treatment %in% c("PASTURE", "PASTURE/GRASSLAND", "PASTURE/MIXEDWOOD"),
                            "GRASSLAND",
                            treatment))

# Rename SITE_ID (OS character issue)
names(sites_df)[2] <- "SITE_ID"

# Load 2022 data
load("Data/birdnet2022CAAFsitesDetections_2024-01-16.RData")

# Load 2023 data
load("Data/birdnet2023CAAFsitesDetections_2024-01-16.RData")

# Combine
birdnet_detections_joined <- rbind(birdnet_detections_2022,
                                   birdnet_detections_2023) %>%
  # Fix Site ID discrepancies from changes to site data and changes to site names during study
  # I compared coordinates to site names in updated site list
  rename(SITE_ID = unique_id) %>%
  mutate(# Alberta sites adding "0"
        SITE_ID = ifelse(SITE_ID == "AB1",
                          "AB01",
                          SITE_ID),
         SITE_ID = ifelse(SITE_ID == "AB2",
                          "AB02",
                          SITE_ID),
         SITE_ID = ifelse(SITE_ID == "AB3",
                          "AB03",
                          SITE_ID),
         SITE_ID = ifelse(SITE_ID == "AB4",
                          "AB04",
                          SITE_ID),
         SITE_ID = ifelse(SITE_ID == "AB5",
                          "AB05",
                          SITE_ID),
         SITE_ID = ifelse(SITE_ID == "AB6",
                          "AB06",
                          SITE_ID),
         SITE_ID = ifelse(SITE_ID == "AB7",
                          "AB07",
                          SITE_ID),
         SITE_ID = ifelse(SITE_ID == "AB8",
                          "AB08",
                          SITE_ID),
         SITE_ID = ifelse(SITE_ID == "AB9",
                          "AB09",
                          SITE_ID),
        # AB-13 to AB13
        SITE_ID = ifelse(SITE_ID == "AB-13",
                         "AB13",
                         SITE_ID),
        # MB sites adding "0"
         SITE_ID = ifelse(SITE_ID == "MB1",
                          "MB01",
                          SITE_ID),
         SITE_ID = ifelse(SITE_ID == "MB2",
                          "MB02",
                          SITE_ID),
         SITE_ID = ifelse(SITE_ID == "MB3",
                          "MB03",
                          SITE_ID),
         SITE_ID = ifelse(SITE_ID == "MB4",
                          "MB04",
                          SITE_ID),
         SITE_ID = ifelse(SITE_ID == "MB5",
                          "MB05",
                          SITE_ID),
         SITE_ID = ifelse(SITE_ID == "MB6",
                          "MB06",
                          SITE_ID),
         SITE_ID = ifelse(SITE_ID == "MB7",
                          "MB07",
                          SITE_ID),
         SITE_ID = ifelse(SITE_ID == "MB8",
                          "MB08",
                          SITE_ID),
         SITE_ID = ifelse(SITE_ID == "MB9",
                          "MB09",
                          SITE_ID),
        # SK-DB-12 = SK-A-08
        SITE_ID = ifelse(SITE_ID == "SK-DB-12",
                         "SK-A-08",
                         SITE_ID),
        # SK_C_06 in birdnet  = sk-B-08 in s123 = SK-C-06 in sites_df
        SITE_ID = ifelse(SITE_ID == "SK_C_06",
                         "SK-C-06",
                         SITE_ID),
        # SK_C_10 in birdnet  = SK-B-12 in site123 = SK-C-10 in sites_df
        SITE_ID = ifelse(SITE_ID == "SK_C_10",
                         "SK-C-10",
                         SITE_ID),
        # SK_C_11 in birdnet = SK-B-13 in site123 = SK-C-11 in sites_df
        SITE_ID = ifelse(SITE_ID == "SK_C_11",
                         "SK-C-11",
                         SITE_ID),
        # SK_C_16 in birdnet = SK-C-16 in sites_df
        SITE_ID = ifelse(SITE_ID == "SK_C_16",
                         "SK-C-16",
                         SITE_ID),
        # SK_DB_2 = SK-A-01
        SITE_ID = ifelse(SITE_ID == "SK_DB_2",
                         "SK-A-01",
                         SITE_ID),
        # # SK_DB_8 = SK-A-06
        SITE_ID = ifelse(SITE_ID == "SK_DB_8",
                         "SK-A-06",
                         SITE_ID),
         # SK_DB_Mckay = SK-A-14
        SITE_ID = ifelse(SITE_ID == "SK_DB_Mckay",
                         "SK-A-14",
                         SITE_ID),
        # SK_DB_Sagen = SK-A-12
        SITE_ID = ifelse(SITE_ID == "SK_DB_Sagen",
                         "SK-A-12",
                         SITE_ID),
        ) %>%
  mutate(SITE_ID = ifelse(SITE_ID == "MB4_original",
                          "MB04",
                          SITE_ID)) %>%
  left_join(.,
          sites_df %>%
            dplyr::select(SITE_ID, PROVINCE, treatment, Latitude.Y, Longtitude.X) %>%
            rename(Longitude.X = Longtitude.X),
          by ="SITE_ID") %>%
  dplyr::select(-Scientific.name) %>%
  left_join(.,
            acad %>%
              dplyr::rename(Common.name = Common.Name,
                            Scientific.name = Scientific.Name,
                            group = Group) %>%
              dplyr::select(Common.name, Scientific.name, group, habitat) %>%
              rbind(.,
                    amphibian_species %>%
                      rename(Common.name = common_name) %>%
                      mutate(Scientific.name = paste0(genus, " ",species),
                             group = "anuran",
                             habitat = "Wetlands") %>%
                      dplyr::select(Common.name, Scientific.name, group, habitat)),
            by = "Common.name") %>%
  # mutate(group = ifelse(is.na(group),
  #                       "anuran",
  #                       group),
  #        habitat = ifelse(is.na(habitat),
  #                       "Wetlands",
  #                       habitat),
  #        habitat = ifelse(habitat == "Forest",
  #                         "Forests",
  #                         habitat)) %>%
  filter(SITE_ID != "MB4_error") %>%
  dplyr::select(Start..s.:Common.name, Scientific.name, group:habitat, Confidence:Longitude.X) %>%
  # One observation (and not in ACAD)
  filter(Common.name != "Eastern Whip-poor-will")


summary(birdnet_detections_joined)

unique(birdnet_detections_joined$SITE_ID)
unique(birdnet_detections_joined$PROVINCE)
unique(birdnet_detections_joined$group)

# load("Data/biodiversityData_2023-06-20.RData") # Comparing to the old one

# For species that we manually changed Common.name, need to update Scientific.name


# Save. 1060083 rows
save(birdnet_detections_joined,
     file = "Data/biodiversityData_BCRC_CAAF_2024-01-16.RData")


##### 5. Making site table with coordinates --------------------------------------

caafBiodiversitySites <- birdnet_detections_joined %>%
  dplyr::select(SITE_ID, PROVINCE, treatment, Latitude.Y, Longitude.X) %>%
  distinct(.)

# Save
write.csv(caafBiodiversitySites,
          "Data/caafBiodiversitySites.csv")


##### 6. Updating BirdNetData with verification results (final data sets to analyze) --------------------------

# Note: this uses manual listening (written in 01.1, then verified by NH and JEP)

# Load previous .RData file
load("Data/biodiversityData_BCRC_CAAF_2024-01-16.RData")

# Birds with wetlands as a "secondary breeding habitat"
secondaryWetlandBirds <- c("Red-winged Blackbird", "Northern Harrier", "Sedge Wren", "Purple Martin", "Yellow Warbler", "Common Yellowthroat", "Northern Rough-winged Swallow", "Killdeer")

# Need to add filter for sites that are not included in rest of the manuscript
caafSites_SW <- read.csv("Data/site_df_SW.csv") # sites from SW

# Filter detections table based on May 28-July31 window and human verification of listening.
# manualListening_caaf_20250404_NH_JP.csv has all verified detections
birdnet_detections_joined_updated <- birdnet_detections_joined %>%
  # filter() %>%
  mutate(year = lubridate::year(date),
         month = lubridate::month(date),
        day = lubridate::yday(date)) %>%
  # Only observations >= May 28
  filter(day >= lubridate::yday("2022-05-28"),
         # Only sites Sam Woodman includes in manuscript
         SITE_ID %in% caafSites_SW$site_id,
         # Remove species where all vocalizations were confirmed false positive
         !Common.name == "Bonaparte's Gull",
         !Common.name == "Cattle Egret",
         # Hooded Merganser; remove from sites with false positives (confirmed with 2 observers listening)
         !(Common.name == "Hooded Merganser" & SITE_ID == "AB10"),
         !Common.name == "Northern Harrier",
         !Common.name == "Northern Rough-winged Swallow",
         !Common.name == "Palm Warbler",
         # American Bittern; remove from sites with false positives (confirmed with 2 observers listening)
         !(Common.name == "American Bittern" & SITE_ID == "SK-C-10"),
         !(Common.name == "American Bittern" & SITE_ID == "AB03"),
         !(Common.name == "American Bittern" & SITE_ID == "AB16"),
         # All bald eagle sightings were confirmed false positive
         !Common.name == "Bald Eagle",
         !Common.name == "Belted Kingfisher",
         !(Common.name == "Black-crowned Night-heron"),
         # Common Merganser never heard >1 at a site
         !Common.name == "Common Merganser",
         # Common Yellowthroat; remove from sites with false positives (confirmed with 2 observers listening)
         !(Common.name == "Common Yellowthroat" & SITE_ID == "MB15"),
         # Eared Grebe; remove from sites with false positives (confirmed with 2 observers listening)
         !(Common.name == "Eared Grebe" & SITE_ID == "SK01"),
         !(Common.name == "Eared Grebe" & SITE_ID == "SK03"),
         # LeConte's Sparrow; remove from sites with false positives (confirmed with 2 observers listening)
         !(Common.name == "LeConte's Sparrow" & SITE_ID == "AB01"),
         !(Common.name == "LeConte's Sparrow" & SITE_ID == "AB-DB-07"),
         !(Common.name == "LeConte's Sparrow" & SITE_ID == "MB06"),
         !(Common.name == "LeConte's Sparrow" & SITE_ID == "MB08"),
         !(Common.name == "LeConte's Sparrow" & SITE_ID == "SK15"),
         !(Common.name == "LeConte's Sparrow" & SITE_ID == "SK16"),
         # Marbled Godwit. remove from sites with false positives (confirmed with 2 observers listening)
         !(Common.name == "Marbled Godwit" & SITE_ID == "SK05"),
         !(Common.name == "Marbled Godwit" & SITE_ID == "SK11"),
         !(Common.name == "Marbled Godwit" & SITE_ID == "SK12"),
         # Marsh Wren; remove from sites with false positives (confirmed with 2 observers listening)
         !(Common.name == "Marsh Wren" & SITE_ID == "SK-A-14"),
         # Northern Shoveler; remove from sites with false positives (confirmed with 2 observers listening)
         !(Common.name == "Northern Shoveler" & SITE_ID == "AB09"),
         # Pied-billed Grebe; remove from sites with false positives (confirmed with 2 observers listening)
         !(Common.name == "Pied-billed Grebe" & SITE_ID == "AB10"),
         # Ruddy Duck; remove from sites with false positives (confirmed with 2 observers listening)
         !(Common.name == "Ruddy Duck" & SITE_ID == "MB01"),
         !(Common.name == "Ruddy Duck" & SITE_ID == "MB04"),
         # Rusty Blackbird; remove from sites with false positives (confirmed with 2 observers listening); no confirmed songs
         !Common.name == "Rusty Blackbird",
         # Sandhill Crane; remove from sites with false positives (confirmed with 2 observers listening)
         !(Common.name == "Sandhill Crane" & SITE_ID == "SK16"),
         # Solitary Sandpiper; remove from sites with false positives (confirmed with 2 observers listening)
         !(Common.name == "Solitary Sandpiper" & SITE_ID == "AB03"),
         # Trumpeter Swan; remove from sites with false positives (confirmed with 2 observers listening); all detections false positive
         !Common.name == "Trumpeter Swan",
         # Virginia Rail; remove from sites with false positives (confirmed with 2 observers listening)
         !(Common.name == "Virginia Rail" & SITE_ID == "MB10"),
         # White-faced Ibis; remove from sites with false positives (confirmed with 2 observers listening)
         !(Common.name == "White-faced Ibis" & SITE_ID == "AB16"),
         !(Common.name == "White-faced Ibis" & SITE_ID == "SK-A-08"),
         # Wood Duck; remove from sites with false positives (confirmed with 2 observers listening)
         !(Common.name == "Wood Duck" & SITE_ID == "AB01"),
         # Wilson's Phalarope; remove from sites with false positives (confirmed with 2 observers listening)
         !(Common.name == "Wilson's Phalarope" & SITE_ID == "AB02"),
         !(Common.name == "Wilson's Phalarope" & SITE_ID == "AB05"),
         !(Common.name == "Wilson's Phalarope" & SITE_ID == "AB10"),
         !(Common.name == "Wilson's Phalarope" & SITE_ID == "AB11"),
         !(Common.name == "Wilson's Phalarope" & SITE_ID == "AB15"),
         !(Common.name == "Wilson's Phalarope" & SITE_ID == "MB05"),
         !(Common.name == "Wilson's Phalarope" & SITE_ID == "MB10"),
         !(Common.name == "Wilson's Phalarope" & SITE_ID == "MB15"),
         !(Common.name == "Wilson's Phalarope" & SITE_ID == "SK03"),
         !(Common.name == "Wilson's Phalarope" & SITE_ID == "SK11"),
         !(Common.name == "Wilson's Phalarope" & SITE_ID == "SK14"),
         !(Common.name == "Wilson's Phalarope" & SITE_ID == "SK-A-08"),
         # Wilson's Snipe
         # Some false positives during storms; many detections on other days, so don't remove from AB02, AB03, AB05, AB07, MB04, SK02, SK05, SK12
         !(Common.name == "Wilson's Snipe" & SITE_ID == "SK-A-08"), # 3 detections, confirmed false positive
         # Yellow Rail; all detections confirmed false positive
         !Common.name == "Yellow Rail",
         # Yellow-headed Blackbird; remove from sites with false positives (confirmed with 2 observers listening)
         !(Common.name == "Yellow-headed Blackbird" & SITE_ID == "MB01"),
         !(Common.name == "Yellow-headed Blackbird" & SITE_ID == "MB04"),
         !(Common.name == "Yellow-headed Blackbird" & SITE_ID == "MB08"),
         !(Common.name == "Yellow-headed Blackbird" & SITE_ID == "SK15"),
         !(Common.name == "Yellow-headed Blackbird" & SITE_ID == "SK16"),
         # Least Bittern; remove (maybe one detection is real, but not repeated)
         !Common.name == "Least Bittern",
         # Just include wetland species
         habitat == "Wetlands" & !group == "anuran" & !Common.name == "Common Loon" | Common.name %in% secondaryWetlandBirds)

# Find species detected just on multiple days and filter detections by that combination
speciesSitesTable <- birdnet_detections_joined_updated %>%
  group_by(Common.name, Scientific.name, SITE_ID) %>%
  summarize(num_days = length(unique(date))) %>%
  ungroup() %>%
  filter(num_days >= 2) %>%
  mutate(Common.name.SITE_ID = paste0(Common.name, "-", SITE_ID),
         province = substr(SITE_ID, 1, 2))

# Create final birdNet detections to save
wetlandBirdDetections <- birdnet_detections_joined_updated %>%
  ungroup() %>%
  mutate(Common.name.SITE_ID = paste0(Common.name, "-", SITE_ID)) %>%
  filter(Common.name.SITE_ID %in% speciesSitesTable$Common.name.SITE_ID)

# Save file as .RData (1060083 rows)
save(wetlandBirdDetections,
     file = "Data/wetlandBirdDetections_CAAF_2025-04-15.RData")

##### END