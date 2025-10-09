## ---------------------------
##
## Script name: 01.2_wetlandBenefitsTradeoffs_manualListeningPrep.R 
##
## Purpose of script: prepare recordings for manual listening
## Objective: input clips, then write sections of wav files that are much shorter and have easier to sort filenames (by species)
##
## Author: Ash Pidwerbesky
##
## Date Created: 2024-10-03
## 
## Email: a_pidwerbesky@ducks.ca
##
## ---------------------------

library(dplyr)
library(monitoR)
library(tidyverse)
library(readr)
library(tidyr)
library(lubridate)
library(stringr)

# Creating manual listening verification spreadsheet ----------------------
#
# # Objective: for all species-site combinations (species detected >= 2 days) select 3 detections/recordings with highest confidence level
# # to review with manual listening.
# # This iteration based on 2022 data. 59 sites (1 site = 4470 excluded because of incomplete data)
#
#
# # Load birdnet detection data
load("Data/biodiversityData_BCRC_CAAF_2024-01-16.RData")

# Exclude obvious errors (based on species in NACC list outside of Prairie Canada during breeding or migration)
bad_species <- c("Brown Shrike", "Chihuahuan Raven", "Common Greenshank", "Common Scoter", "Eurasian Woodcock",
                 "Gray Heron", "Hawaiian Petrel", "Laughing Falcon", "Lineated Woodpecker", "Song Thrush",
                 "Wedge-tailed Shearwater", "Barn Owl", "Barnacle Goose", "Bell's Sparrow", "Brambling",
                 "Eurasian Coot","Eurasian Hobby","Flammulated Owl", "Florida Scrub-Jay","Great Gray Owl",
                 "Greater Prairie-Chicken", "Henslow's Sparrow", "Lesson's Motmot", "Little Bittern","Pinyon Jay",
                 "Red-throated Pipit", "Spotted Owl", "Tree Pipit", "Bulwer's Petrel","Common Ringed Plover",
                 "Eurasian Bullfinch", "Eurasian Wigeon", "European Robin", "Gray-crowned Rosy-Finch", "Lesser Violetear",
                 "Northern Beardless-Tyrannulet", "Pacific Golden-Plover", "Redwing", "Rook", "Whooper Swan",
                 "Akohekohe", "Ash-throated Flycatcher", "Buff-throated Foliage-gleaner", "Clark's Nutcracker", "Great-tailed Grackle",
                 "Juniper Titmouse", "Mangrove Vireo", "Olive-backed Euphonia", "Orange-fronted Parakeet", "Tricolored Blackbird", "Varied Thrush",
                 "Wood Warbler", "Abert's Towhee", "Bachman's Sparrow", "Buff-breasted Flycatcher", "Golden-cheeked Warbler",
                 "Lined Seedeater", "Lucy's Warbler", "Maui Alauahio", "Red-faced Warbler", "Rufous-winged Sparrow",
                 "Rusty Sparrow", "Williamson's Sapsucker", "Yellowish Flycatcher", "Arctic Warbler", "Puaiohi",
                 "Red-legged Thrush", "Willow Ptarmigan", "Yellow-throated Warbler", "Crescent-chested Warbler", "Olive Warbler",
                 "Ruddy Foliage-gleaner","Spotted Redshank", "Striped Owl","Black-chinned Sparrow", "Cassin's Sparrow",
                 "Gilded Flicker", "Grassland Yellow-Finch", "Lesser Nighthawk", "Scaled Antpitta","Seaside Sparrow",
                 "Smooth-billed Ani", "Sooty-capped Chlorospingus", "Chestnut-capped Brushfinch", "Ruddy Shelduck", "Scott's Oriole",
                 "Shiny Cowbird", "Streaked Flycatcher","Boat-tailed Grackle", "Common Gallinule",  "Striped Cuckoo",
                 "Alpine Swift", "Stripe-headed Sparrow", "Northern Wheatear", "Buff-necked Ibis", "Ferruginous Pygmy-Owl",
                 "Green Parakeet", "Laysan Albatross", "Steller's Jay", "White-winged Tern", "Pink-footed Goose", "Virginia's Warbler",
                 "Red Knot", "Yellow-backed Oriole", "Yellow-throated Toucan", "Fish Crow","Long-billed Gnatwren",
                 "Long-tailed Manakin", "Bran-colored Flycatcher", "Hooded Oriole",  "Rustic Bunting", "Summer Tanager",
                 "Broad-tailed Hummingbird", "Crested Caracara", "Prothonotary Warbler", "Wood Sandpiper","Wood Thrush",
                 "Sharp-tailed Streamcreeper", "Little Tern", "Grass Wren", "Gray-tailed Tattler", "Lawrence's Goldfinch",
                 "Bridled Titmouse", "Cattle Tyrant", "King Rail", "Black-faced Grassquit", "Spotted Crake", "Violet-green Swallow",
                 "Gray Kingbird", "Narcissus Flycatcher", "Northern Bobwhite", "Swainson's Warbler", "White Wagtail",
                 "Lesser Whitethroat", "Northern Jacana", "Tropical Screech-Owl", "Common Waxbill", "Buff-rumped Warbler",
                 "Carib Grackle", "Fork-tailed Flycatcher", "Crowned Slaty Flycatcher","Band-backed Wren", "Garganey",
                 "Bright-rumped Attila", "Hawaii Amakihi", "White's Thrush", "Carolina Chickadee",
                 "Golden-fronted Greenlet", "Louisiana Waterthrush", "Whiskered Screech-Owl", "Canyon Wren", "Little Bunting",
                 "Botteri's Sparrow", "Hawaiian Goose", "Black Phoebe", "Kalij Pheasant", "Swallow-tailed Kite",
                 "Elf Owl", "Phainopepla", "Woodhouse's Scrub-Jay", "Red-crested Cardinal", "Ruddy Quail-Dove",
                 "Common Gull", "European Golden-Plover", "Gray Flycatcher", "Mottled Owl", "Worm-eating Warbler",
                 "Clapper Rail", "Japanese Quail", "Sage Thrasher", "Bushy-crested Jay", "Olive Sparrow", "Taiga Flycatcher",
                 "Common Ground Dove", "Red-billed Pigeon", "Crested Guan","Snowy Plover", "Keel-billed Toucan", "Gray Wagtail",
                 "Reed Bunting", "Hawaiian Coot", "White-faced Whistling-Duck", "Barred Becard", "Black-vented Oriole", "Wilson's Plover",
                 "Rufous-capped Warbler", "Black-tailed Gnatcatcher", "Golden-winged Warbler", "Bicknell's Thrush", "Green Jay",
                 "Happy Wren", "Orange-chinned Parakeet", "Common Eider", "Olive-backed Pipit", "Red-cockaded Woodpecker", "White-naped Brushfinch",
                 "White-throated Swift", "Bewick's Wren", "Great Spotted Woodpecker", "Hepatic Tanager" , "Lewis's Woodpecker",
                 "Hammond's Flycatcher", "Golden-fronted Woodpecker", "Black-winged Stilt", "Green Heron" , "Least Grebe",
                 "Rivoli's Hummingbird", "White-bellied Wren", "Canyon Towhee", "Iiwi", "Olive-throated Parakeet", "Rufous-breasted Antthrush",
                 "Common Sandpiper", "Antillean Nighthawk", "Black-tailed Godwit", "Kirtland's Warbler", "Bronzed Cowbird", "Dusky Thrush",
                 "White-throated Magpie-Jay"  , "Russet-crowned Motmot" , "Great Cormorant", "Northern Gannet",  "White-tailed Kite",
                 "Akiapolaau", "Slaty-backed Nightingale-Thrush", "Nuttall's Woodpecker", "Village Weaver", "Crested Owl", "Short-tailed Nighthawk",
                 "Short-tailed Shearwater", "Common Shelduck", "California Quail", "Hawaii Creeper", "Common Crane", "Dusky Flycatcher",
                 "Green Sandpiper", "Razorbill", "Yellow-breasted Chat", "LeConte's Thrasher", "Rock Wren", "Common Swift",
                 "Spot-crowned Woodcreeper", "Red-backed Shrike", "Sunbittern", "Hutton's Vireo", "Common Tody-Flycatcher", "Eurasian Curlew",
                 "Whiskered Tern", "Siberian Blue Robin", "Northern Lapwing",
                 # During cleaning
                 "Eurasian Kestrel", "	Brant", "Burrowing Owl", "California Thrasher", "Laughing Gull",
                 "Northern Pygmy-Owl", "Greater Sage-Grouse")


birdnet_detections_joined <- birdnet_detections_joined %>% 
  mutate(dateMMDD = format(as.Date(date), "%m-%d")) %>%
  filter(dateMMDD > "05-27") 

# Number of days a species is detected at each site
birdnet_detection_days <- birdnet_detections_joined %>%
  filter(!Common.name %in% bad_species #,
         # Confidence >= 0.75,
         #date>= as.Date("2022-06-01")
  ) %>%
  group_by(SITE_ID, PROVINCE, Common.name, group, habitat) %>%
  # Change to be number of unique days a species is detected.
  summarize(numberOfDaysDetected = length(unique(date))) %>%
  # summarize(numberOfDetections = n()) %>%
  #summarize(richness_bird = length(unique(Common.name))) %>%
  ungroup(.) %>%
  filter(numberOfDaysDetected >= 2,
         # For now, remove these uncertain species (haven't found a positive detection yet)
         # Common.name != "Least Bittern"#,
         #Common.name != "Yellow Rail"
  )


# Use a loop based on bird_detections_days (for each row, select 3 detections with highest confidence)
# Put in a list, then unlist at the end
# Takes a long time when doing it for full data set.

manualListeningList <- list()

for(i in 1:nrow(birdnet_detection_days)) {
  # Set site and common name
  site_i <- birdnet_detection_days$SITE_ID[i]
  common_name_i <- birdnet_detection_days$Common.name[i]
  # Filter
  manualListeningList[[i]] <- birdnet_detections_joined %>%
    filter(SITE_ID == site_i,
           Common.name == common_name_i) %>%
    dplyr::select(`Start..s.`:date, group, habitat) %>%
    mutate(human_listen_verify = NA) %>%
    arrange(-Confidence) %>%
    # Take 10 most confident 3 second detections
    .[1:10,] %>%
    # Take 3 unique files (to have more independence if a false positive)
    group_by(filename) %>%
    slice_sample(., n = 1) %>%
    .[1:3,]
}

# Then combine into one dataframe
manualListening <- do.call(rbind.data.frame, manualListeningList) %>%
  as.data.frame(.) %>%
  dplyr::filter(!is.na(Common.name))

# Species-Group-Habitat dataframe
speciesGroupHabitat <- birdnet_detection_days %>%
  dplyr::select(Common.name, group, habitat) %>%
  group_by(Common.name, group, habitat) %>%
  summarize(nsites = n()) %>%
  filter(!Common.name %in% bad_species)



manualListeningFull <- manualListening %>%
  mutate(species_filename_start = paste0(Common.name, filename, Start..s.)) %>%
  # separate(file_part2, into = paste0('file_part2', 1:5), sep = '[_.]', fill = "right") %>%
  # rename(time = file_part23) %>%
  # mutate(date_time = paste0(date, " ",time)) %>%
  # mutate(date_time = ymd_hms(date_time)) %>%
  # mutate(file_part2 = paste0(file_part21, "/", file_part22,"/", time)) %>%
  # select(-file_part21, -file_part22, -file_part23, -file_part24, -file_part25, -date) %>%
  select(-date) %>%
  # mutate(startTime = seconds_to_period(Start..s.)) %>%
  filter(!is.na(Start..s.)) %>%
  # mutate(date_time = date_time + Start..s.) %>%
  dplyr::select(-species_filename_start) %>%
  mutate(manualListeningFileName = paste0(Common.name,
                                          "_",
                                          # startTime,
                                          # "_",
                                          str_replace(file_part2,
                                                      pattern = ".BirdNET.results.csv",
                                                      replacement = ".wav")))


secondaryWetlandBirds <- c("Red-winged Blackbird", "Northern Harrier", "Sedge Wren", "Purple Martin", "Yellow Warbler", "Common Yellowthroat", "Northern Rough-winged Swallow", "Killdeer")

wetlandBirdRichness_caaf <- manualListeningFull %>% 
  # filter to remove upland birds and amphibians, include species that use wetlands as secondary breeding habitat, exclude Common Loons
  filter(habitat == "Wetlands" & !group == "anuran" & !Common.name == "Common Loon" | Common.name %in% secondaryWetlandBirds)  
  
caafSites_SW <- read.csv("Data/site_df_SW.csv") #sites from SW

SiteList <- caafSites_SW %>% 
  filter(!is.na(wet_bird_richness)) %>% 
  filter(!site_id == "AB-B-06") %>% 
  rename(SITE_ID = site_id) 

SiteList <- SiteList$SITE_ID 


wetlandBirdRichness_caaf <- wetlandBirdRichness_caaf %>% 
  filter(SITE_ID %in% SiteList)

write.csv(wetlandBirdRichness_caaf, "Data/manualListening_caaf_2024-10-09.csv")

wetlandBirdRichness_caaf <- read.csv("Data/manualListening_caaf_2024-10-09.csv")


# 2023 sites ###################################################################

AB2023_sites<- list.dirs("D:/2023/ARU data/AB", full.names = F, recursive = F)
MB2023_sites<- list.dirs("D:/2023/ARU data/MB", full.names = F, recursive = F)
SK2023_sites<- list.dirs("D:/2023/ARU data/SK", full.names = F, recursive = F)

SK2023_sites <- gsub("_", "-", SK2023_sites)


AB2023_sites_filtered <- AB2023_sites[AB2023_sites %in% wetlandBirdRichness_caaf$SITE_ID] 
MB2023_sites_filtered <- MB2023_sites[MB2023_sites %in% wetlandBirdRichness_caaf$SITE_ID] 
SK2023_sites_filtered <- SK2023_sites[SK2023_sites %in% wetlandBirdRichness_caaf$SITE_ID] 

sites2023 <- c(AB2023_sites_filtered, SK2023_sites_filtered, "SK-A-06", "SK-A-08", "SK-A-12", "SK-A-14", "AB13")

wetlandBirdRichness_caaf2023 <- wetlandBirdRichness_caaf %>%
  filter(SITE_ID %in% sites2023) %>%
  mutate(filename2 = case_when( 
    str_starts(SITE_ID, "AB") ~ paste0("AB/", filename), 
    str_starts(SITE_ID, "SK") ~ paste0("SK/", filename),
    TRUE ~ NA_character_)) %>% 
  filter(!is.na(filename2))


system.time(
  for(i in 1:nrow(wetlandBirdRichness_caaf2023)){ # manualListeningFull
    
    file_i_og <- paste0("D:/2023/ARU data/", wetlandBirdRichness_caaf2023$filename2[i])
    file_i_wav <- str_replace(file_i_og, pattern = ".BirdNET.results.csv", replacement = ".wav")
    
    # Read Wave
    newSection_i <-readWave(file_i_wav, 
                            from = wetlandBirdRichness_caaf2023$Start..s.[i]-3, 
                            to = wetlandBirdRichness_caaf2023$End..s.[i]+3, 
                            units = "seconds")
    
    # view spectrogram data
    # viewSpec(newSection_i)
    
    # Write new spectogram
    writeWave(object = newSection_i, 
              filename = paste0("D:/2023/ARU data/soundClipsForManualListening_caaf/", 
                                wetlandBirdRichness_caaf2023$Common.name[i], 
                                "_",
                                # manualListeningTest$startTime[i],
                                # "_",
                                str_replace(wetlandBirdRichness_caaf2023$file_part2[i], 
                                            pattern = ".BirdNET.results.csv", 
                                            replacement = ".wav")
              ),
              extensible = FALSE)
    
  }
)

# 2022 sites ###################################################################

AB2022_sites <- list.dirs("F:/Evaluating Wetland Restorations/Data/Alberta", full.names = F, recursive = F) 
MB2022_sites <- list.dirs("F:/Evaluating Wetland Restorations/Data/Manitoba", full.names = F, recursive = F) 
SK2022_sites <- list.dirs("F:/Evaluating Wetland Restorations/Data/SASKATCHEWAN", full.names = F, recursive = F) 

AB2022_sites <- ifelse(grepl("^AB[0-9]$", AB2022_sites), 
                           gsub("AB([0-9])$", "AB0\\1", AB2022_sites), 
                           AB2022_sites)


MB2022_sites <- ifelse(grepl("^MB[0-9]$", MB2022_sites), 
                           gsub("MB([0-9])$", "MB0\\1", MB2022_sites), 
                           MB2022_sites)


AB2022_sites_caaf <- AB2022_sites[AB2022_sites %in% wetlandBirdRichness_caaf$SITE_ID] 
MB2022_sites_caaf <- MB2022_sites[MB2022_sites %in% wetlandBirdRichness_caaf$SITE_ID] 
SK2022_sites_caaf <- SK2022_sites[SK2022_sites %in% wetlandBirdRichness_caaf$SITE_ID]



sites2022_caaf  <- c(AB2022_sites_caaf, MB2022_sites_caaf, SK2022_sites_caaf, "MB04" )

wetlandBirdRichness_caaf2022 <- wetlandBirdRichness_caaf %>%
  filter(SITE_ID %in% sites2022_caaf) %>%
  mutate(filename2 = case_when(
    str_starts(SITE_ID, "AB") ~ paste0("Alberta/", filename), 
    str_starts(SITE_ID, "MB") ~ paste0("Manitoba/", filename),
    str_starts(SITE_ID, "SB") ~ paste0("SASKATCHEWAN/", filename),
    TRUE ~ NA_character_)) %>% 
  filter(!is.na(filename2))


system.time(
  for(i in 1:nrow(wetlandBirdRichness_caaf2022)){ # manualListeningFull
    
    file_i_og <- paste0("F:/Evaluating Wetland Restorations/Data/", wetlandBirdRichness_caaf2022$filename2[i])
    file_i_wav <- str_replace(file_i_og, pattern = ".BirdNET.results.csv", replacement = ".wav")
    
    # Read Wave
    newSection_i <-readWave(file_i_wav, 
                            from = wetlandBirdRichness_caaf2022$Start..s.[i]-3, 
                            to = wetlandBirdRichness_caaf2022$End..s.[i]+3, 
                            units = "seconds")
    
    # view spectrogram data
    # viewSpec(newSection_i)
    
    # Write new spectogram
    writeWave(object = newSection_i, 
              filename = paste0("D:/2023/ARU data/soundClipsForManualListening_caaf/", 
                                wetlandBirdRichness_caaf2022$Common.name[i], 
                                "_",
                                # manualListeningTest$startTime[i],
                                # "_",
                                str_replace(wetlandBirdRichness_caaf2022$file_part2[i], 
                                            pattern = ".BirdNET.results.csv", 
                                            replacement = ".wav")
              ),
              extensible = FALSE)
    
  }
)

wavFiles <- list.files("D:/2023/ARU data/soundClipsForManualListening_caaf", full.names = F, recursive = F)

extra <- wetlandBirdRichness_caaf %>% 
  filter(!manualListeningFileName %in% wavFiles) 


#graveyard 
# ################################################################################
# # There are likely missing sites (2022: 29, 2023: 11, should be 57) 
# # Guessing that there is name differences (ie. MB1 vs MB01)
# 
# manualListeningSitesDone <- c(sites2022, sites2023)
# missing <- SiteList %>% filter(!SITE_ID %in% manualListeningSitesDone)
# missing$SITE_ID
# 
# AB2022_sites_new <- ifelse(grepl("^AB[0-9]$", AB2022_sites), 
#                           gsub("AB([0-9])$", "AB0\\1", AB2022_sites), 
#                           AB2022_sites)
# AB2022_sites_new
# 
# MB2022_sites_new <- ifelse(grepl("^MB[0-9]$", MB2022_sites), 
#                            gsub("MB([0-9])$", "MB0\\1", MB2022_sites), 
#                            MB2022_sites)
# MB2022_sites_new
# 
# sites_next <- c(AB2022_sites_new, MB2022_sites_new) 
# 
# sites_next <- sites_next[sites_next %in% missing$SITE_ID]
# sites_next
# 
# #still missing "SK-A-06" "SK-A-08" "SK-A-12" "SK-A-14"
# 
# wetlandBirdRichness_caafnext <- wetlandBirdRichness_caaf %>%
#   filter(SITE_ID %in% sites_next) %>%
#   mutate(filename2 = case_when( 
#     SITE_ID %in% AB2022_sites_new ~ paste0("Alberta/", filename), 
#     SITE_ID %in% MB2022_sites_new ~ paste0("Manitoba/", filename),
#     #SITE_ID %in% SK2022_sites ~ paste0("SASKATCHEWAN/", filename),
#     TRUE ~ NA_character_)) %>% 
#   filter(!is.na(filename2))
# 
# 
# system.time(
#   for(i in 1:nrow(wetlandBirdRichness_caafnext)){ # manualListeningFull
#     
#     file_i_og <- paste0("F:/Evaluating Wetland Restorations/Data/", wetlandBirdRichness_caafnext$filename2[i])
#     file_i_wav <- str_replace(file_i_og, pattern = ".BirdNET.results.csv", replacement = ".wav")
#     
#     # Read Wave
#     newSection_i <-readWave(file_i_wav, 
#                             from = wetlandBirdRichness_caafnext$Start..s.[i]-3, 
#                             to = wetlandBirdRichness_caafnext$End..s.[i]+3, 
#                             units = "seconds")
#     
#     # view spectrogram data
#     # viewSpec(newSection_i)
#     
#     # Write new spectogram
#     writeWave(object = newSection_i, 
#               filename = paste0("D:/2023/ARU data/soundClipsForManualListening_caaf/", 
#                                 wetlandBirdRichness_caafnext$Common.name[i], 
#                                 "_",
#                                 # manualListeningTest$startTime[i],
#                                 # "_",
#                                 str_replace(wetlandBirdRichness_caafnext$file_part2[i], 
#                                             pattern = ".BirdNET.results.csv", 
#                                             replacement = ".wav")
#               ),
#               extensible = FALSE)
#     
#   }
# )
# 
# # ################################################################################
# # # Found "SK-A-06" "SK-A-08" "SK-A-12" "SK-A-14" 
# # # SK-A-08 = SK-DB-12
# # # SK-A-12 = SK-DB-Sagen 
# # # SK-A-14 = SK-DB-McKay
# # # SK-A-06 = SK-DB-08
# # 
# # SK2023_sites<- list.dirs("D:/2023/ARU data/SK", full.names = F, recursive = F)
# # 
# # SK2023_sites <- gsub("_", "-", SK2023_sites)
# # 
# # 
# # missingSKSites <- c("SK-A-06", "SK-A-08", "SK-A-12", "SK-A-14")
# # 
# # 
# # wetlandBirdRichness_caaf_SK <- wetlandBirdRichness_caaf %>%
# #   filter(SITE_ID %in% missingSKSites) %>%
# #   mutate(filename2 = paste0("SK/", filename)) %>% 
# #   filter(!is.na(filename2))
# # 
# # 
# # system.time(
# #   for(i in 1:nrow(wetlandBirdRichness_caaf_SK)){ # manualListeningFull
# #     
# #     file_i_og <- paste0("D:/2023/ARU data/", wetlandBirdRichness_caaf_SK$filename2[i])
# #     file_i_wav <- str_replace(file_i_og, pattern = ".BirdNET.results.csv", replacement = ".wav")
# #     
# #     # Read Wave
# #     newSection_i <-readWave(file_i_wav, 
# #                             from = wetlandBirdRichness_caaf_SK$Start..s.[i]-3, 
# #                             to = wetlandBirdRichness_caaf_SK$End..s.[i]+3, 
# #                             units = "seconds")
# #     
# #     # view spectrogram data
# #     # viewSpec(newSection_i)
# #     
# #     # Write new spectogram
# #     writeWave(object = newSection_i, 
# #               filename = paste0("D:/2023/ARU data/soundClipsForManualListening_caaf/", 
# #                                 wetlandBirdRichness_caaf_SK$Common.name[i], 
# #                                 "_",
# #                                 # manualListeningTest$startTime[i],
# #                                 # "_",
# #                                 str_replace(wetlandBirdRichness_caaf_SK$file_part2[i], 
# #                                             pattern = ".BirdNET.results.csv", 
# #                                             replacement = ".wav")
# #               ),
# #               extensible = FALSE)
# #     
# #   }
# # )
# #   
# ################################################################################
# 
# wavFiles <- list.files("D:/2023/ARU data/soundClipsForManualListening_caaf", full.names = F, recursive = F)
# 
# extra <- wetlandBirdRichness_caaf %>% 
#   filter(!manualListeningFileName %in% wavFiles) 
# 
# wetlandBirdRichness_filtered <- wetlandBirdRichness_caaf %>%
#   filter(SITE_ID == "AB13") %>%
#   mutate(filename2 = paste0("AB/", filename)) %>%
#   filter(!is.na(filename2))
# 
# 
# system.time(
#   for(i in 1:nrow(wetlandBirdRichness_filtered)){ # manualListeningFull
# 
#     file_i_og <- paste0("D:/2023/ARU data/", wetlandBirdRichness_filtered$filename2[i])
#     file_i_wav <- str_replace(file_i_og, pattern = ".BirdNET.results.csv", replacement = ".wav")
# 
#     # Read Wave
#     newSection_i <-readWave(file_i_wav,
#                             from = wetlandBirdRichness_filtered$Start..s.[i]-3,
#                             to = wetlandBirdRichness_filtered$End..s.[i]+3,
#                             units = "seconds")
# 
#     # view spectrogram data
#     # viewSpec(newSection_i)
# 
#     # Write new spectogram
#     writeWave(object = newSection_i,
#               filename = paste0("D:/2023/ARU data/soundClipsForManualListening_caaf/",
#                                 wetlandBirdRichness_filtered$Common.name[i],
#                                 "_",
#                                 # manualListeningTest$startTime[i],
#                                 # "_",
#                                 str_replace(wetlandBirdRichness_filtered$file_part2[i],
#                                             pattern = ".BirdNET.results.csv",
#                                             replacement = ".wav")
#               ),
#               extensible = FALSE)
# 
#   }
# )
# 
# 
# wetlandBirdRichness_caaf_filtered <- wetlandBirdRichness_caaf %>%
#   filter(SITE_ID == "MB04") %>%
#   mutate(filename2 = paste0("Manitoba/", filename)) 
# 
# 
# system.time(
#   for(i in 1:nrow(wetlandBirdRichness_caaf_filtered)){ # manualListeningFull
#     
#     file_i_og <- paste0("F:/Evaluating Wetland Restorations/Data/", wetlandBirdRichness_caaf_filtered$filename2[i])
#     file_i_wav <- str_replace(file_i_og, pattern = ".BirdNET.results.csv", replacement = ".wav")
#     
#     # Read Wave
#     newSection_i <-readWave(file_i_wav, 
#                             from = wetlandBirdRichness_caaf_filtered$Start..s.[i]-3, 
#                             to = wetlandBirdRichness_caaf_filtered$End..s.[i]+3, 
#                             units = "seconds")
#     
#     # view spectrogram data
#     # viewSpec(newSection_i)
#     
#     # Write new spectogram
#     writeWave(object = newSection_i, 
#               filename = paste0("D:/2023/ARU data/soundClipsForManualListening_caaf/", 
#                                 wetlandBirdRichness_caaf_filtered$Common.name[i], 
#                                 "_",
#                                 # manualListeningTest$startTime[i],
#                                 # "_",
#                                 str_replace(wetlandBirdRichness_caaf_filtered$file_part2[i], 
#                                             pattern = ".BirdNET.results.csv", 
#                                             replacement = ".wav")
#               ),
#               extensible = FALSE)
#     
#   }
# )

