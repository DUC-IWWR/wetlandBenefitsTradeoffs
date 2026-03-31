## -----------------------------------------------------------------------------
##
## Script name: 02.1_wetlandBenefitsTradeoffs_analyses.R 
##
## Purpose of script: test out bootstrap to estimate the number of species at a site with uncertainty 
##
## Author: Ash Melo (Pidwerbesky), James Paterson
##
## Date Created: 2026-03-23
## 
## Email: a_melo@ducks.ca, j_paterson@ducks.ca
##
## -----------------------------------------------------------------------------




###---------------------------------------------------------
# Use BiodiversityR and bootstrapping to estimate the number of species at a site with uncertainty".
###---------------------------------------------------------

library(dplyr)
library(tidyr)
library(lubridate)
library(BiodiversityR)
library(ggplot2)
library(purrr)

load("Data/wetlandBirdDetections_CAAF_2025-04-15.RData")

wetlandBirdDetections <- wetlandBirdDetections %>%
  mutate(week = floor_date(date, "week"))

df_wide <- wetlandBirdDetections %>%
  group_by(SITE_ID, week, Common.name) %>%
  summarise(count = n(), .groups = "drop") %>%
  pivot_wider(
    names_from = Common.name,
    values_from = count,
    values_fill = 0
  )

site_list <- df_wide %>%
  group_by(SITE_ID) %>%
  group_split()

names(site_list) <- unique(df_wide$SITE_ID)

accum_list <- map(
  names(site_list),
  function(site) {
    dat <- site_list[[site]]
    
    comm_mat <- dat %>%
      select(-SITE_ID, -week) %>%
      as.data.frame()
    
    # BiodiversityR accumulation
    acc <- accumresult(comm_mat, method = "exact", conditioned = F) 

    
    # Extract internal data frame (Kindt method)
    df <- data.frame(
      site = site,
      samples = acc$sites,
      richness = acc$richness,
      sd = acc$sd,
      lower = acc$richness - acc$sd,
      upper = acc$richness + acc$sd
    )
    
    df
  }
)

names(accum_list) <- names(site_list)

accum_all <- bind_rows(accum_list)

p <- ggplot(accum_all, aes(x = samples, y = richness, ymin = lower, ymax = upper)) +
  geom_ribbon(aes(colour=site), alpha=0.2, show.legend=FALSE) + 
  geom_line(aes(colour=site), size=2) +
  labs(
    x = "Sampling events",
    y = "Species richness",
    title = "Species Accumulation Curves by Site (BiodiversityR)"
  ) +
  theme_bw()

p 

## add to data ------------------------------------------------------------ 

wetland_tradeoff_df_2025 <- read_csv("wetland_tradeoff_df_2025-10-09.csv") %>% 
  select(-`...1`)

richness_SD <- accum_all %>% 
  group_by(site) %>% 
  filter(samples == max(samples)) %>% 
  select(site, richness, sd)  


wetland_tradeoff_df_2026 <- wetland_tradeoff_df_2025 %>% 
  left_join(richness_SD %>% 
              rename(richness_sd = sd, 
                     site_id = site, 
                     richnessConfirmed = richness))


write.csv(wetland_tradeoff_df_2026, "wetland_tradeoff_df_2026-03-31.csv")


# graveyard. 

# Use vegan::specaccum() and bootstrapping to estimate the number of species at a site with uncertainty". 
# Most examples use "sites" as samples, but we'd mostly be using "time" as samples (e.g., a day or a week) 
# that accumulate more observed species within a site.
# 
# 
# # 1. Learning :) ---------------------------------------------------------------
# library(vegan)
# 
# # example community matrix (samples × species)
# data(dune)     # vegan example dataset
# 
# # species accumulation curve with bootstrap uncertainty
# acc <- specaccum(dune, method = "exact", gamma = "boot", permutations = 1000)
# 
# # plot observed richness ± bootstrap SE
# plot(acc, ci.type = "poly", col = "blue", lwd = 2,
#      ci.lty = 0, ci.col = "lightblue",
#      xlab = "Number of samples", ylab = "Species richness")
# 
# acc$richness
# acc$sd
# 
# # What I learned: Dune sample data is a matrix of sample x species and values are abundance classes (1-5)
# # Species accumulation is across all samples (for dune example sample =location)
# # We would want to use time as a sample, probably won't use abundance classes? Probably use counts of detections instead. We'll see. 
# 
# # 2. Let's goooooo -------------------------------------------------------------
# 
# # Load libraries 
# library(dplyr)
# library(tidyr)
# library(vegan)
# library(ggplot2)
# library(purrr)
# library(lubridate)
# 
# ###---------------------------------------------------------
# ### 1. Load your data
# ###---------------------------------------------------------
# 
# load("Data/wetlandBirdDetections_CAAF_2025-04-15.RData")  
# 
# ###---------------------------------------------------------
# ### OPTIONAL: Weekly sampling instead of daily
# ## Uncomment if you prefer weeks.
# wetlandBirdDetections <- wetlandBirdDetections %>%
#   mutate(week = floor_date(date, "week"))
# 
# ###---------------------------------------------------------
# ### 4. Create SITE × SAMPLE × SPECIES incidence or abundance
# ###---------------------------------------------------------
# 
# df_wide <- wetlandBirdDetections %>%
#   group_by(SITE_ID, week, Common.name) %>%
#   summarise(count = n(), .groups = "drop") %>%
#   pivot_wider(
#     names_from = Common.name,
#     values_from = count,
#     values_fill = 0
#   )
# 
# 
# ###---------------------------------------------------------
# ### 5. Split into separate matrices per site
# ###---------------------------------------------------------
# site_list <- df_wide %>%
#   group_by(SITE_ID) %>%
#   group_split()
# 
# names(site_list) <- unique(df_wide$SITE_ID)
# 
# ###---------------------------------------------------------
# ### 6. Run specaccum for each site
# ###---------------------------------------------------------
# accum_list <- list()
# 
# for (site in names(site_list)) {
#   dat <- site_list[[site]]
#   
#   # remove SITE_ID and date column, keep only species columns
#   comm_mat <- dat %>%
#     select(-SITE_ID, -week) %>%
#     as.data.frame()
#   
#   # species accumulation curve with bootstrap uncertainty
#   acc <- specaccum(comm_mat, method = "random", permutations = 1000)
#   
#   accum_list[[site]] <- acc
# }
# 
# ###---------------------------------------------------------
# ### 7. Plot results for all sites
# ###---------------------------------------------------------
# par(mfrow = c(6, 10))  # adjust depending on number of sites
# 
# for (site in names(accum_list)) {
#   acc <- accum_list[[site]]
#   
#   plot(acc, ci.type = "poly",
#        col = "blue", lwd = 2,
#        ci.col = "lightblue",
#        main = paste(site),
#        xlab = "Sampling events",
#        ylab = "Species richness")
# }
# 
# par(mfrow = c(1, 1))
# 
# acc_AB_B_08 <- accum_list[["AB-B-08"]]
# 
# plot(
#   acc_AB_B_08,
#   ci.type = "poly",       # shaded CI
#   col = "blue",
#   lwd = 2,
#   ci.col = "lightblue",
#   xlab = "Sampling events",
#   ylab = "Species richness",
#   main = "Species Accumulation Curve - AB-B-08"
# )
# 
# ###---------------------------------------------------------
# ### 8. Extract site-level richness and CI into a summary table
# ###---------------------------------------------------------
# 
# library(dplyr)
# library(purrr)
# 
# richness_summary <- map_dfr(
#   names(accum_list),
#   function(site) {
#     acc <- accum_list[[site]]
#     
#     # Skip sites with invalid accumulation curves
#     if (length(acc$richness) == 0) {
#       return(
#         data.frame(
#           SITE_ID = site,
#           richness_estimate = NA,
#           sd_richness = NA
#         )
#       )
#     }
#     
#     # Extract richness estimate (last point on curve)
#     estimate <- tail(acc$richness, 1)
#     
#     # Extract SD (bootstrap SD at last point)
#     if (!is.null(acc$sd) && length(acc$sd) > 0) {
#       sd_richness <- tail(acc$sd, 1)
#     } else {
#       sd_richness <- NA
#     }
#     
#     data.frame(
#       SITE_ID = site,
#       richness_estimate = estimate,
#       sd_richness = sd_richness
#     )
#   }
# )
# 
# #https://rpubs.com/Roeland-KINDT/694021