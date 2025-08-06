## ---------------------------
##
## Script name: 01.04_wetlandBenefitsTradeoffs_dataUpdateCheck.R 
##
## Purpose of script: Analyze relationships between previous and current flux values
##
## Author: Ash Melo
##
## Date Created: 2025-08-05
## 
## Email: a_melo@ducks.ca
##
## --------------------------- 

# Load Libraries
library(readxl)
library(dplyr)
library(ggplot2)
library(ggpubr)
library(gridExtra)
library(units)
library(ggpmisc)
library(ggtext)

# 1. Load Data -----------------------------------------------------------------

data_original <- read.csv("wetland_tradeoff_df_2025-05-02.csv") 
  colnames(data_original) <-  paste0(colnames(data_original), "_original")
data_updated <- read_excel("wetland_tradeoff_df_2025-07-28.xlsx", na = "NA") 
  colnames(data_updated) <-  paste0(colnames(data_updated), "_updated")

# Data summary
summary(data_original) 
summary(data_updated)

data <- data_original %>% 
  full_join(data_updated, by = c("site_id_original" = "site_id_updated"))


GWPcheck <- ggplot(data, aes(pl_gwp_sum_original, pl_gwp_sum_g_updated)) + 
  geom_point() + 
  geom_smooth(method = "lm", col = "black", formula = 'y~x')+
  theme_classic(base_size = 16) + 
  stat_poly_eq(use_label(c("R2")), size = rel(6)) + 
  labs(x = "Original GWP (pseudo-log)", y = "Updated GWP (pseudo-log)")
GWPcheck

CO2check <- ggplot(data, aes(pl_flux_co2_original, pl_flux_co2_updated)) + 
  geom_point() + 
  geom_smooth(method = "lm", col = "black", formula = 'y~x')+
  theme_classic(base_size = 16) + 
  stat_poly_eq(use_label(c("R2")), size = rel(6)) + 
  labs(x = "Original CO2 flux (pseudo-log)", y = "Updated CO2 flux (pseudo-log)")
CO2check

CH4check <- ggplot(data, aes(log_flux_ch4_original, log_flux_ch4_updated)) + 
  geom_point() + 
  geom_smooth(method = "lm", col = "black", formula = 'y~x')+
  theme_classic(base_size = 16) + 
  stat_poly_eq(use_label(c("R2")), size = rel(6))  + 
  labs(x = "Original CH4 flux (log)", y = "Updated CH4 (log)")
CH4check

N2Ocheck <- ggplot(data, aes(pl_flux_n2o_original, pl_flux_n2o_updated)) + 
  geom_point() + 
  geom_smooth(method = "lm", col = "black", formula = 'y~x')+
  theme_classic(base_size = 16) + 
  stat_poly_eq(use_label(c("R2")), size = rel(6)) + 
  labs(x = "Original N2O flux (pseudo-log)", y = "Updated N2O flux (pseudo-log)")
N2Ocheck 

grid.arrange(GWPcheck, CO2check, CH4check, N2Ocheck)

#Look at "outliers" 
# CO2: SK-C-10 has a higher flux (1.64) than updated (-0.46)
View(data %>% select(site_id_original, pl_flux_co2_original, pl_flux_co2_updated) %>% arrange(pl_flux_co2_updated))
# N2O: AB01 has a higher updated flux (3.42), than original (-1.3)
View(data %>% select(site_id_original, pl_flux_n2o_original, pl_flux_n2o_updated) %>% arrange(pl_flux_n2o_original))