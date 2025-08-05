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
data_updated <- read_excel("AshCheck/wetland_tradeoff_df_2025-07-28.xlsx", na = "NA") 
  colnames(data_updated) <-  paste0(colnames(data_updated), "_updated")

# Data summary
summary(data_original) 
summary(data_updated)

data <- data_original %>% 
  full_join(data_updated, by = c("site_id_original" = "site_id_updated"))


ggplot(data, aes(pl_gwp_sum_original, pl_gwp_sum_g_updated)) + 
  geom_point() + 
  geom_smooth(method = "lm", col = "black", formula = 'y~x')+
  theme_classic(base_size = 16) + 
  stat_poly_eq(use_label(c("R2")), size = rel(6)) 


ggplot(data, aes(pl_flux_co2_original, pl_flux_co2_updated)) + 
  geom_point() + 
  geom_smooth(method = "lm", col = "black", formula = 'y~x')+
  theme_classic(base_size = 16) + 
  stat_poly_eq(use_label(c("R2")), size = rel(6)) 

ggplot(data, aes(log_flux_ch4_original, log_flux_ch4_updated)) + 
  geom_point() + 
  geom_smooth(method = "lm", col = "black", formula = 'y~x')+
  theme_classic(base_size = 16) + 
  stat_poly_eq(use_label(c("R2")), size = rel(6)) 

ggplot(data, aes(pl_flux_n2o_original, pl_flux_n2o_updated)) + 
  geom_point() + 
  geom_smooth(method = "lm", col = "black", formula = 'y~x')+
  theme_classic(base_size = 16) + 
  stat_poly_eq(use_label(c("R2")), size = rel(6)) 
