## ---------------------------
##
## Script name: 02.1_wetlandBenefitsTradeoffs_analyses.R 
##
## Purpose of script: Analyze relationships between productivity, GHG flux, & wetland bird richness
##
## Author: Ash Pidwerbesky, James Paterson
##
## Date Created: 2024-09-16
## Date Edited: 2025-05-26
## 
## Email: a_pidwerbesky@ducks.ca, j_paterson@ducks.ca
##
## ---------------------------

# Load Libraries
library(dplyr)
library(ggplot2)
library(ggpubr)
library(gridExtra)
library(units)
library(ggpmisc)
library(ggtext)

# 1. Load Data -----------------------------------------------------------------

data <- read.csv("wetland_tradeoff_df_2025-05-02.csv")

# Data summary
summary(data)

# 2. Correlation Analyses -------------------------------------------------------------

# Correlation Matrix. 8 total variables
corMatrix <- data %>%
  dplyr::select(total_density, aerial_nep, 
                pl_flux_co2, pl_flux_n2o, log_flux_ch4, pl_gwp_sum, 
                wetlandBirdRichness, wetlandArea_500m) %>% # Removed from SW version: aerial_gpp, aerial_r, pl_gwp_co2, pl_gwp_n2o, log_gwp_ch4.
  cor(., 
      method = c("pearson"),
      use =  "pairwise.complete.obs") %>%
  round(., digits = 2)

# Set diagonal of same variables to NA
corMatrix[corMatrix == 1] <- NA

# Set lower triangle to NA
corMatrix[lower.tri(corMatrix)] <- NA

# Adjust because we didn't examine comparisons between CO2, N02, CH4, Total GWP, and R wasn't compared to anything but NEP and total density
corMatrix["pl_flux_co2","pl_gwp_sum"] <- NA  # 
corMatrix["pl_flux_n2o","pl_gwp_sum"] <- NA
corMatrix["log_flux_ch4","pl_gwp_sum"] <- NA
corMatrix["pl_flux_co2","pl_flux_n2o"] <- NA  # 
corMatrix["pl_flux_co2","log_flux_ch4"] <- NA  # 
corMatrix["pl_flux_n2o","log_flux_ch4"] <- NA  # 
corMatrix["pl_flux_n2o","log_flux_ch4"] <- NA  # 

# How many pairwise tests?
sum(!is.na(corMatrix))

# Correlation tests (p-values)
# Use corMatrix as base
corPMatrix <- corMatrix
corPMatrix[is.numeric(corPMatrix)] <- NA

# Use loop to fill in matrix
# For each row...
for(i in 1:nrow(corPMatrix)){
  # For each column...
  for(k in 1:ncol(corPMatrix)){
    
    row_i <- row.names(corPMatrix)[i]
    col_k <- colnames(corPMatrix)[k]
    
    data_ik <- data %>%
      dplyr::select(any_of(c(row_i, col_k))) %>%
      na.omit()
    
    corPMatrix[i,k] <- round(cor.test(x = data_ik[,row_i], y = data_ik[,col_k])$p.value, 5)
    corPMatrix[lower.tri(corPMatrix)] <- NA
    
    
    
  }
  
}


# Set diagonal of same variables to NA
corPMatrix[corPMatrix == 0] <- NA

# Adjust for comparisons we don't test
corPMatrix["pl_flux_co2","pl_gwp_sum"] <- NA  # 
corPMatrix["pl_flux_n2o","pl_gwp_sum"] <- NA
corPMatrix["log_flux_ch4","pl_gwp_sum"] <- NA
corPMatrix["pl_flux_co2","pl_flux_n2o"] <- NA  # 
corPMatrix["pl_flux_co2","log_flux_ch4"] <- NA  # 
corPMatrix["pl_flux_n2o","log_flux_ch4"] <- NA  # 

# Perform Bonferroni correction on 30 tests
p_adjusted <- p.adjust(corPMatrix[!is.na(corPMatrix)], 
                       method = "holm", 
                       n = sum(!is.na(corMatrix))) # 22 comparisons

# Make matrix to hold values
corPadjMatrix <- corPMatrix

# Vector of original p-values
corPOriginal <- corPMatrix[!is.na(corPMatrix)]

# Put adjusted p-values in same matrix
for(j in 1:length(p_adjusted)){
  corPOriginal_j <- corPOriginal[j]
  corPadjMatrix[corPadjMatrix == corPOriginal_j] <- p_adjusted[j]
  
}

# Print correlation matrix with corrected p-values and only specific pair-wise comparisons
print(corPadjMatrix)

# Write csv of corMatrix to use as Table 
write.csv(corMatrix,
          "Outputs/tableS4_correlationMatrix.csv")

# 3. Figure 1 -----------------------------------------------------

richnessVegDens <- ggplot(data = data %>% 
                            filter(!is.na(wetlandBirdRichness), !is.na(total_density)), 
                          aes(x = total_density, y = wetlandBirdRichness)) + 
  geom_point() + 
  #geom_smooth(method = "lm", se = F, col = "black")+ 
  theme_classic(base_size = 14) + 
  labs(y = "Wetland bird richness" ,  
       x = expression(atop("Density of wetland emergent", paste("vegetation (kg m"^-2,")"))), tag = "a") + 
  ggtitle("Emergent vegetation density") +
  theme(plot.title = element_text(hjust = 0.5)) + 
  scale_y_continuous(breaks = c(0, 5, 10, 15, 20, 25, 30), limits = c(0, 30)) +
  scale_x_continuous(breaks = c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6), limits = c(0, 0.6))
richnessVegDens

richnessNEP <- ggplot(data = data%>% filter(!is.na(wetlandBirdRichness), !is.na(aerial_nep)), 
                      aes( x = aerial_nep, y = wetlandBirdRichness)) + 
  geom_point() + 
  #geom_smooth(method = "lm", se = F, col = "black")+ 
  theme_classic(base_size = 14) + 
  labs(y = "Wetland bird richness" ,  
       x = expression(atop(paste("NEP"[italic("aq")], " (g O"[2],"m"^-2,"d"^-1,")"), " ")), tag = "b") + 
  ggtitle("Aquatic productivity") +
  theme(plot.title = element_text(hjust = 0.5)) + 
  scale_y_continuous(breaks = c(0, 5, 10, 15, 20, 25, 30), limits = c(0, 30)) +
  scale_x_continuous(breaks = c(-20, -10, 0, 10), limits = c(-20, 15))
richnessNEP

fig1 <- grid.arrange(richnessVegDens, richnessNEP, nrow = 1)

# Save
ggsave(fig1, file = "Outputs/figure1.png", 
       width = 9, height = 4)

# 4. Figure 2 -----------------------------------------------------

# James: keeping version from AP re-creating SW's figures, but adding version that uses actual data in correlation test (pseuo-log, and log transformed total and specific fluxes)

# vegDensGWP <- ggplot(data = data %>% 
#                        filter(!is.na(total_density)), 
#                      aes( x = total_density, y = gwp_sum)) + 
#   geom_point() + 
#   geom_smooth(method = "lm", se = F, col = "black", formula = 'y~x')+ 
#   scale_y_continuous(breaks=c(0, 10000, 100000), 
#                      labels = expression("0", "10"^4, "10"^5)) +
#   coord_cartesian(ylim = c(0, 100000))+
#   theme_classic(base_size = 14) + 
#   labs(y = expression(atop("Total Instantaneous GHG Flux "[italic("aq")], paste("(mg CO"[2], " m"^-2, " d"^-1, ")"))),  
#        x = " ", #expression(atop("Density of wetland emergent", paste("vegetation (kg m"^-2,")"))), 
#        tag = "a") + 
#   ggtitle("Emergent vegetation density") +
#   theme(plot.title = element_text(hjust = 0.5))

vegDensGWP <- ggplot(data = data %>% 
                       filter(!is.na(total_density)), 
                     aes( x = total_density, y = pl_gwp_sum)) + 
  geom_point() + 
  scale_y_continuous(breaks = c(0, 1, 2, 3, 4, 5), limits = c(-0.75, 5)) +
  # geom_smooth(method = "lm", se = F, col = "black", formula = 'y~x')+ 
  # scale_y_continuous(breaks=c(0, 10000, 100000), 
  #                    labels = expression("0", "10"^4, "10"^5)) +
  # coord_cartesian(ylim = c(0, 100000))+
  theme_classic(base_size = 14) + 
  labs(y = expression(atop("Total Instantaneous GHG Flux "[italic("aq")], paste("pseudo-log","(mg CO"[2], " m"^-2, " d"^-1, ")"))),  
       x = " ", #expression(atop("Density of wetland emergent", paste("vegetation (kg m"^-2,")"))), 
       tag = "a") + 
  ggtitle("Emergent vegetation density") +
  theme(plot.title = element_text(hjust = 0.5))
vegDensGWP

# vegDensCO2 <- ggplot(data = data%>% filter(!is.na(total_density)), 
#                      aes( x = total_density, y = flux_co2)) + 
#   geom_point() + 
#   #geom_smooth(method = "lm", se = F, col = "black")+ 
#   # scale_y_continuous(breaks=c(0, 10000, 100000), 
#   #                    labels = expression("0", "10"^4, "10"^5)) +
#   # coord_cartesian(ylim = c(0, 100000))+
#   theme_classic(base_size = 14) + 
#   labs(y = expression(atop(CO[2]*" "[italic("aq")], "(mmol m"^-2*" d"^-1*")")),  
#        x = " ", #expression(atop("Density of wetland emergent", paste("vegetation (kg m"^-2,")"))), 
#        tag = "b") 

vegDensCO2 <- ggplot(data = data%>% filter(!is.na(total_density)), 
                     aes( x = total_density, y = pl_flux_co2)) + 
  geom_point() + 
  scale_y_continuous(breaks = c(-1, 0, 1, 2, 3, 4, 5), limits = c(-1.4, 5)) +
  #geom_smooth(method = "lm", se = F, col = "black")+ 
  # scale_y_continuous(breaks=c(0, 10000, 100000), 
  #                    labels = expression("0", "10"^4, "10"^5)) +
  # coord_cartesian(ylim = c(0, 100000))+
  theme_classic(base_size = 14) + 
  labs(y = expression(atop(CO[2]*" "[italic("aq")], "pseudo-log(mmol m"^-2*" d"^-1*")")),  
       x = " ", #expression(atop("Density of wetland emergent", paste("vegetation (kg m"^-2,")"))), 
       tag = "b") 
vegDensCO2

# vegDensCH4 <- ggplot(data = data %>% 
#                        filter(!is.na(total_density)), 
#                      aes( x = total_density, y = flux_ch4)) + 
#   geom_point() + 
#   geom_smooth(method = "lm", se = F, col = "black", formula = 'y~x')+ 
#   # scale_y_continuous(breaks=c(0, 1, 10, 100))+ 
#   #                    labels = expression("0", "10"^4, "10"^5)) +
#   # coord_cartesian(ylim = c(0, 100000))+
#   theme_classic(base_size = 14) + 
#   labs(y = expression(atop(CH[4]*" "[italic("aq")], "(mmol m"^-2*" d"^-1*")")),  
#        x = " ", #expression(atop("Density of wetland emergent", paste("vegetation (kg m"^-2,")"))), 
#        tag = "c") + 
#   scale_y_log10(breaks=c(0, 1, 10, 100))

# vegDensCH4 <- ggplot(data = data %>% 
#                        filter(!is.na(total_density)), 
#                      aes( x = total_density, y = flux_ch4)) + 
#   geom_point() + 
#   geom_smooth(method = "lm", se = F, col = "black", formula = 'y~x')+ 
#   # scale_y_continuous(breaks=c(0, 1, 10, 100))+ 
#   #                    labels = expression("0", "10"^4, "10"^5)) +
#   # coord_cartesian(ylim = c(0, 100000))+
#   theme_classic(base_size = 14) + 
#   labs(y = expression(atop(CH[4]*" "[italic("aq")], "(mmol m"^-2*" d"^-1*")")),  
#        x = " ", #expression(atop("Density of wetland emergent", paste("vegetation (kg m"^-2,")"))), 
#        tag = "c") + 
#   scale_y_log10(breaks=c(0, 1, 10, 100))

vegDensCH4 <- ggplot(data = data %>% 
                       filter(!is.na(total_density)), 
                     aes( x = total_density, y = log_flux_ch4)) + 
  geom_point() + 
  geom_smooth(method = "lm", se = F, col = "black", formula = 'y~x')+ 
  scale_y_continuous(breaks = c(0, 0.5, 1.0, 1.5, 2.0), limits = c(-0.10, 2.1)) +
  # scale_y_continuous(breaks=c(0, 1, 10, 100))+ 
  #                    labels = expression("0", "10"^4, "10"^5)) +
  # coord_cartesian(ylim = c(0, 100000))+
  theme_classic(base_size = 14) + 
  labs(y = expression(atop(CH[4]*" "[italic("aq")], "log(mmol m"^-2*" d"^-1*")")),  
       x = " ", #expression(atop("Density of wetland emergent", paste("vegetation (kg m"^-2,")"))), 
       tag = "c")

vegDensCH4


# vegDensN2O <- ggplot(data = data %>% 
#                        filter(!is.na(total_density)), 
#                      aes( x = total_density, y = flux_n2o)) + 
#   geom_point() + 
#   scale_y_continuous(breaks = c(0, 0.05, 0.10, 0.15),
#                      limits = c(-0.01, 0.15)) +
#   #geom_smooth(method = "lm", se = F, col = "black")+ 
#   #scale_y_continuous(breaks=c(0, 1, 10, 100))+ 
#   #                    labels = expression("0", "10"^4, "10"^5)) +
#   # coord_cartesian(ylim = c(0, 100000))+
#   theme_classic(base_size = 14) + 
#   labs(y = expression(atop(N[2]*"O "[italic("aq")], "(mmol m"^-2*" d"^-1*")")),  
#        x = expression(atop("Density of wetland emergent", paste("vegetation (kg m"^-2,")"))), tag = "d") 

vegDensN2O <- ggplot(data = data %>% 
                       filter(!is.na(total_density)), 
                     aes( x = total_density, y = pl_flux_n2o)) + 
  geom_point() + 
  scale_y_continuous(breaks = c(-2, -1, 0, 1, 2, 3, 4, 5), limits = c(-2.15, 5.5)) +
  # scale_y_continuous(breaks = c(0, 0.05, 0.10, 0.15),
  #                    limits = c(-0.01, 0.15)) +
  #geom_smooth(method = "lm", se = F, col = "black")+ 
  #scale_y_continuous(breaks=c(0, 1, 10, 100))+ 
  #                    labels = expression("0", "10"^4, "10"^5)) +
  # coord_cartesian(ylim = c(0, 100000))+
  theme_classic(base_size = 14) + 
  labs(y = expression(atop(N[2]*"O "[italic("aq")], "pseudo-log(mmol m"^-2*" d"^-1*")")),  
       x = expression(atop("Density of wetland emergent", paste("vegetation (kg m"^-2,")"))), tag = "d") 
vegDensN2O

# nepGWP <- ggplot(data = data %>% 
#                    filter(!is.na(aerial_nep)), 
#                  aes( x = aerial_nep, y = gwp_sum)) + 
#   geom_point() + 
#   geom_smooth(method = "lm", se = F, col = "black", formula = 'y~x')+ 
#   scale_y_continuous(breaks=c(0, 10000, 100000), 
#                      labels = expression("0", "10"^4, "10"^5)) +
#   coord_cartesian(ylim = c(0, 100000))+
#   theme_classic(base_size = 14) + 
#   labs(y = " ", #expression(atop("Total Instantaneous GHG Flux "[italic("aq")], paste("(mg CO"[2], " m"^-2, " d"^-1, ")"))),  
#        x = " ", #expression(atop(paste("NEP"[italic("aq")], " (g O"[2],"m"^-2,"d"^-1,")"), " ")), 
#        tag = "e") + 
#   ggtitle("Aquatic productivity") +
#   theme(plot.title = element_text(hjust = 0.5))

nepGWP <- ggplot(data = data %>% 
                   filter(!is.na(aerial_nep)), 
                 aes( x = aerial_nep, y = pl_gwp_sum)) + 
  geom_point() + 
  scale_y_continuous(breaks = c(0, 1, 2, 3, 4, 5), limits = c(-0.75, 5)) +
  # geom_smooth(method = "lm", se = F, col = "black", formula = 'y~x')+ 
  # scale_y_continuous(breaks=c(0, 10000, 100000), 
  #                    labels = expression("0", "10"^4, "10"^5)) +
  # coord_cartesian(ylim = c(0, 100000))+
  theme_classic(base_size = 14) + 
  labs(y = " ", #expression(atop("Total Instantaneous GHG Flux "[italic("aq")], paste("(mg CO"[2], " m"^-2, " d"^-1, ")"))),  
       x = " ", #expression(atop(paste("NEP"[italic("aq")], " (g O"[2],"m"^-2,"d"^-1,")"), " ")), 
       tag = "e") + 
  ggtitle("Aquatic productivity") +
  theme(plot.title = element_text(hjust = 0.5))
nepGWP

# nepCO2 <- ggplot(data = data %>% 
#                    filter(!is.na(aerial_nep)), 
#                  aes( x = aerial_nep, y = flux_co2)) + 
#   geom_point() + 
#   #geom_smooth(method = "lm", se = F, col = "black")+ 
#   # scale_y_continuous(breaks=c(0, 10000, 100000), 
#   #                    labels = expression("0", "10"^4, "10"^5)) +
#   # coord_cartesian(ylim = c(0, 100000))+
#   theme_classic(base_size = 14) + 
#   labs(y = " ", # expression(atop(CO[2]*" "[italic("aq")], "(mmol m"^-2*" d"^-1*")")),  
#        x = " ", #expression(atop(paste("NEP"[italic("aq")], " (g O"[2],"m"^-2,"d"^-1,")"), " "))
#        tag = "f") 

nepCO2 <- ggplot(data = data %>% 
                   filter(!is.na(aerial_nep)), 
                 aes( x = aerial_nep, y = pl_flux_co2)) + 
  geom_point() + 
  scale_y_continuous(breaks = c(-1, 0, 1, 2, 3, 4, 5), limits = c(-1.4, 5)) +
  #geom_smooth(method = "lm", se = F, col = "black")+ 
  # scale_y_continuous(breaks=c(0, 10000, 100000), 
  #                    labels = expression("0", "10"^4, "10"^5)) +
  # coord_cartesian(ylim = c(0, 100000))+
  theme_classic(base_size = 14) + 
  labs(y = " ", # expression(atop(CO[2]*" "[italic("aq")], "(mmol m"^-2*" d"^-1*")")),  
       x = " ", #expression(atop(paste("NEP"[italic("aq")], " (g O"[2],"m"^-2,"d"^-1,")"), " "))
       tag = "f") 
nepCO2

# nepCH4 <- ggplot(data = data%>% filter(!is.na(aerial_nep)), 
#                  aes( x = aerial_nep, y = flux_ch4)) + 
#   geom_point() + 
#   geom_smooth(method = "lm", se = F, col = "black", formula = 'y~x')+ 
#   # scale_y_continuous()+ 
#   #                    labels = expression("0", "10"^4, "10"^5)) +
#   # coord_cartesian(ylim = c(0, 100000))+
#   theme_classic(base_size = 14) + 
#   labs(y = " ",  #expression(atop(CH[4]*" "[italic("aq")], "(mmol m"^-2*" d"^-1*")"))
#        x = " ", #expression(atop(paste("NEP"[italic("aq")], " (g O"[2],"m"^-2,"d"^-1,")"), " ")), 
#        tag = "g") + 
#   scale_y_log10(breaks=c(0, 1, 10, 100))

nepCH4 <- ggplot(data = data%>% filter(!is.na(aerial_nep)), 
                 aes( x = aerial_nep, y = log_flux_ch4)) + 
  geom_point() + 
  geom_smooth(method = "lm", se = F, col = "black", formula = 'y~x')+ 
  scale_y_continuous(breaks = c(0, 0.5, 1.0, 1.5, 2.0), limits = c(-0.10, 2.1)) +
  # scale_y_continuous()+ 
  #                    labels = expression("0", "10"^4, "10"^5)) +
  # coord_cartesian(ylim = c(0, 100000))+
  theme_classic(base_size = 14) + 
  labs(y = " ",  #expression(atop(CH[4]*" "[italic("aq")], "(mmol m"^-2*" d"^-1*")"))
       x = " ", #expression(atop(paste("NEP"[italic("aq")], " (g O"[2],"m"^-2,"d"^-1,")"), " ")), 
       tag = "g")
nepCH4


# nepN2O <- ggplot(data = data %>% 
#                    filter(!is.na(aerial_nep)), 
#                  aes( x = aerial_nep, y = flux_n2o)) + 
#   geom_point() +
#   scale_y_continuous(breaks = c(0, 0.05, 0.10, 0.15),
#                      limits = c(-0.01, 0.15)) +
#   #geom_smooth(method = "lm", se = F, col = "black")+ 
#   #scale_y_continuous(breaks=c(0, 1, 10, 100))+ 
#   #                    labels = expression("0", "10"^4, "10"^5)) +
#   # coord_cartesian(ylim = c(0, 100000))+
#   theme_classic(base_size = 14) + 
#   labs(y = " ", #expression(atop(N[2]*"O "[italic("aq")], "(mmol m"^-2*" d"^-1*")")),  
#        x = expression(atop(paste("NEP"[italic("aq")], " (g O"[2]," m"^-2,"d"^-1,")"), " ")), tag = "h") 

nepN2O <- ggplot(data = data %>% 
                   filter(!is.na(aerial_nep)), 
                 aes( x = aerial_nep, y = pl_flux_n2o)) + 
  geom_point() +
  scale_y_continuous(breaks = c(-2, -1, 0, 1, 2, 3, 4, 5), limits = c(-2.15, 5.5)) +
  # scale_y_continuous(breaks = c(0, 0.05, 0.10, 0.15),
  #                    limits = c(-0.01, 0.15)) +
  #geom_smooth(method = "lm", se = F, col = "black")+ 
  #scale_y_continuous(breaks=c(0, 1, 10, 100))+ 
  #                    labels = expression("0", "10"^4, "10"^5)) +
  # coord_cartesian(ylim = c(0, 100000))+
  theme_classic(base_size = 14) + 
  labs(y = " ", #expression(atop(N[2]*"O "[italic("aq")], "(mmol m"^-2*" d"^-1*")")),  
       x = expression(atop(paste("NEP"[italic("aq")], " (g O"[2]," m"^-2,"d"^-1,")"), " ")), tag = "h") 
nepN2O

fig2 <- grid.arrange(vegDensGWP, nepGWP, vegDensCO2, nepCO2, vegDensCH4, nepCH4, vegDensN2O, nepN2O, nrow = 4)

# Save
ggsave(fig2, file = "Outputs/figure2.png", width = 9, height = 14)

# 5. Figure 3 -----------------------------------------------------

# See comments from above, adjusting figures to use scale in Pearson's correlation analyses

# richnessGWP <- ggplot(data = data %>% filter(!is.na(wetlandBirdRichness)), 
#                       aes( x = wetlandBirdRichness, y = gwp_sum)) + 
#   geom_point() + 
#   geom_smooth(method = "lm", se = F, col = "black", formula = 'y~x')+ 
#   scale_y_continuous(breaks=c(0, 10000, 100000), 
#                      labels = expression("0", "10"^4, "10"^5)) +
#   coord_cartesian(ylim = c(0, 100000))+
#   theme_classic(base_size = 14) + 
#   scale_x_continuous(breaks = c(5, 10, 15, 20, 25, 30), limits = c(5, 30)) +
#   labs(y = expression(atop("Total Instantaneous GHG Flux "[italic("aq")], paste("(mg CO"[2], " m"^-2, " d"^-1, ")"))),  
#        x = "Wetland bird richness", tag = "a") 

richnessGWP <- ggplot(data = data %>% filter(!is.na(wetlandBirdRichness)), 
                     aes( x = wetlandBirdRichness, y = pl_gwp_sum)) + 
  geom_point() + 
  geom_smooth(method = "lm", se = F, col = "black", formula = 'y~x')+ 
  # scale_y_continuous(breaks=c(0, 10000, 100000), 
  #                    labels = expression("0", "10"^4, "10"^5)) +
  # coord_cartesian(ylim = c(0, 100000))+
  theme_classic(base_size = 14) + 
  scale_x_continuous(breaks = c(5, 10, 15, 20, 25, 30), limits = c(5, 30)) +
  scale_y_continuous(breaks = c(0, 1, 2, 3, 4, 5), limits = c(-0.75, 5)) +
  labs(y = expression(atop("Total Instantaneous GHG Flux "[italic("aq")], paste("pseudo-log(mg CO"[2], " m"^-2, " d"^-1, ")"))),  
       x = "Wetland bird richness", tag = "a") 
richnessGWP

# richnessCO2 <- ggplot(data = data %>% filter(!is.na(wetlandBirdRichness)), 
#                       aes( x = wetlandBirdRichness, y = flux_co2)) + 
#   geom_point() + 
#   geom_smooth(method = "lm", se = F, col = "black")+ 
#   # scale_y_continuous(breaks=c(0, 10000, 100000), 
#   #                    labels = expression("0", "10"^4, "10"^5)) +
#   # coord_cartesian(ylim = c(0, 100000))+
#   theme_classic(base_size = 14) + 
#   scale_x_continuous(breaks = c(5, 10, 15, 20, 25, 30), limits = c(5, 30)) +
#   labs(y = expression(atop(CO[2]*" "[italic("aq")], "(mmol m"^-2*" d"^-1*")")),  
#        x = "Wetland bird richness", tag = "b") 

richnessCO2 <- ggplot(data = data %>% filter(!is.na(wetlandBirdRichness)), 
                      aes( x = wetlandBirdRichness, y = pl_flux_co2)) + 
  geom_point() + 
  geom_smooth(method = "lm", se = F, col = "black")+ 
  # scale_y_continuous(breaks=c(0, 10000, 100000), 
  #                    labels = expression("0", "10"^4, "10"^5)) +
  # coord_cartesian(ylim = c(0, 100000))+
  theme_classic(base_size = 14) + 
  scale_x_continuous(breaks = c(5, 10, 15, 20, 25, 30), limits = c(5, 30)) +
  scale_y_continuous(breaks = c(-1, 0, 1, 2, 3, 4, 5), limits = c(-1.4, 5)) +
  labs(y = expression(atop(CO[2]*" "[italic("aq")], "pseudo-log(mmol m"^-2*" d"^-1*")")),  
       x = "Wetland bird richness", tag = "b") 
richnessCO2

# richnessCH4 <- ggplot(data = data %>% filter(!is.na(wetlandBirdRichness)), 
#                       aes( x = wetlandBirdRichness, y = flux_ch4)) + 
#   geom_point() + 
#   geom_smooth(method = "lm", se = F, col = "black", formula = 'y~x')+ 
#   scale_y_continuous(breaks=c(0, 1, 10, 100))+ 
#   #                    labels = expression("0", "10"^4, "10"^5)) +
#   # coord_cartesian(ylim = c(0, 100000))+
#   theme_classic(base_size = 14) + 
#   scale_x_continuous(breaks = c(5, 10, 15, 20, 25, 30), limits = c(5, 30)) +
#   labs(y = expression(atop(CH[4]*" "[italic("aq")], "(mmol m"^-2*" d"^-1*")")),  
#        x = "Wetland bird richness", tag = "c") + scale_y_log10()

richnessCH4 <- ggplot(data = data %>% filter(!is.na(wetlandBirdRichness)), 
                      aes( x = wetlandBirdRichness, y = log_flux_ch4)) + 
  geom_point() + 
  geom_smooth(method = "lm", se = F, col = "black", formula = 'y~x')+ 
  # scale_y_continuous(breaks=c(0, 1, 10, 100))+ 
  #                    labels = expression("0", "10"^4, "10"^5)) +
  # coord_cartesian(ylim = c(0, 100000))+
  theme_classic(base_size = 14) + 
  scale_x_continuous(breaks = c(5, 10, 15, 20, 25, 30), limits = c(5, 30)) +
  scale_y_continuous(breaks = c(-0.5, 0, 0.5, 1, 1.5, 2.0), limits = c(-0.6, 2)) +
  labs(y = expression(atop(CH[4]*" "[italic("aq")], "log(mmol m"^-2*" d"^-1*")")),  
       x = "Wetland bird richness", tag = "c")
richnessCH4


# richnessN2O <- ggplot(data = data%>% filter(!is.na(wetlandBirdRichness)), 
#                       aes( x = wetlandBirdRichness, y = flux_n2o)) + 
#   geom_point() + 
#   geom_smooth(method = "lm", se = F, col = "black", formula = 'y~x')+ 
#   scale_x_continuous(breaks = c(5, 10, 15, 20, 25, 30), limits = c(5, 30)) +
#   #scale_y_continuous(breaks=c(0, 1, 10, 100))+ 
#   #                    labels = expression("0", "10"^4, "10"^5)) +
#   # coord_cartesian(ylim = c(0, 100000))+
#   theme_classic(base_size = 14) + 
#   labs(y = expression(atop(N[2]*"O "[italic("aq")], "(mmol m"^-2*" d"^-1*")")),  
#        x = "Wetland bird richness", tag = "d") 

richnessN2O <- ggplot(data = data%>% filter(!is.na(wetlandBirdRichness)), 
                      aes( x = wetlandBirdRichness, y = pl_flux_n2o)) + 
  geom_point() + 
  # geom_smooth(method = "lm", se = F, col = "black", formula = 'y~x')+ 
  scale_x_continuous(breaks = c(5, 10, 15, 20, 25, 30), limits = c(5, 30)) +
  scale_y_continuous(breaks = c(-2, -1, 0, 1, 2, 3, 4, 5), limits = c(-2.15, 5.5)) +
  #scale_y_continuous(breaks=c(0, 1, 10, 100))+ 
  #                    labels = expression("0", "10"^4, "10"^5)) +
  # coord_cartesian(ylim = c(0, 100000))+
  theme_classic(base_size = 14) + 
  labs(y = expression(atop(N[2]*"O "[italic("aq")], "pseudo-log(mmol m"^-2*" d"^-1*")")),  
       x = "Wetland bird richness", tag = "d") 
richnessN2O


fig3 <- grid.arrange(richnessGWP, richnessCO2, richnessCH4, richnessN2O, nrow = 4)

# Save
ggsave(fig3, file = "Outputs/figure3.png", 
       width = 4, height = 14)

# 6. Figure 4 ------------------------------------------------------------------

wetlandAreaVegDens <- ggplot(data = data %>% 
                               filter(!is.na(total_density)), 
                             aes( x = wetlandArea_500m/1000000, y = total_density )) + 
  geom_point() + 
  # geom_smooth(method = "lm", se = F, col = "black", formula = 'y~x')+ 
  scale_x_continuous(#labels = c("0.0", "0.1", "0.2"), 
    breaks = c(0.0, 0.05, 0.10, 0.15, 0.20),
    limits = c(0, 0.2)) +
  scale_y_continuous(breaks = c(0.0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6),
                     limits = c(0, 0.6)) +
  theme_classic(base_size = 14) + 
  ylab("Density of wetland emergent<br>vegetation (kg m<sup>-2</sup>)") +
  xlab("Wetland area (km<sup>2</sup>) within 500m")+
  theme(axis.title.y = element_markdown(), 
        axis.title.x = element_markdown()) +
  labs(tag = "a") 
wetlandAreaVegDens

wetlandAreaGWP <- ggplot(data = data, aes( x = wetlandArea_500m/1000000, y = pl_gwp_sum )) + 
  geom_point() + 
  # geom_smooth(method = "lm", se = F, col = "black", formula = 'y~x')+ 
  # scale_y_continuous(breaks=c(0, 10000, 100000), 
  #                    labels = expression(0, 10^4, 10^5),
  #                    expand = expansion(mult = c(0.05, 0.05))) +
  scale_x_continuous(breaks = c(0.0, 0.10, 0.20, 0.30, 0.40),
                     limits = c(0, 0.42)) +
  scale_y_continuous(breaks = c(0, 1, 2, 3, 4, 5), limits = c(-0.75, 5)) +
  theme_classic(base_size = 14) + 
  labs(y = expression(atop("Total Instantaneous GHG Flux "[italic("aq")], paste("pseudo-log(mg CO"[2], " m"^-2, " d"^-1, ")"))),
       x = expression(paste("Wetland area (km"^2,") within 500m")), tag = "b") 
wetlandAreaGWP

wetlandAreaRichness_500m2 <- ggplot(data = data %>% filter(!is.na(wetlandBirdRichness)), 
                                    aes( x = wetlandArea_500m/1000000, y = wetlandBirdRichness)) + 
  geom_point() + 
  geom_smooth(method = "lm", se = F, col = "black", formula = 'y~x')+
  theme_classic(base_size = 14) + 
  scale_y_continuous(breaks = c(0, 5, 10, 15, 20, 25, 30),
                     limits = c(0, 30)) +
  scale_x_continuous(breaks = c(0.0, 0.10, 0.20, 0.30, 0.40),
                     limits = c(0, 0.42)) +
  labs(y = expression(atop("Wetland bird richness", " ")), 
       x = expression(paste("Wetland area (km"^2,") within 500m")), 
       tag = "c") 
wetlandAreaRichness_500m2

fig4 <- grid.arrange(wetlandAreaVegDens, wetlandAreaGWP, wetlandAreaRichness_500m2)

# Save
ggsave(fig4, file = "Outputs/figure4.png", 
       width = 4, height = 10)

# 7. Figure S3: richness vs wetland area - Confirmed Richness -----------------------------

richnessWetlandAreaPlot_250m2 <- ggplot(data = data %>%
                                          filter(!is.na(wetlandBirdRichness)), 
                                        aes(x = wetlandArea_250m/1000000, y =  wetlandBirdRichness)) + 
  geom_point() + 
  geom_smooth(method = "lm", col = "black", formula = 'y~x')+
  theme_classic(base_size = 16) + 
  scale_y_continuous(breaks = c(0, 5, 10, 15, 20, 25, 30), limits = c(0, 30)) +
  labs(y = "Wetland bird richness", x = expression(paste("Wetland area (km"^2,") ", bold("within 250m"))), tag = "a")+ 
  stat_poly_eq(use_label(c("R2")), size = rel(6)) 
richnessWetlandAreaPlot_250m2

richnessWetlandAreaPlot_500m2 <- ggplot(data = data %>%
                                          filter(!is.na(wetlandBirdRichness)), 
                                        aes(x = wetlandArea_500m/1000000, y =  wetlandBirdRichness)) + 
  geom_point() + 
  geom_smooth(method = "lm", col = "black", formula = 'y~x')+
  theme_classic(base_size = 16) + 
  scale_y_continuous(breaks = c(0, 5, 10, 15, 20, 25, 30), limits = c(0, 30)) +
  labs(y = "Wetland bird richness", x = expression(paste("Wetland area (km"^2,") ", bold("within 500m"))), tag = "b")+ 
  stat_poly_eq(use_label(c("R2")), size = rel(6)) 
richnessWetlandAreaPlot_500m2

richnessWetlandAreaPlot_1km2 <- ggplot(data = data%>%
                                         filter(!is.na(wetlandBirdRichness)), 
                                       aes( x = wetlandArea_1km/1000000, y =  wetlandBirdRichness)) + 
  geom_point() + 
  geom_smooth(method = "lm", col = "black", formula = 'y~x')+
  theme_classic(base_size = 16) + 
  scale_y_continuous(breaks = c(0, 5, 10, 15, 20, 25, 30), limits = c(0, 30)) +
  labs(y = "Wetland bird richness", x = expression(paste("Wetland area (km"^2,") ", bold("within 1km"))), tag = "c") + 
  stat_poly_eq(use_label(c("R2")), size = rel(6)) 
richnessWetlandAreaPlot_1km2

richnessWetlandAreaPlot_5km2 <- ggplot(data = data %>%
                                         filter(!is.na(wetlandBirdRichness)),
                                       aes( x = wetlandArea_5km/1000000, y =  wetlandBirdRichness)) + 
  geom_point() + 
  geom_smooth(method = "lm", col = "black", formula = 'y~x')+
  theme_classic(base_size = 16) + 
  scale_y_continuous(breaks = c(0, 5, 10, 15, 20, 25, 30), limits = c(0, 30)) +
  labs(y = "Wetland bird richness", x = expression(paste("Wetland area (km"^2,") ", bold("within 5km"))), tag = "d")+ 
  stat_poly_eq(use_label(c("R2")), size = rel(6)) 
richnessWetlandAreaPlot_5km2

figS3_richnessWetlandArea <- grid.arrange(richnessWetlandAreaPlot_250m2, richnessWetlandAreaPlot_500m2, 
                                    richnessWetlandAreaPlot_1km2, richnessWetlandAreaPlot_5km2)

# Save
ggsave(figS3_richnessWetlandArea, 
       file = "Outputs/figureS3_richnessWetlandArea.png", 
       width = 10, height = 7)

## THE END :)