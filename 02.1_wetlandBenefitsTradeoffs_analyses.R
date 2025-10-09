## ---------------------------
##
## Script name: 02.1_wetlandBenefitsTradeoffs_analyses.R 
##
## Purpose of script: Analyze relationships between productivity, GHG flux, & wetland bird richness
##
## Author: Ash Melo (Pidwerbesky), James Paterson
##
## Date Created: 2024-09-16
## Date Edited: 2025-10-09
## 
## Email: a_melo@ducks.ca, j_paterson@ducks.ca
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

# 1. Load Data & Summarize -----------------------------------------------------------------

data <- read.csv("wetland_tradeoff_df_2025-10-09.csv")

# Data summary
summary(data)

# How many sites with bird biodiversity data?
data %>%
  filter(!is.na(wetlandBirdRichness)) %>%
  pull(wetlandBirdRichness) %>%
  length(.)
# 57 sites

# How many sites with emergent vegetetation
data %>%
  filter(!is.na(total_density)) %>%
  pull(total_density) %>%
  length(.)
# 20 sites

# How many sites with NEPaq
data %>%
  filter(!is.na(aerial_nep)) %>%
  pull(aerial_nep) %>%
  length(.)
# 22 sites

# How many sites with total GHG fluxes (includes CO2, CH4, N2O)?
data %>%
  filter(!is.na(pl_gwp_sum_g)) %>%
  pull(pl_gwp_sum_g) %>%
  length(.)
# 65 sites

# Full summary of each variable created for Table S1 below

# 2. Correlation Analyses -------------------------------------------------------------

# Correlation Matrix. 8 total variables
corMatrix <- data %>%
  dplyr::select(total_density, aerial_nep, 
                pl_flux_co2, pl_flux_n2o, log_flux_ch4, pl_gwp_sum_g, 
                wetlandBirdRichness, wetlandArea_500m) %>% # Removed from SW version: aerial_gpp, aerial_r, pl_gwp_co2, pl_gwp_n2o, log_gwp_ch4.
  cor(., 
      method = c("pearson"),
      use =  "pairwise.complete.obs") #%>%
  # round(., digits = 2)

# Set diagonal of same variables to NA
corMatrix[corMatrix == 1] <- NA

# Set lower triangle to NA
corMatrix[lower.tri(corMatrix)] <- NA

# Adjust because we didn't examine comparisons between CO2, N02, CH4, Total GWP, and R wasn't compared to anything but NEP and total density
corMatrix["pl_flux_co2","pl_gwp_sum_g"] <- NA  # 
corMatrix["pl_flux_n2o","pl_gwp_sum_g"] <- NA
corMatrix["log_flux_ch4","pl_gwp_sum_g"] <- NA
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
    
    corPMatrix[i,k] <- round(cor.test(x = data_ik[[row_i]], y = data_ik[[col_k]])$p.value, 5)
    corPMatrix[lower.tri(corPMatrix)] <- NA
    
  }
  
}


# Set diagonal of same variables to NA
corPMatrix[corPMatrix == 0] <- NA

# Adjust for comparisons we don't test
corPMatrix["pl_flux_co2","pl_gwp_sum_g"] <- NA
corPMatrix["pl_flux_n2o","pl_gwp_sum_g"] <- NA
corPMatrix["log_flux_ch4","pl_gwp_sum_g"] <- NA
corPMatrix["pl_flux_co2","pl_flux_n2o"] <- NA 
corPMatrix["pl_flux_co2","log_flux_ch4"] <- NA
corPMatrix["pl_flux_n2o","log_flux_ch4"] <- NA

# Perform Holm correction on 22 tests
p_adjusted <- p.adjust(corPMatrix[!is.na(corPMatrix)], 
                       method = "holm", 
                       n = sum(!is.na(corMatrix)))

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
write.csv(corMatrix %>%
          round(., digits = 2), 
          paste0("Outputs/table_correlationMatrix_", Sys.Date(), ".csv")
          )

# 3. Figure 1 -----------------------------------------------------

df_plot_1a <- data %>%
  filter(!is.na(wetlandBirdRichness), !is.na(total_density))

# Run correlation test
cor_test_1a <- cor.test(df_plot_1a$total_density, df_plot_1a$wetlandBirdRichness, method = "pearson")

# Extract values
r_val_1a <- cor_test_1a$estimate
n_val_1a <- nrow(df_plot_1a)
p_adj_1a <- corPadjMatrix["total_density", "wetlandBirdRichness"]

richnessVegDens <- ggplot(df_plot_1a, 
                          aes(x = total_density, y = wetlandBirdRichness)) + 
  geom_point() + 
  theme_classic(base_size = 14) + 
  labs(y = "Wetland bird richness" ,  
       x = expression(atop("Density of wetland emergent", paste("vegetation (kg m"^-2,")"))), 
       tag = "a") + 
  ggtitle("Emergent vegetation density") +
  theme(plot.title = element_text(hjust = 0.5)) + 
  scale_y_continuous(breaks = c(0, 5, 10, 15, 20, 25, 30), limits = c(0, 30)) +
  scale_x_continuous(breaks = c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6), limits = c(0, 0.6)) +
  annotate("text", x = min(df_plot_1a$total_density), y = 30, 
           label = paste0("italic(r) == ", round(r_val_1a, 2), 
                          "*','~italic(n) == ", n_val_1a, 
                          "*','~italic(p)[adj] == ", signif(p_adj_1a, 3)), 
           parse = TRUE, hjust = 0, size = 4)

richnessVegDens

df_plot_1b <- data %>%
  filter(!is.na(wetlandBirdRichness), !is.na(aerial_nep))

# Run correlation test
cor_test_1b <- cor.test(df_plot_1b$aerial_nep, df_plot_1b$wetlandBirdRichness, method = "pearson")

# Extract values
r_val_1b <- cor_test_1b$estimate
n_val_1b <- nrow(df_plot_1b)
p_adj_1b <- corPadjMatrix["aerial_nep", "wetlandBirdRichness"]

richnessNEP <- ggplot(data = df_plot_1b, 
                      aes( x = aerial_nep, y = wetlandBirdRichness)) + 
  geom_point() + 
  #geom_smooth(method = "lm", se = F, col = "black")+ 
  theme_classic(base_size = 14) + 
  labs(y = "Wetland bird richness" ,  
       x = expression(atop(paste("NEP"[italic("aq")], " (g O"[2],"m"^-2,"d"^-1,")"), " ")), tag = "b") + 
  ggtitle("Aquatic productivity") +
  theme(plot.title = element_text(hjust = 0.5)) + 
  scale_y_continuous(breaks = c(0, 5, 10, 15, 20, 25, 30), limits = c(0, 30)) +
  scale_x_continuous(breaks = c(-20, -10, 0, 10), limits = c(-20, 15)) + 
  annotate("text", x = min(df_plot_1b$aerial_nep), y = 30, 
           label = paste0("italic(r) == ", round(r_val_1b, 2), 
                          "*','~italic(n) == ", n_val_1b,  
                          "*','~italic(p)[adj] == ", signif(p_adj_1b, 3)), 
           parse = TRUE, hjust = 0, size = 4)
richnessNEP

fig1 <- grid.arrange(richnessVegDens, richnessNEP, nrow = 1)

# Save
ggsave(fig1, file = paste0("Outputs/figure1_", Sys.Date(), ".png"), 
       width = 9, height = 4)

# 4. Figure 2 -----------------------------------------------------

df_plot_2a <- data %>%
  filter(!is.na(pl_gwp_sum_g), !is.na(total_density))

# Run correlation test
cor_test_2a <- cor.test(df_plot_2a$total_density, df_plot_2a$pl_gwp_sum_g, method = "pearson")

# Extract values
r_val_2a <- cor_test_2a$estimate
n_val_2a <- nrow(df_plot_2a)
p_adj_2a <- corPadjMatrix["total_density", "pl_gwp_sum_g"]

vegDensGWP <- ggplot(data = df_plot_2a, 
                     aes( x = total_density, y = pl_gwp_sum_g)) + 
  geom_point() + 
  scale_y_continuous(breaks = c(0, 1, 2, 3, 4, 5), limits = c(-0.75, 5)) +
  geom_smooth(method = "lm", se = F, col = "black", formula = 'y~x')+ 
  theme_classic(base_size = 14) + 
  labs(y = expression(atop("Total Instantaneous GHG Flux "[italic("aq")], paste("pseudo-log","(g CO"[2], " m"^-2, " d"^-1, ")"))),  
       x = " ", #expression(atop("Density of wetland emergent", paste("vegetation (kg m"^-2,")"))), 
       tag = "a") + 
  ggtitle("Emergent vegetation density") +
  theme(plot.title = element_text(hjust = 0.5))+
  annotate("text", x = min(df_plot_2a$total_density), y = 5, 
           label = paste0("italic(r) == ", round(r_val_2a, 2), 
                          "*','~italic(n) == ", n_val_2a, 
                          "*','~italic(p)[adj] == ", round(p_adj_2a, 2)), 
           parse = TRUE, hjust = 0, size = 4)
vegDensGWP

df_plot_2b <- data %>%
  filter(!is.na(pl_flux_co2), !is.na(total_density))

# Run correlation test
cor_test_2b <- cor.test(  df_plot_2b$total_density, df_plot_2b$pl_flux_co2, method = "pearson")

# Extract values
r_val_2b <- cor_test_2b$estimate
n_val_2b <- nrow(df_plot_2b)
p_adj_2b <- corPadjMatrix["total_density", "pl_flux_co2"]

vegDensCO2 <- ggplot(data = data%>% filter(!is.na(total_density)), 
                     aes( x = total_density, y = pl_flux_co2)) + 
  geom_point() + 
  scale_y_continuous(breaks = c(-1, 0, 1, 2, 3, 4, 5), limits = c(-1.4, 5)) +
  theme_classic(base_size = 14) + 
  labs(y = expression(atop(CO[2]*" "[italic("aq")], "pseudo-log(mmol m"^-2*" d"^-1*")")),  
       x = " ",
       tag = "b") +
  annotate("text", x = min(df_plot_2b$total_density), y = 5, 
           label = paste0("italic(r) == ", round(r_val_2b, 2), 
                          "*','~italic(n) == ", n_val_2b, 
                          "*','~italic(p)[adj] == ", round(p_adj_2b, 2)), 
           parse = TRUE, hjust = 0, size = 4)
vegDensCO2

df_plot_2c <- data %>%
  filter(!is.na(log_flux_ch4), !is.na(total_density))

# Run correlation test
cor_test_2c <- cor.test(  df_plot_2c$total_density, df_plot_2c$log_flux_ch4, method = "pearson")

# Extract values
r_val_2c <- cor_test_2c$estimate
n_val_2c <- nrow(df_plot_2c)
p_adj_2c <- corPadjMatrix["total_density", "log_flux_ch4"]

vegDensCH4 <- ggplot(data = data %>% 
                       filter(!is.na(total_density)), 
                     aes( x = total_density, y = log_flux_ch4)) + 
  geom_point() + 
  geom_smooth(method = "lm", se = F, col = "black", formula = 'y~x')+ 
  #scale_y_continuous(breaks = c(0, 0.5, 1.0, 1.5, 2.0), limits = c(-0.10, 2.1)) +
  scale_y_continuous(breaks = c(-1, 0, 1, 2, 3, 4, 5), limits = c(-0.5, 5)) +
  # scale_y_continuous(breaks=c(0, 1, 10, 100))+ 
  #                    labels = expression("0", "10"^4, "10"^5)) +
  # coord_cartesian(ylim = c(0, 100000))+
  theme_classic(base_size = 14) + 
  labs(y = expression(atop(CH[4]*" "[italic("aq")], "log(mmol m"^-2*" d"^-1*")")),  
       x = " ", #expression(atop("Density of wetland emergent", paste("vegetation (kg m"^-2,")"))), 
       tag = "c") +
  annotate("text", x = min(df_plot_2c$total_density), y = 5, 
           label = paste0("italic(r) == ", round(r_val_2c, 2), 
                          "*','~italic(n) == ", n_val_2c, 
                          "*','~italic(p)[adj] == ", round(p_adj_2c, 2)), 
           parse = TRUE, hjust = 0, size = 4)
vegDensCH4

df_plot_2d <- data %>%
  filter(!is.na(pl_flux_n2o), !is.na(total_density))

# Run correlation test
cor_test_2d <- cor.test(  df_plot_2d$total_density, df_plot_2d$pl_flux_n2o, method = "pearson")

# Extract values
r_val_2d <- cor_test_2d$estimate
n_val_2d <- nrow(df_plot_2d)
p_adj_2d <- corPadjMatrix["total_density", "pl_flux_n2o"]

vegDensN2O <- ggplot(data = data %>% 
                       filter(!is.na(total_density)), 
                     aes( x = total_density, y = pl_flux_n2o)) + 
  geom_point() + 
  scale_y_continuous(breaks = c(-2, -1, 0, 1, 2, 3, 4, 5), limits = c(-2.15, 5.5)) +
  theme_classic(base_size = 14) + 
  labs(y = expression(atop(N[2]*"O "[italic("aq")], "pseudo-log(mmol m"^-2*" d"^-1*")")),  
       x = expression(atop("Density of wetland emergent", paste("vegetation (kg m"^-2,")"))), tag = "d") +
  annotate("text", x = min(df_plot_2d$total_density), y = 5, 
           label = paste0("italic(r) == ", round(r_val_2d, 2), 
                          "*','~italic(n) == ", n_val_2d, 
                          "*','~italic(p)[adj] == ", round(p_adj_2d, 2)), 
           parse = TRUE, hjust = 0, size = 4)
vegDensN2O

df_plot_2e <- data %>%
  filter(!is.na(pl_gwp_sum_g), !is.na(aerial_nep))

# Run correlation test
cor_test_2e <- cor.test(df_plot_2e$aerial_nep, df_plot_2e$pl_gwp_sum_g, method = "pearson")

# Extract values
r_val_2e <- cor_test_2e$estimate
n_val_2e <- nrow(df_plot_2e)
p_adj_2e <- corPadjMatrix["aerial_nep", "pl_gwp_sum_g"]

nepGWP <- ggplot(data = data %>% 
                   filter(!is.na(aerial_nep)), 
                 aes( x = aerial_nep, y = pl_gwp_sum_g)) + 
  geom_point() + 
  scale_y_continuous(breaks = c(0, 1, 2, 3, 4, 5), limits = c(-0.75, 5)) +
  theme_classic(base_size = 14) + 
  labs(y = " ", #expression(atop("Total Instantaneous GHG Flux "[italic("aq")], paste("(mg CO"[2], " m"^-2, " d"^-1, ")"))),  
       x = " ", #expression(atop(paste("NEP"[italic("aq")], " (g O"[2],"m"^-2,"d"^-1,")"), " ")), 
       tag = "e") + 
  ggtitle("Aquatic productivity") +
  theme(plot.title = element_text(hjust = 0.5)) + 
  annotate("text", x = min(df_plot_2e$aerial_nep), y = 5, 
           label = paste0("italic(r) == ", round(r_val_2e, 2), 
                          "*','~italic(n) == ", n_val_2e, 
                          "*','~italic(p)[adj] == ", round(p_adj_2e, 2)), 
           parse = TRUE, hjust = 0, size = 4)
nepGWP

df_plot_2f <- data %>%
  filter(!is.na(pl_flux_co2), !is.na(aerial_nep))

# Run correlation test
cor_test_2f <- cor.test(df_plot_2f$aerial_nep, df_plot_2f$pl_flux_co2, method = "pearson")

# Extract values
r_val_2f <- cor_test_2f$estimate
n_val_2f <- nrow(df_plot_2f)
p_adj_2f <- corPadjMatrix["aerial_nep", "pl_flux_co2"]

nepCO2 <- ggplot(data = df_plot_2f, 
                 aes( x = aerial_nep, y = pl_flux_co2)) + 
  geom_point() + 
  scale_y_continuous(breaks = c(-1, 0, 1, 2, 3, 4, 5), limits = c(-1.4, 5)) +
  theme_classic(base_size = 14) + 
  labs(y = " ", # expression(atop(CO[2]*" "[italic("aq")], "(mmol m"^-2*" d"^-1*")")),  
       x = " ", #expression(atop(paste("NEP"[italic("aq")], " (g O"[2],"m"^-2,"d"^-1,")"), " "))
       tag = "f") + 
  annotate("text", x = min(df_plot_2f$aerial_nep), y = 5, 
           label = paste0("italic(r) == ", round(r_val_2f, 2), 
                          "*','~italic(n) == ", n_val_2f, 
                          "*','~italic(p)[adj] == ", round(p_adj_2f, 2)), 
           parse = TRUE, hjust = 0, size = 4)
nepCO2

df_plot_2g <- data %>%
  filter(!is.na(log_flux_ch4), !is.na(aerial_nep))

# Run correlation test
cor_test_2g <- cor.test(df_plot_2g$aerial_nep, df_plot_2g$log_flux_ch4, method = "pearson")

# Extract values
r_val_2g <- cor_test_2g$estimate
n_val_2g <- nrow(df_plot_2g)
p_adj_2g <- corPadjMatrix["aerial_nep", "log_flux_ch4"]

nepCH4 <- ggplot(data = df_plot_2g, 
                 aes( x = aerial_nep, y = log_flux_ch4)) + 
  geom_point() + 
  geom_smooth(method = "lm", se = F, col = "black", formula = 'y~x')+ 
  scale_y_continuous(breaks = c(-1, 0, 1, 2, 3, 4, 5), limits = c(-1.4, 5)) +
  theme_classic(base_size = 14) + 
  labs(y = " ",  #expression(atop(CH[4]*" "[italic("aq")], "(mmol m"^-2*" d"^-1*")"))
       x = " ", #expression(atop(paste("NEP"[italic("aq")], " (g O"[2],"m"^-2,"d"^-1,")"), " ")), 
       tag = "g") + 
  annotate("text", x = min(df_plot_2g$aerial_nep), y = 5, 
           label = paste0("italic(r) == ", round(r_val_2g, 2), 
                          "*','~italic(n) == ", n_val_2g, 
                          "*','~italic(p)[adj] == ", round(p_adj_2g, 2)), 
           parse = TRUE, hjust = 0, size = 4)
nepCH4

df_plot_2h <- data %>%
  filter(!is.na(pl_flux_n2o), !is.na(aerial_nep))

# Run correlation test
cor_test_2h <- cor.test(df_plot_2h$aerial_nep, df_plot_2h$pl_flux_n2o, method = "pearson")

# Extract values
r_val_2h <- cor_test_2h$estimate
n_val_2h <- nrow(df_plot_2h)
p_adj_2h <- corPadjMatrix["aerial_nep", "pl_flux_n2o"]

nepN2O <- ggplot(data = df_plot_2h, 
                 aes( x = aerial_nep, y = pl_flux_n2o)) + 
  geom_point() +
  scale_y_continuous(breaks = c(-2, -1, 0, 1, 2, 3, 4, 5), limits = c(-2.15, 5.5)) +
  theme_classic(base_size = 14) + 
  labs(y = " ", #expression(atop(N[2]*"O "[italic("aq")], "(mmol m"^-2*" d"^-1*")")),  
       x = expression(atop(paste("NEP"[italic("aq")], " (g O"[2]," m"^-2,"d"^-1,")"), " ")), tag = "h") + 
  annotate("text", x = min(df_plot_2h$aerial_nep), y = 5, 
           label = paste0("italic(r) == ", round(r_val_2h, 2), 
                          "*','~italic(n) == ", n_val_2h, 
                          "*','~italic(p)[adj] == ", round(p_adj_2h, 2)), 
           parse = TRUE, hjust = 0, size = 4)
nepN2O 

fig2 <- grid.arrange(vegDensGWP, nepGWP, vegDensCO2, nepCO2, vegDensCH4, nepCH4, vegDensN2O, nepN2O, nrow = 4)

# Save
ggsave(fig2, file = paste0("Outputs/figure2_", Sys.Date(), ".png"), width = 9, height = 14)

# 5. Figure 3 -----------------------------------------------------

df_plot_3a <- data %>%
  filter(!is.na(pl_gwp_sum_g), !is.na(wetlandBirdRichness))

# Run correlation test
cor_test_3a <- cor.test(df_plot_3a$wetlandBirdRichness, df_plot_3a$pl_gwp_sum_g, method = "pearson")

# Extract values
r_val_3a <- cor_test_3a$estimate
n_val_3a <- nrow(df_plot_3a)
p_adj_3a <- corPadjMatrix[ "pl_gwp_sum_g", "wetlandBirdRichness"]

richnessGWP <- ggplot(data = df_plot_3a, 
                     aes(x = wetlandBirdRichness, y = pl_gwp_sum_g)) + 
  geom_point() + 
  geom_smooth(method = "lm", se = F, col = "black", formula = 'y~x') + 
  theme_classic(base_size = 14) + 
  scale_x_continuous(breaks = c(5, 10, 15, 20, 25, 30), limits = c(5, 30)) +
  scale_y_continuous(breaks = c(0, 1, 2, 3, 4, 5), limits = c(-0.75, 5)) +
  labs(y = expression(atop("Total Instantaneous GHG Flux "[italic("aq")], paste("pseudo-log(g CO"[2], " m"^-2, " d"^-1, ")"))),  
       x = "Wetland bird richness", tag = "a") + 
  annotate("text", x = min(df_plot_3a$wetlandBirdRichness), y = 5, 
           label = paste0("italic(r) == ", round(r_val_3a, 2), 
                          "*','~italic(n) == ", n_val_3a, 
                          "*','~italic(p)[adj] == ", round(p_adj_3a, 2)), 
           parse = TRUE, hjust = 0, size = 4)
richnessGWP

df_plot_3b <- data %>%
  filter(!is.na(pl_flux_co2), !is.na(wetlandBirdRichness))

# Run correlation test
cor_test_3b <- cor.test(df_plot_3b$wetlandBirdRichness, df_plot_3b$pl_flux_co2, method = "pearson")

# Extract values
r_val_3b <- cor_test_3b$estimate
n_val_3b <- nrow(df_plot_3b)
p_adj_3b <- corPadjMatrix["pl_flux_co2", "wetlandBirdRichness"]

richnessCO2 <- ggplot(data = df_plot_3b, 
                      aes( x = wetlandBirdRichness, y = pl_flux_co2)) + 
  geom_point() + 
  geom_smooth(method = "lm", se = F, col = "black")+ 
  theme_classic(base_size = 14) + 
  scale_x_continuous(breaks = c(5, 10, 15, 20, 25, 30), limits = c(5, 30)) +
  scale_y_continuous(breaks = c(-1, 0, 1, 2, 3, 4, 5), limits = c(-1.4, 5)) +
  labs(y = expression(atop(CO[2]*" "[italic("aq")], "pseudo-log(mmol m"^-2*" d"^-1*")")),  
       x = "Wetland bird richness", tag = "b") + 
  annotate("text", x = min(df_plot_3b$wetlandBirdRichness), y = 5, 
           label = paste0("italic(r) == ", round(r_val_3b, 2), 
                          "*','~italic(n) == ", n_val_3b, 
                          "*','~italic(p)[adj] == ", round(p_adj_3b, 2)), 
           parse = TRUE, hjust = 0, size = 4)
richnessCO2

df_plot_3c <- data %>%
  filter(!is.na(log_flux_ch4), !is.na(wetlandBirdRichness))

# Run correlation test
cor_test_3c <- cor.test(df_plot_3c$wetlandBirdRichness, df_plot_3c$log_flux_ch4, method = "pearson")

# Extract values
r_val_3c <- cor_test_3c$estimate
n_val_3c <- nrow(df_plot_3c)
p_adj_3c <- corPadjMatrix["log_flux_ch4", "wetlandBirdRichness"]

richnessCH4 <- ggplot(data = df_plot_3c, 
                      aes( x = wetlandBirdRichness, y = log_flux_ch4)) + 
  geom_point() + 
  theme_classic(base_size = 14) + 
  scale_x_continuous(breaks = c(5, 10, 15, 20, 25, 30), limits = c(5, 30)) +
  scale_y_continuous(breaks = c(-1, 0, 1, 2, 3, 4, 5), limits = c(-1.4, 5)) +
  labs(y = expression(atop(CH[4]*" "[italic("aq")], "log(mmol m"^-2*" d"^-1*")")),  
       x = "Wetland bird richness", tag = "c") + 
  annotate("text", x = min(df_plot_3c$wetlandBirdRichness), y = 5, 
           label = paste0("italic(r) == ", round(r_val_3c, 2), 
                          "*','~italic(n) == ", n_val_3c, 
                          "*','~italic(p)[adj] == ", round(p_adj_3c, 2)), 
           parse = TRUE, hjust = 0, size = 4)
richnessCH4

df_plot_3d <- data %>%
  filter(!is.na(pl_flux_n2o), !is.na(wetlandBirdRichness))

# Run correlation test
cor_test_3d <- cor.test(df_plot_3d$wetlandBirdRichness, df_plot_3d$pl_flux_n2o, method = "pearson")

# Extract values
r_val_3d <- cor_test_3d$estimate
n_val_3d <- nrow(df_plot_3d)
p_adj_3d <- corPadjMatrix["pl_flux_n2o", "wetlandBirdRichness"]

richnessN2O <- ggplot(data = df_plot_3d, 
                      aes(x = wetlandBirdRichness, y = pl_flux_n2o)) + 
  geom_point() + 
  scale_x_continuous(breaks = c(5, 10, 15, 20, 25, 30), limits = c(5, 30)) +
  scale_y_continuous(breaks = c(-2, -1, 0, 1, 2, 3, 4, 5), limits = c(-2.15, 5.5)) +
  theme_classic(base_size = 14) + 
  labs(y = expression(atop(N[2]*"O "[italic("aq")], "pseudo-log(mmol m"^-2*" d"^-1*")")),  
       x = "Wetland bird richness", tag = "d") + 
  annotate("text", x = min(df_plot_3a$wetlandBirdRichness), y = 5, 
           label = paste0("italic(r) == ", round(r_val_3d, 2), 
                          "*','~italic(n) == ", n_val_3d, 
                          "*','~italic(p)[adj] == ", round(p_adj_3d, 2)), 
           parse = TRUE, hjust = 0, size = 4)
richnessN2O


fig3 <- grid.arrange(richnessGWP, richnessCO2, richnessCH4, richnessN2O, nrow = 2)

# Save
ggsave(fig3, file = paste0("Outputs/figure3_", Sys.Date(), ".png"), 
       width = 10, height = 8)

# 6. Figure 4 ------------------------------------------------------------------

df_plot_4a <- data %>%
  filter(!is.na(total_density), !is.na(wetlandArea_500m))

# Run correlation test
cor_test_4a <- cor.test(  df_plot_4a$wetlandArea_500m, df_plot_4a$total_density, method = "pearson")

# Extract values
r_val_4a <- cor_test_4a$estimate
n_val_4a <- nrow(df_plot_4a)
p_adj_4a <- corPadjMatrix["total_density", "wetlandArea_500m"]

wetlandAreaVegDens <- ggplot(data = df_plot_4a, 
                             aes(x = wetlandArea_500m/1000000, y = total_density )) + 
  geom_point() + 
  scale_x_continuous(breaks = c(0.0, 0.05, 0.10, 0.15, 0.20),
                     limits = c(0, 0.2)) +
  scale_y_continuous(breaks = c(0.0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6),
                     limits = c(0, 0.6)) +
  theme_classic(base_size = 14) + 
  ylab("Density of wetland emergent<br>vegetation (kg m<sup>-2</sup>)") +
  xlab("") +
  theme(axis.title.y = element_markdown(), 
        axis.title.x = element_markdown()) +
  labs(tag = "a") + 
  annotate("text", x = min(df_plot_4a$wetlandArea_500m/1000000), y = 0.6, 
           label = paste0("italic(r) == ", round(r_val_4a, 2), 
                          "*','~italic(n) == ", n_val_4a, 
                          "*','~italic(p)[adj] == ", round(p_adj_4a, 2)), 
           parse = TRUE, hjust = 0, size = 4)
wetlandAreaVegDens

df_plot_4b <- data %>%
  filter(!is.na(pl_gwp_sum_g), !is.na(wetlandArea_500m))

# Run correlation test
cor_test_4b <- cor.test(df_plot_4b$wetlandArea_500m, df_plot_4b$pl_gwp_sum_g, method = "pearson")

# Extract values
r_val_4b <- cor_test_4b$estimate
n_val_4b <- nrow(df_plot_4b)
p_adj_4b <- corPadjMatrix["pl_gwp_sum_g", "wetlandArea_500m"]

wetlandAreaGWP <- ggplot(data = df_plot_4b, 
                         aes(x = wetlandArea_500m/1000000, y = pl_gwp_sum_g )) + 
  geom_point() + 
  scale_x_continuous(breaks = c(0.0, 0.10, 0.20, 0.30, 0.40),
                     limits = c(0, 0.42)) +
  scale_y_continuous(breaks = c(0, 1, 2, 3, 4, 5), limits = c(-0.75, 5)) +
  theme_classic(base_size = 14) + 
  labs(y = expression(atop("Total Instantaneous GHG Flux "[italic("aq")], paste("pseudo-log(g CO"[2], " m"^-2, " d"^-1, ")"))),
       x = "", tag = "b")+ #x = expression(paste("Wetland area (km"^2,") within 500m")) 
annotate("text", x = min(df_plot_4b$wetlandArea_500m/1000000), y = 5, 
         label = paste0("italic(r) == ", round(r_val_4b, 2), 
                        "*','~italic(n) == ", n_val_4b, 
                        "*','~italic(p)[adj] == ", round(p_adj_4b, 2)), 
         parse = TRUE, hjust = 0, size = 4)
wetlandAreaGWP

df_plot_4c <- data %>%
  filter(!is.na(wetlandArea_500m), !is.na(wetlandBirdRichness))

# Run correlation test
cor_test_4c <- cor.test(  df_plot_4c$wetlandArea_500m, df_plot_4c$wetlandBirdRichness, method = "pearson")

# Extract values
r_val_4c <- cor_test_4c$estimate
n_val_4c <- nrow(df_plot_4c)
p_adj_4c <- corPadjMatrix[ "wetlandBirdRichness", "wetlandArea_500m"]

wetlandAreaRichness_500m2 <- ggplot(data = df_plot_4c, 
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
       tag = "c") + 
  annotate("text", x = min(df_plot_4c$wetlandArea_500m/1000000), y = 30, 
           label = paste0("italic(r) == ", round(r_val_4c, 2), 
                          "*','~italic(n) == ", n_val_4c, 
                          "*','~italic(p)[adj] == ", round(p_adj_4c, 3)), 
           parse = TRUE, hjust = 0, size = 4)
wetlandAreaRichness_500m2

fig4 <- grid.arrange(wetlandAreaVegDens, wetlandAreaGWP, wetlandAreaRichness_500m2)

# Save
ggsave(fig4, file = paste0("Outputs/figure4_", Sys.Date(), ".png"), 
       width = 4, height = 10)

# 7. Figure S3: richness vs wetland area - Confirmed Richness -----------------------------

df_plot_s3 <- data %>%
  filter(!is.na(wetlandBirdRichness)) %>%
  mutate(wetlandArea_250m = wetlandArea_250m/1000000,
         wetlandArea_500m = wetlandArea_500m/1000000,
         wetlandArea_1km = wetlandArea_1km/1000000,
         wetlandArea_5k0m = wetlandArea_5km/1000000)

wetlandArea_250m_lm <- lm(wetlandBirdRichness~wetlandArea_250m,
                          data = df_plot_s3)

r2_val_s3a <- summary(wetlandArea_250m_lm)$r.squared
n_val_s3a <- nrow(df_plot_s3)
p_val_s3a <- pf(summary(wetlandArea_250m_lm)$fstatistic[1],
                summary(wetlandArea_250m_lm)$fstatistic[2],
                summary(wetlandArea_250m_lm)$fstatistic[3],
                lower.tail = FALSE)

richnessWetlandAreaPlot_250m2 <- ggplot(data = df_plot_s3, 
                                        aes(x = wetlandArea_250m, y =  wetlandBirdRichness)) + 
  geom_point() + 
  geom_smooth(method = "lm", col = "black", se = F, formula = 'y~x')+
  theme_classic(base_size = 16) + 
  scale_y_continuous(breaks = c(0, 5, 10, 15, 20, 25, 30), limits = c(0, 30)) +
  labs(y = "Wetland bird richness", x = expression(paste("Wetland area (km"^2,") ", "within 250m")), tag = "a") + 
  annotate("text", x = min(df_plot_s3$wetlandArea_250m), y = 30, 
           label = paste0("italic(R^2) == ", round(r2_val_s3a, 2), 
                          "*','~italic(n) == ", n_val_s3a, 
                          "*','~italic(p) == ", round(p_val_s3a, 2)), 
           parse = TRUE, hjust = 0, size = 4)
richnessWetlandAreaPlot_250m2

wetlandArea_500m_lm <- lm(wetlandBirdRichness~wetlandArea_500m,
                          data = df_plot_s3)

r2_val_s3b <- summary(wetlandArea_500m_lm)$r.squared
n_val_s3b <- nrow(df_plot_s3)
p_val_s3b <- pf(summary(wetlandArea_500m_lm)$fstatistic[1],
                summary(wetlandArea_500m_lm)$fstatistic[2],
                summary(wetlandArea_500m_lm)$fstatistic[3],
                lower.tail = FALSE)
# p_val_s3b < 0.001
p_val_s3b <- 0.001

richnessWetlandAreaPlot_500m2 <- ggplot(data = df_plot_s3, 
                                        aes(x = wetlandArea_500m, y =  wetlandBirdRichness)) + 
  geom_point() + 
  geom_smooth(method = "lm", col = "black", se = F,  formula = 'y~x')+
  theme_classic(base_size = 16) + 
  scale_y_continuous(breaks = c(0, 5, 10, 15, 20, 25, 30), limits = c(0, 30)) +
  labs(y = "Wetland bird richness", x = expression(paste("Wetland area (km"^2,") ", "within 500m")), tag = "b")+ 
  annotate("text", x = min(df_plot_s3$wetlandArea_250m), y = 30, 
           label = paste0("italic(R^2) == ", round(r2_val_s3b, 2), 
                          "*','~italic(n) == ", n_val_s3b, 
                          "*','~italic(p) < ", round(p_val_s3b, 3)), 
           parse = TRUE, hjust = 0, size = 4)
richnessWetlandAreaPlot_500m2

wetlandArea_1km_lm <- lm(wetlandBirdRichness~wetlandArea_1km,
                          data = df_plot_s3)

r2_val_s3c <- summary(wetlandArea_1km_lm)$r.squared
n_val_s3c <- nrow(df_plot_s3)
p_val_s3c <- pf(summary(wetlandArea_1km_lm)$fstatistic[1],
                summary(wetlandArea_1km_lm)$fstatistic[2],
                summary(wetlandArea_1km_lm)$fstatistic[3],
                lower.tail = FALSE)

richnessWetlandAreaPlot_1km2 <- ggplot(data = df_plot_s3, 
                                       aes(x = wetlandArea_1km, y =  wetlandBirdRichness)) + 
  geom_point() + 
  geom_smooth(method = "lm", col = "black", se = F, formula = 'y~x')+
  theme_classic(base_size = 16) + 
  scale_y_continuous(breaks = c(0, 5, 10, 15, 20, 25, 30), limits = c(0, 30)) +
  labs(y = "Wetland bird richness", x = expression(paste("Wetland area (km"^2,") ", "within 1km")), tag = "c") + 
  annotate("text", x = min(df_plot_s3$wetlandArea_1km), y = 30, 
           label = paste0("italic(R^2) == ", round(r2_val_s3c, 2), 
                          "*','~italic(n) == ", n_val_s3c, 
                          "*','~italic(p) == ", round(p_val_s3c, 2)), 
           parse = TRUE, hjust = 0, size = 4)
richnessWetlandAreaPlot_1km2

wetlandArea_5km_lm <- lm(wetlandBirdRichness~wetlandArea_5km,
                         data = df_plot_s3)

r2_val_s3d <- summary(wetlandArea_5km_lm)$r.squared
# r2 < 0.01
r2_val_s3d <- 0.01
n_val_s3d <- nrow(df_plot_s3)
p_val_s3d <- pf(summary(wetlandArea_5km_lm)$fstatistic[1],
                summary(wetlandArea_5km_lm)$fstatistic[2],
                summary(wetlandArea_5km_lm)$fstatistic[3],
                lower.tail = FALSE)

richnessWetlandAreaPlot_5km2 <- ggplot(data = df_plot_s3,
                                       aes( x = wetlandArea_5km, y =  wetlandBirdRichness)) + 
  geom_point() + 
  theme_classic(base_size = 16) + 
  scale_y_continuous(breaks = c(0, 5, 10, 15, 20, 25, 30), limits = c(0, 30)) +
  labs(y = "Wetland bird richness", x = expression(paste("Wetland area (km"^2,") ", "within 5km")), tag = "d")+ 
  # stat_poly_eq(use_label(c("R2", "P", "n")), size = rel(4)) +
  annotate("text", x = min(df_plot_s3$wetlandArea_5km), y = 30, 
           label = paste0("italic(R^2) < ", round(r2_val_s3d, 2), 
                          "*','~italic(n) == ", n_val_s3d, 
                          "*','~italic(p) == ", round(p_val_s3d, 2)), 
           parse = TRUE, hjust = 0, size = 4)
richnessWetlandAreaPlot_5km2

figS3_richnessWetlandArea <- grid.arrange(richnessWetlandAreaPlot_250m2, richnessWetlandAreaPlot_500m2, 
                                    richnessWetlandAreaPlot_1km2, richnessWetlandAreaPlot_5km2)

# Save
ggsave(figS3_richnessWetlandArea, 
       file = "Outputs/figureS3_richnessWetlandArea.png", 
       width = 10, height = 7) 

# 8. Fig S4. density of wetland emergent vegetation vs NEP ------------
df_plot_S4 <- data %>%
  filter(!is.na(total_density), !is.na(aerial_nep)) 
  

# Run correlation test
cor_test_S4 <- cor.test(  df_plot_S4$total_density, df_plot_S4$aerial_nep, method = "pearson")

# Extract values
r_val_S4 <- cor_test_S4$estimate
n_val_S4 <- nrow(df_plot_S4)
p_adj_S4 <- corPadjMatrix[  "total_density", "aerial_nep"]

FigS4 <- ggplot(data = df_plot_S4, 
                                    aes( x = total_density, y = aerial_nep)) + 
  geom_point() + 
  theme_classic(base_size = 14) + 
  scale_y_continuous(breaks = c(-30, -20, -10, 0, 10, 20), limits = c(-35, 20)) + 
  scale_x_continuous(breaks = c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6), limits = c(0, 0.6)) +
  labs(y = expression(paste("NEP"[italic("aq")], " (g O"[2],"m"^-2,"d"^-1,")")), 
       x = expression(atop("Density of wetland emergent", paste("vegetation (kg m"^-2,")")))) + 
  annotate("text", x = min(df_plot_S4$total_density), y = 18, 
           label = paste0("italic(r) == ", round(r_val_S4, 2), 
                          "*','~italic(n) == ", n_val_S4, 
                          "*','~italic(p)[adj] == ", round(p_adj_S4, 3)), 
           parse = TRUE, hjust = 0, size = 4)
FigS4

ggsave(FigS4, file = paste0("Outputs/figureS4_", Sys.Date(), ".png"), 
       width = 5, height = 4)


# 9. Table S1: Summary of wetland characteristics -------------------------------------------------------

summaryVariables <- data %>% 
  select(depth_filled, Area.Ha, total_em_area_m2, total_density, aerial_gpp, aerial_r, aerial_nep,  
         flux_co2, flux_ch4, flux_n2o, gwp_sum_g, wetlandBirdRichness, wetlandArea_500m)

summary_table <- data.frame(
  Variable = names(summaryVariables),
  Mean = sapply(summaryVariables, function(x) round(mean(x, na.rm = TRUE), 2)),
  Median = sapply(summaryVariables, function(x) round(median(x, na.rm = TRUE), 2)),
  SD = sapply(summaryVariables, function(x) round(sd(x, na.rm = TRUE), 2)),
  Min = sapply(summaryVariables, function(x) round(min(x, na.rm = TRUE), 2)),
  Max = sapply(summaryVariables, function(x) round(max(x, na.rm = TRUE), 2)),
  N = sapply(summaryVariables, function(x) sum(!is.na(x)))
)

print(summary_table)

# Write table
write.csv(summary_table,
          paste0("Outputs/tableS1_dataSummary_", Sys.Date(), ".csv"))

## THE END :)