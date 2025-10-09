## Summary

This repository contains the code and data for analyses for "Synergies, not trade-offs: Greenhouse gas emissions are inversely related to bird biodiversity along productivity gradients in prairie wetlands" by Woodman et al.

## Contents:
* `01.1_wetlandBenefitsTradeoffs_BirdNetDataOrg.R` combines and cleans bird detection data from BirdNet and human listening
* `01.2_wetlandBenefitsTradeoffs_manualListeningPrep.R` uses bird detection data from BirdNet to prepare human listening verification files
* `01.3_wetlandBenefitsTradeoffs_dataPrep.R` uses cleaned bird detection data to estimate wetland bird species richness per site, measure surrounding wetland area, and save final data file for analyses
* `02.1_wetlandBenefitsTradeoffs_analyses.R`analyzes data and creates figures and tables from the manuscript.
* `wetland_tradeoff_df_2025-10-09.csv` contains the data used in the analyses (in 02.1)

## Notes

Due to file sizes, data sensitivity, and data sharing agreement requirements, we have excluded the raw data files with bird detections and surrounding wetland area from the Canadian Wetland Inventory. However, we included the code used to create the final data set.