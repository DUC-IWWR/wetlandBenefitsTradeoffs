This repository contains the code and data for analyses of prairie pothole wetland benefits and tradeoffs by Woodman et al.

Contents:
* `01.1_wetlandBenefitsTradeoffs_BirdNetDataOrg.R` combines and cleans bird detection data from BirdNet and human listening
* `01.2_wetlandBenefitsTradeoffs_manualListeningPrep.R` uses bird detection data from BirdNet to prepare human listening verification files
* `01.3_wetlandBenefitsTradeoffs_dataPrep.R` uses cleaned bird detection data to estimate wetland bird species richness per site, measure surrounding wetland area, and save final data file for analyses
* `01.4_wetlandBenefitsTradeoffs_fluxCorrectionCheck.R` compares original data to updated flux data with correlation plots
* `02.1_wetlandBenefitsTradeoffs_analyses.R`analyzes data and creates figures from manuscript.
* wetland_tradeoff_df_2025-05-02.csv contains the data used in the analyses
* wetland_tradeoff_df_2025-07-28.xlsx contains updated data
