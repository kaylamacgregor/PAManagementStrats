# PAManagementStrats

This repository contains all R scripts used for my EES undergraduate dissertation project, which investigates how protected area (PA) management strategies influence conservation outcomes for waterbirds across sites in Scotland.

The analysis involves spatial analysis, cumulative link mixed models (CLMMs), post-hoc comparisons, and data visualisation.

## Contents

- `01_data_preprocessing.R`: Cleans and prepares the raw dataset for analysis.
- `02_spatial_analysis.R`: Tests for spatial autocorrelation in site characteristics and conservation outcomes.
- `03_modelling.R`: Fits CLMMs to model conservation impact as an ordinal response, including model selection and interaction effects.
- `04.1_exploratory_plots.R`: Generates initial plots to visualise patterns in the data.
- `04.2_posthoc_and_plots.R`: Conducts post-hoc comparisons and visualises predicted probabilities from the models.
- `05_mapping.R`: Creates maps of the study sites and their management objectives using geospatial data.

## Notes

- All scripts are written in R.
- To replicate the full analysis, run the scripts** **in numerical order**, starting from `01_data_preprocessing.R` through to `05_mapping.R`.
- The conservation impact data for waterbirds is sourced from **Wauchope et al. (2022)**.
- All management strategies have been sourced from publically available management plans. 
