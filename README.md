## Data-driven identification of hub chains in disease accruals
This repository contains the implementation of a nested k-means trajectory clustering method for analyzing disease progression patterns. The method groups disease accrual processes and identifies critical *hub chains* - key directional disease pairs that drive the progression of multimorbidity and healthcare resource utilization.

Associated paper: Han, S., Zhou, C., Li, S. et al. Data-driven identification of hub chains in disease accruals to reduce healthcare utilization and costs. 

# Methodological overview

The analytical pipeline comprises five key stages:

1. Directional disease pair identification: Quantifies directional associations between disease pairs using a matched relative risk approach

2. Statistical significance testing: Assesses the significance of disease chains via binomial testing

3. Trajectory construction: Builds disease trajectories as sequences of three distinct diseases (D₁ → D₂ → D₃)

4. Healthcare utilization analysis: Implements longitudinal tracking of healthcare costs and utilization along trajectories

5. Trajectory clustering: Applies nested k-means clustering to identify hub chains and disease progression patterns

# Prerequisites
## Prerequisite software 
* R version 4.3.0.
## Prerequisite third-party R packages
Data manipulation and analysis: dplyr,readxl,readr,tidyr,reshape2,stringr, psych, mlbench, RcmdrMisc, openxlsx

Visualization: ggplot2,cowplot, RColorBrewer,ggrepel, ggraph, grid, gridExtra,circlize,dendextend

Graph and network analysis: igraph,tidygraph

Parallel computing: Parallel, MCL

Meta-packages and development:tidyverse,devtools

# Descriptions of the files

In the "main analysis" subfolder: 
1.identification_of_directional_disease_pairs.R includes the code to quantify the directional association between two diseases.The strength of this association was measured using a matched relative risk (RR) approach.

2.directionality_testing.R includes the code to assess the statistical significance of each chain (D1 to D2) with RR >1 using a binomial test. 

3.construction_of_disease_accrual_trajectories.R includes the code to construct disease trajectories as sequences of three distinct diseases (D1 to D2 to D3).

4.healthcare_utilization_and_costs_of_trajectories includes the code for the longitudinal approach to track healthcare utilization and costs across the multimorbidity trajectory. 

5.trajectory_clustering.R includes the code for the nested k-means clustering approach to group the identified trajectories.

The "source data" subfolder includes the source data for the plots, figures 1-5.
The "source plot" subfolder includes the plotting code for figures 1-5.

