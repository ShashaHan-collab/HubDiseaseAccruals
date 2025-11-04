# Data-driven identification of hub chains in disease accruals
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
## Software requirements 
* R version 4.3.0.
## Prerequisite third-party R packages
Data manipulation and analysis: dplyr,readxl,readr,tidyr,reshape2,stringr, psych, mlbench, RcmdrMisc, openxlsx

Visualization: ggplot2,cowplot, RColorBrewer,ggrepel, ggraph, grid, gridExtra,circlize,dendextend

Graph and network analysis: igraph,tidygraph

Parallel computing: Parallel, MCL

Meta-packages and development:tidyverse,devtools

# Repository Structure

📁 *main_analysis/*
Core analytical scripts implementing the methodological pipeline:

identification_of_directional_disease_pairs.R
Implements the matched relative risk approach to quantify directional disease associations

directionality_testing.R
Performs binomial testing for statistically significant disease chains (RR > 1)

construction_of_disease_accrual_trajectories.R
Constructs three-disease sequences (D₁ → D₂ → D₃) from significant disease pairs

healthcare_utilization_and_costs_of_trajectories.R
Tracks longitudinal healthcare utilization and costs across multimorbidity trajectories

trajectory_clustering.R
Implements nested k-means clustering to identify hub chains and group disease progression patterns

📁 *source_data/*
Contains all source data required to reproduce Figures 1-5 from the manuscript.

📁 *source_plot/*
Includes complete plotting code for generating Figures 1-5 as presented in the manuscript.

# Usage
Execute scripts in numerical order within the main_analysis/ directory to reproduce the complete analytical pipeline:

1. Start with directional pair identification

2. Proceed to significance testing

3. Construct disease trajectories

4. Analyze healthcare utilization

5. Perform trajectory clustering

# Citation
If you use this code in your research, please cite our publication.


