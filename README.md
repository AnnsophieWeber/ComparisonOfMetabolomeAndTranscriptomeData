# Comparison Of Metabolome And Transcriptome Time Series Data

This repository contains the R scripts, which were used to generate the results of a transcriptome and volatilome time series study of *Cyclocybe aegerita*. The scripts were universally written, so they can be used for other organisms and time series with a differing number of time points and replicates.

The R scripts allow the user to perform a differential gene expression (DE) analysis, a principal component analysis (PCA) on the transcriptome as well as on the metabolome. Additionally, they facilitate the comparison of both data sets by calculating correlation coefficients and plotting time series of substance concentrations and transcript counts combined in one plot. Furthermore, selected genes and substances can be chosen for subsequent and more detailed analyses to identify potential biosynthetic pathways. Using the new experimental setup and applying the newly developed scripts can help to better understand the expression of genes and production of substances over time. Moreover, the results potentially allow identification of new biosynthetic pathways.

## The scripts

* **00_clean_up_script.R** <br/>- a script to clean up your environment and make some space on your memory after extensive calculations
* **00_installation_script.R** <br/>- a script to help you install all required packages
* **1.1_differential_gene_expression_analysis.R** <br/>- a script to perform a differential gene expression for transcriptome time series data
* **1.2_normalisation_of_substances.R** <br/>- a script to normalise the concentration (=peak areas) of the substance time series data
* **2.1_subsequent_differential_gene_expression_analysis.R** <br/>- a script to gain more insight on the expression of specific genes of interest
* **2.2_venn_diagrams.R** <br/>- a script to compare different tissues on their differential expression states
* **2.3_principal_component_analysis.R** <br/>- a script to perform a PCA and find similarities or differences between the time points
* **2.4_correlation.R** <br/>- a script to calculate the correlation between transcriptome and metabolome
* **2.5_substance_transcript_time_series_plots.R** <br/>- a script to plot transcript counts and substance concentrations in one plot
* **3.1_subsequent_correlation_analysis.R** <br/>- a script to gain more insight on coherences between specific genes or substances of interest
* **friedman_script_source.R** <br/>- a script containing the code for the Friedman and Wilcoxon-Nemenyi-McDonald-Thompson test, originally implemented by [Galili, T. (2010)](https://www.r-statistics.com/2010/02/post-hoc-analysis-for-friedmans-test-r-code/), retrieved May 28, 2019

## How To
All analyses were performed and scripts were implemented in R, version 3.6.0 (2019-04-26), Planting of a tree. The functionality and compatibility of the scripts with newer R versions has not been checked.

### Input
The scripts should be executed in RStudio and require sepcific .csv and .xlsx files as input. The order in which the scripts should be executed is visualised in the flowchart diagram (Analysis_Flowchart_Diagram.pdf).<br/>
Input files and parameters that need to be provided by the user in each script are explained in the comments within the scripts and additionally in the file "How_To.pdf".<br/>
Furthermore, examples on the structure of the input files for the transcriptome and volatilome/metabolome time series are provided in this repository as well (example_transcriptome_replicate_1.csv, example_volatilome-metabolome_replicate_1.xlsx).

### Required packages
The following packages are required and can be installed with a R script, which can be found amongst the scripts for the analysis:

* circlize, R package, version 0.4.6
* ComplexHeatmaps, R package, version 2.0.0
* dplyr, R package, version 0.8.0.1
* Friedman test with Wilcoxon-Nemenyi-McDonald-Thompson test, Source, Galili, 2010
* ggplot2, R package, version 3.1.1
* ImpulseDE2, R package, version 1.8.0
* readxl/xlsx, R packages, readxl: version 1.3.1, xlsx: version 0.6.1
* reshape2, R package, version 1.4.3
* VennDiagram, R package, version 1.6.20

The functionality and compatibility with newer versions of the packages has not been checked.




### Author
Annsophie Weber

### Important information:
These scripts are currently not maintained.
