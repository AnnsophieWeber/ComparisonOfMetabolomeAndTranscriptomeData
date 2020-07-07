# SCRIPT FOR INSTALLING REQUIRED PACKAGES
# Run this ONLY ONCE after you installed R and before your first use of the scripts.

# cicrlize
install.packages("circlize")

# ComplexHeatmap
if (!requireNamespace("BiocManager", quietly = TRUE))
  install.packages("BiocManager")
BiocManager::install("ComplexHeatmap")

# dplyr
install.packages("dplyr")

# ggplot2
install.packages("ggplot2")

# ImpulseDE2
if (!requireNamespace("BiocManager", quietly = TRUE))
  install.packages("BiocManager")
BiocManager::install("ImpulseDE2")

# readxl
install.packages("readxl")

# reshape2
install.packages("reshape2")

# VennDiagram
install.packages("VennDiagram")

# xlsx
install.packages("xlsx")
install.packages("readxl")


















