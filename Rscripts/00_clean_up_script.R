# SCRIPT TO CLEAN YOUR ENVIRONMENT
# This script should be used after every run of a script to keep the environment clean and your computer fast.

# Clear plots
if(!is.null(dev.list())) dev.off()
# Clean workspace
rm(list=ls())
# Clear console
cat("\014") 
