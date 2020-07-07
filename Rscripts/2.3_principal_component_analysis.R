###########################################################################################################################################################################
# PRINCIPAL COMPONENT ANALYSIS, FRIEDMAN TEST AND FRIEDMAN POST HOC TEST
# Script to do a Principal Component Ananlysis (PCA) for a time series.
# This script is usable for substance concentrations or transcript counts.
# To do the analysis, you need a .csv file with means for each time point as input. 
# This script will produce 
#     - a PCA plot
#     - a scree plot
#     - OPTIONAL: two files with the genes/substances that have the largest effect on the first two principal compontents (PC)
#     - a file containing the Friedman test result
#     - a file containing the Friedman post hoc test results
###########################################################################################################################################################################
      start_time <- Sys.time()

# 1)  SET YOU WORKING DIRECTORY. This is where your input file is located and the results will be stored.
#     Click in the menubar on the tab "Session" > "Set Working Directory" > "Choose Directory..."
#     Then choose the directory where your input file (the one with the means) is located and the results will be stored.



# 2)  READ/IMPORT your file into RStudio. The file should contain the genes/substances as rows with the timepoints as columns and has to be .csv format.
#     Watch out! The row and column names should not contain any special characters!

      input_file <- read.csv("your_means.csv") # Type in here the name of the file
      
      
      
# 3)  SPECIFY your data information:
            
      # How should the timepoints be called? This names will be shown in the PCA plot.
      TimeCateg <- c("Timepoint1", "Timepoint2", "Timepoint3", "Timepoint4", "Timepoint5")
      
    
      
# 4) DEFINE how many genes/substances you want to see that are having the largest effect on PC1 and PC2.
#    If you don't want to use this option, write NULL instead of a number.
      
     largest_effect_on_PC <- NULL
      
      
      
# 5) DEFINE font sizes and colours for the produced plots.
#    If you don't want to use these options and use the default, just write NULL instead of a number for the variable you don't
#    want to use. 
      
      # Set the size for the x and y lab ticks in the scree plot. Recommended: a number between 1 and 2
      scree_plot_axis_tick_size <- NULL
      # Set the size for the x and y lab names in the scree plot. Recommended: a number between 1 and 2
      scree_plot_axis_name_size <- NULL
      # Set the colour for the bars in the scree plot. Colours can be chosen from http://www.stat.columbia.edu/~tzheng/files/Rcolor.pdf
      scree_plot_bar_colour <- "midnightblue"
      
      # Set the font size for the x and y ticks and names in the PCA plot. These are changed simultaneously and in relation to one another.
      # Recommended: numbers between 10 and 20
      pca_plot_axis_name_label_size <- NULL
      # Set the size for the stages shown as labels in the PCA plot. The size depends on the length of the timepoint names.
      # Recommended: start with the default (4) and change the size later
      pca_plot_stages_label_size <- NULL
      
      
      
# 6)  NAME your output files/plots. The name should be as SHORT as possible. 
#     All relevant information about the results will be put in the file name automatically. 
      
      output_name <- "your_output_name"  # Type here the desired name of the output



# 7)  LET THE SCRIPT DO THE REST  
#     Highlight all code (Ctrl + A) and press Ctrl + Enter.
#     Plots and files will be saved automatically in a new directory in your working directory.

      
      
      
      
      
###########################################################################################################################################################################  
###########################################################################################################################################################################
# DON'T CHANGE ANYTHING IN HERE
  
  
  
# CREATE A NEW DIRECTORY where the results will be stored.
      dir_name <- paste(Sys.Date(), "_", output_name, "_results_pca", sep = "")
      ifelse(!dir.exists(dir_name), dir.create(dir_name), FALSE)
      results_dir <- file.path(getwd(), dir_name)
      Sys.chmod(results_dir, mode = "0777", use_umask = TRUE)
      setwd(results_dir)
      
  
  
  
# FORMAT the input file and remove rows with all zeros
      row.names(input_file) <- input_file[,1]
      input_file <- input_file[,-1]
      input_file <- input_file[apply(input_file, 1, function(x) !all(x==0)),]  
      colnames(input_file) <- TimeCateg
      
# WRITE default values for plotting options  
      if(is.null(scree_plot_axis_tick_size)) {scree_plot_axis_tick_size <- 1.75}
      if(is.null(scree_plot_axis_name_size)) {scree_plot_axis_name_size <- 1.5}
      if(is.null(scree_plot_bar_colour)) {scree_plot_bar_colour <- "grey"}
      
      if(is.null(pca_plot_axis_name_label_size)) {pca_plot_axis_name_label_size <- 15}
      if(is.null(pca_plot_stages_label_size)) {pca_plot_stages_label_size <- 4}
      
      
  
#####################################################################################################################  
# PRINCIPAL COMPONENT ANALYSIS ######################################################################################
#####################################################################################################################
      
      pca <- prcomp(t(input_file), scale=TRUE)
      
      
      # Make a scree plot and save it as png
      pca.var <- pca$sdev^2
      pca.var.per <- round(pca.var/sum(pca.var)*100, 1)
      png(paste(output_name, "_scree_plot.png", sep = ""), width = 4800, height = 4800, res = 600)
      par(cex.axis=scree_plot_axis_name_size, cex.lab=scree_plot_axis_tick_size, mgp=c(2.3, 0.5, 0))
      barplot(pca.var.per, main=NULL, xlab="Principal Component", ylab="Percent Variation", col = scree_plot_bar_colour, names.arg = colnames(pca$rotation))
      dev.off()
  
      # Now make a fancy looking plot that shows the PCs and the variation:
      library(ggplot2)
      
      pca.data <- data.frame(Sample=rownames(pca$x), # make column with the sample id's
                                    X=pca$x[,1], # PC1, x-axis coordinates
                                    Y=pca$x[,2]) # PC2, y-axis coordinates
      
      pca_plot <- ggplot(data=pca.data, aes(x=X, y=Y, label=Sample)) + # tell ggplot which columns contain X and Y coordinates and which column contains the id's
        geom_text(size=pca_plot_stages_label_size) + # plot labels instead of shapes
        scale_x_continuous(limits = c(min(pca.data[,"X"])+min(pca.data[,"X"])*0.4, max(pca.data[,"X"])+max(pca.data[,"X"])*0.4)) +
        scale_y_continuous(limits = c(min(pca.data[,"Y"])+min(pca.data[,"X"])*0.1, max(pca.data[,"Y"])+max(pca.data[,"X"])*0.1)) +
        xlab(paste("PC1 - ", pca.var.per[1], "%", sep="")) + # label x-axis
        ylab(paste("PC2 - ", pca.var.per[2], "%", sep="")) + # label y-axis
        theme_bw() + # white theme
        theme(text = element_text(size = pca_plot_axis_name_label_size))+
        ggtitle(NULL)
      ggsave(pca_plot, device = "png", file=paste(output_name, "_pca_plot.png", sep = ""), width = 200, height = 200, units = "mm", dpi = 600)
      
      
      # Save a file with the top x genes/substances that have the largest effect on PC1 and PC2
      if (!is.null(largest_effect_on_PC)) {
        
          loading_scores <- pca$rotation[,1]
          scores <- abs(loading_scores) ## get the magnitudes
          score_ranked <- sort(scores, decreasing=TRUE)
          top_10 <- names(score_ranked[1:largest_effect_on_PC])
          write.csv(pca$rotation[top_10,1], paste(output_name, "_top_", largest_effect_on_PC ,"_largest_effect_on_PC1.csv", sep = ""))
          
          loading_scores <- pca$rotation[,2]
          scores <- abs(loading_scores) ## get the magnitudes
          score_ranked <- sort(scores, decreasing=TRUE)
          top_10 <- names(score_ranked[1:largest_effect_on_PC])
          write.csv(pca$rotation[top_10,2], paste(output_name, "_top_", largest_effect_on_PC ,"_largest_effect_on_PC2.csv", sep = ""))
      }
  
  
#####################################################################################################################  
# PREPARING DATA FOR FRIEDMAN TEST ################################################################################## 
#####################################################################################################################
      
      # Preparing data
      friedman_means <- input_file
      
      # Create unique ID's for long version
      friedman_means$ID <- seq.int(nrow(friedman_means))
      
      # Load library reshape2 to be able to use melt function
      library(reshape2)
      
      # Create long data frame
      friedman_long <- melt(friedman_means, id.vars=c("ID"))
  
  
  
##################################################################################################################### 
# FRIEDMAN AND POST HOC TEST ########################################################################################
#####################################################################################################################
  
      # Use function from source
      source_dir <- paste(dirname(rstudioapi::getSourceEditorContext()$path), "/friedman_script_source.R", sep = "")    
      source(source_dir)
      
      # Do the friedman test and write result in file
      post_hoc_test <- friedman.test.with.post.hoc(value ~ variable | ID , friedman_long)
      sink(paste(output_name, "_friedman_test.txt", sep = ""))
      post_hoc_test$Friedman.Test
      sink()
      
      # Create data frame with sorted comparisons from post hoc test and write it in a file
      post_hoc_data_frame_sorted <- data.frame(dimnames(post_hoc_test$PostHoc.Test)[[1]][order(post_hoc_test$PostHoc.Test)])
      post_hoc_data_frame_sorted <- cbind.data.frame(post_hoc_data_frame_sorted, sort(post_hoc_test$PostHoc.Test))
      colnames(post_hoc_data_frame_sorted) <- c("Pairwise comparison", "p value")
      write.csv(post_hoc_data_frame_sorted, paste(output_name, "_friedman_post_hoc_test.csv"))
      
      end_time <- Sys.time()
      
      
      
    
      
      paste("Total time consumed for the analysis:", round(end_time - start_time, 2), attr((end_time - start_time), "unit"))