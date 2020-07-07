###########################################################################################################################################################################
# CALCULATING CORRELATION COEFFICIENTS
# Script to calculate correlation coefficients for two time series data sets.
# This script is usable for substance concentrations or transcript counts, as long as they show the same number of timepoints.
# To do the analysis, you need a .csv file with means for each time series data set as input. 
# This script will produce: 
#       - a heatmap visualizing the correlation matrix
#       - a file containing the correlation matrix
#       - OPTIONAL: a file containing the x highest correlation coefficients
###########################################################################################################################################################################
      start_time <- Sys.time()

# 1)  SET YOU WORKING DIRECTORY. This is where your input file is located and the results will be stored.
#     Click in the menubar on the tab "Session" > "Set Working Directory" > "Choose Directory..."
#     Then choose the directory where your input file (the one with the means) is located and the results will be stored.



# 2)  READ/IMPORT your files into RStudio. The file should contain the genes/substances as rows with the timepoints as columns and be csv format.
#     The time series to be analysed must have the same number of time points!
#     Watch out! The row and column names should not contain any special characters!

      input_file_1 <- read.csv("your_means_1.csv") # Type in here the name of the first file
      input_file_1_data_type <- "sample_1" # Type in here the type of your first time series, for example "Genes"
      
      input_file_2 <- read.csv("your_means_2.csv") # Type in here the name of the second file
      input_file_2_data_type <- "sample_2" # Type in here the type of your second time series, for example "Substances"
      

      
# 3)  DEFINE the correlation method. Recommended for non normal distribution is "spearman". Other methods are: "pearson" and "kendall".
      
      method <- "spearman"
      
      
      
# 3)  DECIDE whether you want a file with the x highest correlation coefficients.
#     If yes: write how many (x) of the highest correlation coefficients you want to be saved in the file.
#       Example:    number_of_highest_correlation_coefficients <- 50
#     If no: write NULL instead of a number:  
#                   number_of_highest_correlation_coefficients <- NULL      
      
      number_of_highest_correlation_coefficients <- NULL
      
      
      
# 4) DEFINE font sizes for row and column titles, the legend title, legend lables and the size of the legend bar in the heatmap.       
#    If you don't want to use these options and use the default, just write NULL instead of a number for the variable you don't
#    want to use. 
      
      # Set font sizes.
      # Recommended: numbers between 10 and 30
      heatmap_row_column_title_font_size <- NULL
      legend_title_font_size <- NULL
      legend_label_font_size <- NULL
      
      # Set the size of the legend bar (in points)
      # Recommended: a number between 100 and 200
      legend_height <- NULL
      
      
      
# 5)  NAME your output files/plots. The name should be as SHORT as possible. 
#     All relevant information about the results will be put in the file name automatically. 
      
      output_name <- "your_output_name"  # Type here the desired name of the output

      

# 6)  LET THE SCRIPT DO THE REST  
#     Highlight all code (Ctrl + A) and press Ctrl + Enter.
#     Plots and files will be saved automatically in a new directory in your working directory.





      
###########################################################################################################################################################################  
###########################################################################################################################################################################
# DON'T CHANGE ANYTHING IN HERE



# CREATE A NEW DIRECTORY where the results will be stored.
      dir_name <- paste(Sys.Date(), "_", output_name, "_results_correlation", sep = "")
      ifelse(!dir.exists(dir_name), dir.create(dir_name), FALSE)
      results_dir <- file.path(getwd(), dir_name)
      Sys.chmod(results_dir, mode = "0777", use_umask = TRUE)
      setwd(results_dir)


# WRITE default values for plotting options        
      if (is.null(heatmap_row_column_title_font_size)) {heatmap_row_column_title_font_size <- 20}
      if (is.null(legend_title_font_size)) {legend_title_font_size <- 20}
      if (is.null(legend_label_font_size)) {legend_label_font_size <- 15}
      if (is.null(legend_height)) {legend_height <- 200}
      

####################################################################################################################
# CALCULATE CORRELATION COEFFICIENTS ###############################################################################
####################################################################################################################
      
# PREPARE AND FORMAT the data sets for the calculation of the correlation
      # Set rownames
      rownames(input_file_1) <- input_file_1[,1]
      rownames(input_file_2) <- input_file_2[,1]
      
      # Remove first column with names
      input_file_1 <- input_file_1[,-1]
      input_file_2 <- input_file_2[,-1]
      
      # Remove rows with all zero
      input_file_1 <- input_file_1[apply(input_file_1, 1, function(x) !all(x==0)), ]
      input_file_2 <- input_file_2[apply(input_file_2, 1, function(x) !all(x==0)),]
      
      # Get rownames of non zero input data
      rownames_input_file_1 <- rownames(input_file_1)
      rownames_input_file_2 <- rownames(input_file_2)
      
      # Transform data set for correlation
      input_file_1_t <- data.frame(t(input_file_1))
      input_file_2_t <- data.frame(t(input_file_2))
      
     
      # Calcucalte correlation coefficients
      correlation <- cor(input_file_1_t, input_file_2_t, method = method)
      
      input_file_1_names <- data.frame(new.names = rownames(correlation), old.names = rownames_input_file_1)
      rownames(input_file_1_names) <- input_file_1_names[,1]
      input_file_1_names <- input_file_1_names[-1]
      input_file_1_names[] <- lapply(input_file_1_names, as.character)
      
      input_file_2_names <- data.frame(new.names = colnames(correlation), old.names = rownames_input_file_2)
      rownames(input_file_2_names) <- input_file_2_names[,1]
      input_file_2_names <- input_file_2_names[-1]
      input_file_2_names[] <- lapply(input_file_2_names, as.character)
      
      rownames(correlation) <- input_file_1_names$old.names
      colnames(correlation) <- input_file_2_names$old.names
      
      # Plot and save Heatmap
      library(ComplexHeatmap)
      library(circlize)
      big_correlation_heatmap <- Heatmap(correlation, 
                                         heatmap_legend_param = list(title = "Correlation coefficient", title_gp =gpar(fontsize = legend_title_font_size), labels_gp=gpar(fontsize = legend_label_font_size), legend_height=unit(legend_height, "points"), title_position="leftcenter-rot"),
                                         column_title = input_file_2_data_type,
                                         column_title_side = "top",
                                         column_title_gp = gpar(fontsize = heatmap_row_column_title_font_size),
                                         row_title = input_file_1_data_type, 
                                         row_title_side = "left", 
                                         row_title_rot = 90, 
                                         row_title_gp = gpar(fontsize = heatmap_row_column_title_font_size),
                                         show_row_names = FALSE, 
                                         show_column_names = FALSE,
                                         col = colorRamp2(c(-1, 0, 1), c("blue", "#EEEEEE", "red")))
      
      png(paste(output_name, "_correlation_heatmap.png", sep = ""), width = 4800, height = 4800, res = 600)
      draw(big_correlation_heatmap)
      dev.off()
      
      # Get order of substances (rows) and genes (columns) as ordered in heatmap
      colorder <- column_order(big_correlation_heatmap)
      roworder <- row_order(big_correlation_heatmap)
      
      # Create a data frame with ordered substances and genes as in the heatmap
      ordered_correlation <- correlation[roworder, colorder]
      
      # Save ordered correlation matrix in a file
      write.csv(ordered_correlation, paste(output_name, "_correlation_matrix.csv", sep = ""))
      
      
####################################################################################################################
# FIND HIGHEST CORRELATION COEFFICIENTS ############################################################################
####################################################################################################################
      
# GET top x correlation coefficients:
      
      if (!is.null(number_of_highest_correlation_coefficients)) {
      
          # Create long correlation matrix data frame:
          long_correlation <- data.frame(row = rep(rownames(correlation), ncol(correlation)), col = rep(colnames(correlation), each = nrow(correlation)),
                                         correlation.coefficient = as.vector(correlation))
          colnames(long_correlation) <- c(input_file_1_data_type, input_file_2_data_type, "correlation.coefficient")
          
          # Sort the correlation coefficients from highest to lowest
          ordered_long_correlation <- long_correlation[rev(order(long_correlation$correlation.coefficient, na.last = FALSE)), ]
          
          # Only needed if the same file is read twice as input
          if (isTRUE(all.equal(input_file_1, input_file_2))) {
            
              # Remove all values from diagonal, as they are automatically all 1
              ordered_long_correlation <- ordered_long_correlation[ordered_long_correlation[,1] != ordered_long_correlation[,2], ]
              
              # Remove all rows that contain the same elements, but in different columns (x and y, y and x) -> no duplicates
              ordered_long_correlation <- ordered_long_correlation[!duplicated(t(apply(ordered_long_correlation,1,sort))),]
              
              # Get x highest correlation coefficients and write a .csv file
              write.csv(ordered_long_correlation[1:number_of_highest_correlation_coefficients, ], paste(output_name, "_", as.character(number_of_highest_correlation_coefficients), "_highest_correlation_coefficients.csv", sep = ""))
              
              
          } else {    
          
            # Get x highest correlation coefficients and write a .csv file
            write.csv(ordered_long_correlation[1:number_of_highest_correlation_coefficients, ], paste(output_name, "_", as.character(number_of_highest_correlation_coefficients), "_highest_correlation_coefficients.csv", sep = ""))
          }
      }
    
      end_time <- Sys.time()
      
      
     
      
      
      paste("Total time consumed for the analysis:", round(end_time - start_time, 2), attr((end_time - start_time), "unit"))