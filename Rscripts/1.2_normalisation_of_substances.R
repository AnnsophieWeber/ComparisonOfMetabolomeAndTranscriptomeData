###########################################################################################################################################################################
# NORMALISE SUBSTANCES AND CALCULATE NORMALISE MEANS
# Script to normalise substance concentrations and calculate means.
# This script normalises each replicate time series based on the maximum, calculates mean concentrations for each substance at each timepoint
# from all replicates, generates a time series from these calculated  means and normalises these again based on the maximum.
# To generate a .csv file with means you need files containing the measurements for each substance at each timepoint. 
# The files should be .xlsx files containg each replicate as one sheet.
# This script will produce:
#         - a .csv file containing the means of each substance at each timepoint
#         - OPTIONAL: a .csv file containing the means of each substance at selected timepoints
###########################################################################################################################################################################
      start_time <- Sys.time()

# 1)  SET YOU WORKING DIRECTORY. This is where your input file is located and the results will be stored.
#     Click in the menubar on the tab "Session" > "Set Working Directory" > "Choose Directory..."
#     Then choose the directory where your input file (the one with the means) is located and the results will be stored.



# 2)  READ/IMPORT your files into RStudio. Each file should contain the substances as rows with the timepoints as columns from only one replicate.
#     At least 3 replicates are needed for calculation, to calculate a meaningful mean.You can use one xlsx file with different sheets as input.
#     Watch out! The row and column names should not contain any special characters!
#     Ensure that all columns and rows are named uniformly!      


      library(readxl)
      replicate_1 <- read_excel("your_file.xlsx", sheet = "your_replicate_sheet_1") 
      replicate_2 <- read_excel("your_file.xlsx", sheet = "your_replicate_sheet_2")
      replicate_3 <- read_excel("your_file.xlsx", sheet = "your_replicate_sheet_3")
      replicate_4 <- read_excel("your_file.xlsx", sheet = "your_replicate_sheet_4") 
      replicate_5 <- read_excel("your_file.xlsx", sheet = "your_replicate_sheet_5")
      replicate_6 <- read_excel("your_file.xlsx", sheet = "your_replicate_sheet_6")

      # If more replicates exist, you can add them by copying and pasting one of the "replicate_file_x" code lines and just changing the number of the replicate in the names.

      # List the imported replicate data in a variable (needed for computation)    
      data <- list(replicate_1, replicate_2, replicate_3, replicate_4, replicate_5, replicate_6)


            
# 4)  SPECIFY your data information:
      
      # How should the timepoints be called? Watch out! Don't use special characters in the names!  
      TimeCateg <- as.factor(c("Timepoint1", "Timepoint2", "Timepoint3", "Timepoint4", "Timepoint5"))
      
      # How should the timepoints be called as numbers?  
      Time <- c(1, 2, 3, 4, 5)
      
      
      
# 5)  NAME your output files/plots. The name should be as SHORT as possible. 
#     All relevant information about the results will be put in the file name automatically. 
      
      output_name <- "your_output_name"  # Type here the desired name of the output
      
      
      
# 6)  OPTIONAL: If you want only means of selected timepoints to be saved in a file, please specify here which ones:
#     If you don't need this option just write NULL: selected_Time <- NULL
      # Example:    number_of_top_genes <-  c(3,4,5)
      
      selected_Time <- NULL
      
      # You can also define another output name for this special file:
      # The name should be as SHORT as possible. All relevant information about the results will be put in the file name automatically. 
      
      output_name_selected_Time <- "your_output_name_2"
      

      
# 7)  LET THE SCRIPT DO THE REST  
#     Highlight all code (Ctrl + A) and press Ctrl + Enter.
#     Plots and files will be saved automatically in a new directory in your working directory.
    
      
    
      
      
      
###########################################################################################################################################################################  
###########################################################################################################################################################################
# DON'T CHANGE ANYTHING IN HERE
      
      
      
      number_of_replicates <- length(data)
      
      # Transform to data frame      
      data <- lapply(data, data.frame)
      
      # Replace all NA's except for the first column to 0
      data <- lapply(data, function(x) {x[-1][is.na(x[-1])] <- 0; x})      
            
      # Round RI to 3 digits
      data <- lapply(data, function(x) {x$RI <- round(x$RI, 3); x})
      
      # Change the rowname to substance name, if name is known, if not known, change it to RI 
      data <- lapply(data, function(x) { for (i in 1:nrow(x)) {
                                              if (!is.na(x[i, 1])) {
                                                  rownames(x)[i] <- x[i, "Name"]
                                              } else {
                                                  rownames(x)[i] <- x[i, "RI"]
                                              }
                                           }
                                          x
                                        })
      
      
      # calculate all relative peaks; 100%=maximum
      data <- lapply(data, function(x) {x <- x[, -c(1,2)]/apply(x[, -c(1,2)], 1, max)})
      data <- lapply(data, function(x) x*100)

      TimeCateg <- colnames(data[[1]])

      list_with_stage_df <- as.list(TimeCateg)
      
      # Fill list with data frames for stages
      i_df <- matrix(ncol = number_of_replicates, nrow = nrow(data[[1]]))
      i_df <- data.frame(i_df)
      rownames(i_df) <- rownames(data[[1]])
      temp <- as.character()
       
        for (k in 1:length(TimeCateg)) {
          
          for (j in 1:length(data)) {
            i_df[j] <- data[[j]][TimeCateg[k]]
            temp <- append(temp, j)
            
          }
          rownames(i_df) <- rownames(data[[1]])
          colnames(i_df) <- paste(TimeCateg[k], temp, sep = ".")
          list_with_stage_df[[k]] <- i_df
          temp <- as.character()
          
        }
        
      # Convert all NaN's and 0 to NA
      list_with_stage_df <- lapply(list_with_stage_df, function(x) {x[is.na(x)] <- 0; x})
      list_with_stage_df <- lapply(list_with_stage_df, function(x) {x[x==0] <- NA; x})
      
      # set a threshold of 3 values, to calculate a mean or a standard deviation
       for (x in 1:length(list_with_stage_df)){
        for (i in 1:nrow(list_with_stage_df[[x]])) {
          if (sum(is.na(list_with_stage_df[[x]][i, ])) >3)
            list_with_stage_df[[x]][i, ] <- NA
          
        }
      }
      
     # Calculate means and make a new data frame with just means
     means_stages <- lapply(list_with_stage_df, function(x) {apply(x, 1, mean, na.rm=TRUE)})
    
     means <- matrix(ncol = ncol(data[[1]]), nrow = nrow(data[[1]])) 
     means <- data.frame(means)
     
     for (i in 1:length(means_stages)) {
       means[i] <- means_stages[[i]]
     }
     
     means[is.na(means)] <- 0
     
     rownames(means) <- rownames(data[[1]])
     colnames(means) <- TimeCateg
     
     # Normalising again, so the highest value is always 100 %
     means_norm <- means/apply(means, 1, max)
     means_norm <- means_norm*100
     means_norm <- na.omit(means_norm)
    
     write.csv(means_norm, paste(output_name, "_substance_normalised_means.csv", sep = ""))
     
     if (!is.null(selected_Time)) {
        write.csv(means_norm[selected_Time], paste(output_name_selected_Time, "_substance_normalised_means.csv", sep = ""))
     }
     
     
     # # Calculate standard deviations and make a new data frame with just standard deviations
     # sds_stages <-  lapply(list_with_stage_df, function(x) {apply(x, 1, sd, na.rm=TRUE)})
     # 
     # sds <- matrix(ncol = ncol(data[[1]]), nrow = nrow(data[[1]])) 
     # sds <- data.frame(sds)
     # 
     # for (i in 1:length(sds_stages)) {
     #   sds[i] <- sds_stages[[i]]
     # }
     # 
     # sds[is.na(sds)] <- 0
     # 
     # rownames(sds) <- rownames(data[[1]])
     # colnames(sds) <- TimeCateg
     
     end_time <- Sys.time()
     
     
     
     
     
     paste("Total time consumed for the analysis:", round(end_time - start_time, 2), attr((end_time - start_time), "unit")) 