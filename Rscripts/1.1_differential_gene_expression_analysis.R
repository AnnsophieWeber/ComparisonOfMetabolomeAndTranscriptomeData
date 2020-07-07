###########################################################################################################################################################################
# DIFFERENTIAL GENE EXPRESSION ANALYSIS
# Script to do a differential gene expression analysis for a time series.
# This script is usable for not normalised transcript counts as it normalizes the counts itself.
# To do the analysis you need a .csv file with counts for each replicate. 
# This script will produce: 
#       - a report file of the analysis
#       - two heatmaps (raw normalised counts and model fit counts) with each a file of the z-scores
#       - a file containing normalised counts
#       - a file containing normalised counts, their means and standard deviation
#       - a file containing means and standard deviation
#       - a file containing means
#       - optional: plots of top x differential expressed genes
###########################################################################################################################################################################
      start_time <- Sys.time()

# 1)  SET YOU WORKING DIRECTORY. This is where your input file is located and the results will be stored.
#     Click in the menubar on the tab "Session" > "Set Working Directory" > "Choose Directory..."
#     Then choose the directory where your input file (the one with the means) is located and the results will be stored.



# 2)  READ/IMPORT your files into RStudio. Each file should contain the genes as rows with the timepoints as columns from only one replicate.
#     Watch out! The row and column names should not contain any special characters!

      # Replicate 1
      replicate_file_1 <- read.csv("your_replicate_1.csv") # Type in here the name of the file
      
      # Replicate 2
      replicate_file_2 <- read.csv("your_replicate_2.csv") # Type in here the name of the file
      
      # Replicate 3
      replicate_file_3 <- read.csv("your_replicate_3.csv") # Type in here the name of the file
      

      # If more replicates exist, you can add them by copying and pasting one of the "replicate_file_x" code lines and just changing the number of the replicate in the names.
      # If less replicates exist, you can remove the code lines that are not needed.   

      # List the imported replicate data in a variable (needed for computation)    
      data <- list(replicate_file_1, replicate_file_2, replicate_file_3)

      
      
# 3)  SPECIFY your data information:
      
      # How should the timepoints be called?
      TimeCateg <- as.factor(c("Timepoint1", "Timepoint2", "Timepoint3", "Timepoint4", "Timepoint5"))
      
      # How should the timepoints be called as numbers?  
      Time <- c(1, 2, 3, 4, 5)
      
      

# 4)  DECIDE whether you want time series plots of x top differential expressed genes as results.
#     If yes: write how many (x) top differential expressed genes you want to be plotted.
#       Example:    number_of_top_genes <- 10
#     If no: write NULL instead of a number:  
#                   number_of_top_genes <- NULL

      number_of_top_genes <- NULL



# 5) OPTIONAL: Only needed, if you didn't define number_of_top_genes in the previous step as NULL.
#    DECIDE what should be shown in the plot and define font sizes and colours for the produced plots. 

      # Write TRUE if the line/points should be shown, write FALSE if it should not be shown. 
      # You can also decide on a colour for the lines/points. 
      # Colours can be chosen from http://www.stat.columbia.edu/~tzheng/files/Rcolor.pdf 
      # If you wrote FALSE for the line/points, write NULL for the corresponding colour.
      
      # Graph of the fitted model calculated by ImpulseDE2
      show_fitted_model_in_plot <- TRUE
      colour_fitted_model <- "midnightblue"
      
      # Graph of the means calculated from the normalised counts
      show_normalised_count_means <- TRUE
      colour_normalised_count_means <- "cornflowerblue"
      
      # Data points of normalized counts
      show_normalised_counts <- TRUE
      colour_normalised_counts <- "slategray2"
      
      # Set the font size for the x and y ticks, names and the legend labels in the time series plot.
      # If you don't want to use these options and use the default, just write NULL instead of a number for the variable you don't
      # want to use.
      # Recommended: numbers between 10 and 20
      time_series_plot_axis_name_label_size <- NULL
      time_series_plot_legend_label_size <- NULL
      
      # DECIDE whether the timepoints should be shown as numbers or as names (TimeCateg) on the x axis of the time series plot.
      # If the names should be shown, write TRUE, if the numbers should be shown, write FALSE.
      
      show_TimeCateg_on_x_axis <- TRUE
      
  
    
# 6)  OPTIONAL: DEFINE a threshold for transcript counts. 
#     If you want to use this option, write a number, if you don't want to use this option, write NULL instead
#     By using this option some additional files will be generated containing only genes that show at least at one timepoint 
#     transcript counts equal to or more than your set threshold. 
#     This can be useful for the following scripts, especially for correlation e.g. to find only relevant correlations.
      
      transcripts_threshold <- NULL
      


# 7)  NAME your output files/plots. The name should be as SHORT as possible. 
#     All relevant information about the results will be put in the file name automatically. 
      
      output_name <- "your_output_name"  # Type here the desired name of the output



# 8)  LET THE SCRIPT DO THE REST  
#     Highlight all code and press Ctrl + Enter.
#     Plots and files will be saved automatically.






###########################################################################################################################################################################  
###########################################################################################################################################################################
# DON'T CHANGE ANYTHING IN HERE


# CREATE A NEW DIRECTORY where the results will be stored.
      dir_name <- paste(Sys.Date(), "_", output_name,  "_results_differential_gene_expression", sep = "")
      ifelse(!dir.exists(dir_name), dir.create(dir_name), FALSE)
      results_dir <- file.path(getwd(), dir_name)
      Sys.chmod(results_dir, mode = "0777", use_umask = TRUE)
      setwd(results_dir)
      

# FRORMAT the input data 
      data <- lapply(data, FUN = function(i){ 
        row.names(i) <- i[,1] 
        i <- i[,-1]
      })


# CREATE MATRIX for ImpulseDE2 (matCountData)
      Sample <- character()
      Sample <- unlist(append(Sample, lapply(data, colnames)))
      all_reps <- matrix(unlist(data), nrow = nrow(data[[1]]), ncol = ncol(data[[1]])*length(data), dimnames = list(row.names(data[[1]]), Sample))
      

# CREATE META DATA for ImpulseDE2 (dfAnnotation)      
      Condition <- "case"
      cData <- data.frame(row.names = colnames(all_reps), Sample, Time, Condition, TimeCateg)
      

# WRITE default values for plotting options  
      if (is.null(time_series_plot_axis_name_label_size)) {time_series_plot_axis_name_label_size <- 12}
      if (is.null(time_series_plot_legend_label_size)) {time_series_plot_legend_label_size <- 10}
     


#####################################################################################################################
# RUN IMPULSEDE2 ####################################################################################################
#####################################################################################################################

      library(ImpulseDE2)
      
      objectImpulseDE2 <- runImpulseDE2(
        matCountData    = all_reps, 
        dfAnnotation    = cData,
        boolCaseCtrl    = FALSE,                # FALSE if case-only, TRUE if case and control
        boolIdentifyTransients = TRUE)          # TRUE, if you want to identify transition up/down und transient up/down regulated genes
      

# WRITE A REPORT FILE on how long it took R to "runImpulseDE2()"
      writeReportToFile(object = objectImpulseDE2, file = "de_report.txt")
      
      saveRDS(objectImpulseDE2, file = paste(output_name, "_objectImpulseDE2.rds", sep = ""))


# WRITE A FILE with which gene counts are all 0 and therefore not included in the de analysis
      all_zero <- all_reps[apply(all_reps, 1, function(x) all(x==0)),]
      write.csv(all_zero, paste(output_name, "_genes_excluded_from_the_analyis_all_zero.csv", sep = ""))



####################################################################################################################
# CREATE HEATMAPS OF DIFFERENTIAL EXPRESSION #######################################################################
####################################################################################################################

# CREATE HEATMAPS
      library(ComplexHeatmap)
      lsHeatmaps <- plotHeatmap(
        objectImpulseDE2       = objectImpulseDE2,
        strCondition           = "case",
        boolIdentifyTransients = TRUE,
        scaQThres              = 0.01)


# DRAW HEATMAP with normalised raw data
      png(paste(output_name, "_heatmap_raw_data.png"), width = 4800, height = 4800, res = 600)
      draw(lsHeatmaps$complexHeatmapRaw) 
      dev.off()


# DRAW HEATMAP with normalised and fitted data
      png(paste(output_name, "_heatmap_fitted_data.png"), width = 4800, height = 4800, res = 600)
      draw(lsHeatmaps$complexHeatmapFit)
      dev.off()



####################################################################################################################
# EXTRACT Z-SCORE VALUES FROM PLOTHEATMAP RESULTS ##################################################################
####################################################################################################################

# CREATE DATA FRAME with raw z-score values
      raw_z_scores <- lsHeatmaps$complexHeatmapRaw@matrix
      raw_z_scores <- data.frame(raw_z_scores)
      colnames(raw_z_scores) <- c(paste("z.score.stage.", Time, sep = ""))
      

# ADD the differential expression status and the gene names to the data frame
      raw_z_scores["differential.expression"] <- lsHeatmaps$complexHeatmapRaw@matrix_param$row_split
      raw_z_scores["gene"] <- c(lsHeatmaps$lsvecGeneGroups$transition_up, lsHeatmaps$lsvecGeneGroups$transition_down, lsHeatmaps$lsvecGeneGroups$transient_up, lsHeatmaps$lsvecGeneGroups$transient_down)


# SAVE data frame as a csv file
      write.csv(raw_z_scores, paste(output_name, "_z_scores_raw.csv", sep = ""))


# CREATE DATA FRAME with fitted z-score values
      fit_z_scores <- lsHeatmaps$complexHeatmapFit@matrix
      fit_z_scores <- data.frame(fit_z_scores)
      colnames(fit_z_scores) <- c(paste("z.score.stage.", Time, sep = ""))
      

# ADD the differential expression status and the gene names to the data frame
      fit_z_scores["differential.expression"] <- lsHeatmaps$complexHeatmapFit@matrix_param$row_split
      fit_z_scores["gene"] <- c(lsHeatmaps$lsvecGeneGroups$transition_up, lsHeatmaps$lsvecGeneGroups$transition_down, lsHeatmaps$lsvecGeneGroups$transient_up, lsHeatmaps$lsvecGeneGroups$transient_down)
      

# SAVE data frame as a csv file
      write.csv(fit_z_scores, paste(output_name, "_z_scores_fitted.csv", sep = ""))


# SAVE how many up, down, *up (transient) and *down (transient) were identified
      sink(paste(output_name, "_de_analysis_results.txt", sep = ""))
      print(paste("All zero, excluded from analysis: ", nrow(all_zero), sep = ""))
      print(paste("Selected genes for analysis: ", length(objectImpulseDE2@lsModelFits$case)))
      print(paste("Transient down: ", length(lsHeatmaps$lsvecGeneGroups$transient_down), sep = ""))
      print(paste("Transient up: ", length(lsHeatmaps$lsvecGeneGroups$transient_up), sep = ""))
      print(paste("Transition down: ", length(lsHeatmaps$lsvecGeneGroups$transition_down), sep = ""))
      print(paste("Transition up: ",length(lsHeatmaps$lsvecGeneGroups$transition_up), sep = ""))
      sink()



###################################################################################################################
# PLOT TIME SERIES OF X TOP DIFFERENTIAL EXPRESSED GENES ##########################################################
###################################################################################################################

      library(dplyr)
      library(ggplot2)
      
      if (!is.null(number_of_top_genes)) {
        
          dfAnnot <- get_dfAnnotationProc(obj=objectImpulseDE2)
          Time <- sort(unique(dfAnnot$Time), decreasing = FALSE)      
          Reps <- rep(Time, each = nrow(dfAnnot)/length(Time))
          
          top_gene_names <- objectImpulseDE2$dfImpulseDE2Results[
            with(objectImpulseDE2$dfImpulseDE2Results, 
                 order(padj)), ]$Gene[1:number_of_top_genes]
          
          for (i in 1:number_of_top_genes) {
            
              top_genes <- plotGenes(
                vecGeneIDs = top_gene_names[i],
                objectImpulseDE2 = objectImpulseDE2,
                boolCaseCtrl = FALSE,
                boolMultiplePlotsPerPage = TRUE,
                boolSimplePlot = TRUE)
              
              # create own plot with fitted curve and mean
              # get fit data for fit curve
              fitted_curve <- top_genes[[1]]$plot_env$dfFit[, 1:2]
              colnames(fitted_curve) <- c("time", "fitCurve")
              
              # get normCounts from replicates for data points
              normCounts <- top_genes[[1]]$plot_env$dfRaw
              normCounts <- normCounts[, c("time", "normCounts")]
              
              # calculate means of normCounts for mean curve
              means <- aggregate(top_genes[[1]]$plot_env$dfRaw$normCounts, list(top_genes[[1]]$plot_env$dfRaw$time), mean)
              colnames(means) <- c("time", "normCountMeans")
              
              # create full joined data frame with all values to plot all in the same plot and additionally order it by time
              big_df <- full_join(fitted_curve, normCounts, by = "time")
              big_df <- full_join(big_df, means, by = "time")
              big_df <- big_df[with(big_df, order(time)), ]
              
              tempplot <- ggplot(data = NULL, aes(x = Time)) +
                labs(x = "Timepoint", y = "Transcript counts", title = top_gene_names[i]) +
                theme_minimal()+
                scale_color_manual(values=c(colour_fitted_model, colour_normalised_count_means, colour_normalised_counts))
              
              if (show_TimeCateg_on_x_axis) { 
                tempplot <- tempplot + scale_x_continuous(breaks=Time,labels=TimeCateg)
                tempplot <- tempplot + theme(axis.text.x = element_text(angle=45, hjust = 1), plot.title = element_text(hjust = 0.5), text = element_text(size = time_series_plot_axis_name_label_size), legend.title=element_blank(), legend.text =element_text(size = time_series_plot_legend_label_size))
              } else {
                tempplot <- tempplot + scale_x_continuous(labels = Time, breaks = Time)
                tempplot <- tempplot + theme(plot.title = element_text(hjust = 0.5), text = element_text(size = time_series_plot_axis_name_label_size), legend.title=element_blank(), legend.text =element_text(size = time_series_plot_legend_label_size))
              }
              
              if (show_fitted_model_in_plot) {
                tempplot <- tempplot + geom_line(data = na.omit(big_df[, 1:2]), aes(x=time, y = fitCurve, colour = "ImpulseDE2 fitted model"))}
              
              if (show_normalised_count_means) {
                tempplot <- tempplot + geom_line(data = unique(na.omit(big_df[, c(1, 4)])), aes(y = normCountMeans, colour = "Normalised count means"))}
              
              if (show_normalised_counts) {
                tempplot <- tempplot + geom_point(data = na.omit(big_df[, c(1, 3)]), aes(x=Reps ,y = normCounts, colour = "Normalised counts"))}
              
              ggsave(tempplot, device = "png", file=paste(output_name, "_number_", i, "_expressed_", top_gene_names[i] , ".png", sep = ""), width = 200, height = 200, units = "mm", dpi = 600)
            
          }
      }




####################################################################################################################
# CREATE FILES WITH NORMALISED COUNTS, MEANS AND STANDARD DEVIATIONS (SD) ##########################################
####################################################################################################################

      # processed data frame without missing values e.g.
      matCountDataProc <- get_matCountDataProc(objectImpulseDE2) 

      # size factors for calculating size factor scaled and normalized counts
      vecSizeFactors <- get_vecSizeFactors(objectImpulseDE2) 
      
      # create data frame with normalized counts:
      # divide counts by scale size factor
      norm_counts <- sweep(matCountDataProc, 2, vecSizeFactors, `/`)
      norm_counts <- data.frame(norm_counts)
      
      # save file with norm counts
      write.csv(norm_counts, paste(output_name, "_normalised_counts.csv", sep = ""))
      
      number_of_timepoints <- length(Time)
      number_of_replicates <- length(data)
      
      cols <- as.numeric()
      for (j in 1:number_of_timepoints) {
        for (i in 0:(number_of_replicates-1)) {
          cols <- append(cols, i*number_of_timepoints+j)
        }  
      }
      
      counts_means_sd <- data.frame(matrix(nrow = nrow(norm_counts)))
      means <- data.frame(matrix(nrow = nrow(norm_counts)))
      means_sd <- data.frame(matrix(nrow = nrow(norm_counts)))
      
      
      for (i in 1:number_of_timepoints){
          # get columns
          temp <- split(cols, sort(cols%%number_of_timepoints))[[i]]
          
          #create temporary data frame for each timepoint
          temp_df <- norm_counts[,temp]
          temp_df$mean <- apply(temp_df, 1, mean, na.rm=TRUE)
          temp_df$sd <- apply(temp_df[,1:number_of_replicates], 1, sd, na.rm=TRUE)
          
          # create big data frames that will be saved
          counts_means_sd <- cbind.data.frame(counts_means_sd, temp_df)
          
          # create a data frame with just the means
          means <- cbind.data.frame(means, temp_df$mean)
          row.names(means) <- row.names(norm_counts)
          
          # create a data frame with the means and the standard deviations
          means_sd <- cbind.data.frame(means_sd, temp_df$mean, temp_df$sd)
          row.names(means_sd) <- row.names(norm_counts)
      }
      
      # save the created data frames
      counts_means_sd <- counts_means_sd[,-1]
      write.csv(counts_means_sd, paste(output_name, "_normalised_counts_means_sd.csv", sep = ""))
      
      means <- means[,-1]
      timepoints <- as.character(TimeCateg)
      colnames(means) <- timepoints
      write.csv(means, paste(output_name, "_normalised_means.csv", sep = ""))
      
      means_sd <- means_sd[,-1]
      colnames(means_sd) <- paste(rep(c("mean.", "sd."), times = number_of_timepoints), rep(timepoints, each=2), sep = "")
      write.csv(means_sd, paste(output_name, "_normalised_means_sd.csv", sep = ""))
      
      if (!is.null(transcripts_threshold)) {
      
          above_threshold_means <- means[apply(means,1,max) >= transcripts_threshold, ]
          write.csv(above_threshold_means, paste(output_name, "_above_", transcripts_threshold, "-transcript_normalised_means.csv", sep=""))
          
          above_threshold_means_sd <- means_sd[rownames(above_threshold_means), ]
          write.csv(above_threshold_means_sd, paste(output_name, "_above_", transcripts_threshold, "_transcript_normalised_means_sd.csv", sep=""))
          
          above_threshold_counts_means_sd <- counts_means_sd[rownames(above_threshold_means), ]
          write.csv(above_threshold_counts_means_sd, paste(output_name, "_above_", transcripts_threshold, "_transcript_normalised_counts_means_sd.csv", sep=""))
          
          above_threshold_norm_counts <- norm_counts[rownames(above_threshold_means), ]
          write.csv(above_threshold_norm_counts, paste(output_name, "_above_", transcripts_threshold, "_transcript_normalised_counts.csv", sep=""))
          
      }
      
      
      
      end_time <- Sys.time()
      
      
      paste("Total time consumed for the analysis:", round(end_time - start_time, 2), attr((end_time - start_time), "unit"))