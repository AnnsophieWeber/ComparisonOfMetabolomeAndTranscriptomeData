###########################################################################################################################################################################
# PLOTS WITH TWO Y-AXIS
# Script to create plots with two y-axis. One to display relative substance concentration and the other to display the
# number of transcript counts.
# To create the plots, you need one .csv file containing substance means and at least one .csv file containing transcript counts means. 
###########################################################################################################################################################################
      start_time <- Sys.time()

# 1)  SET YOU WORKING DIRECTORY. This is where your input file is located and the results will be stored.
#     Click in the menubar on the tab "Session" > "Set Working Directory" > "Choose Directory..."
#     Then choose the directory where your input file (the one with the means) is located and the results will be stored.



# 2)  READ/IMPORT your file into RStudio.
#     This script requires one file containing means of relative substance concentrations and at least on file containing means
#     of transcript counts. A second file for transcript counts can be used optional.

      substances <- read.csv("your_substance_means.csv") # Type in here the name of the first file

      transcriptome_1 <- read.csv("your_transcript_means_sample_1.csv") # Type in here the name of the first file

      # OPTIONAL: a second transcriptome counts file. If you don't want to use a second file, write transcriptome_2 <- NULL instead.
      transcriptome_2 <- read.csv("your_transcript_means_sample_2.csv") # Type in here the name of the first file
      


# 3)  DEFINE the names of the transcriptome data sets. If you're using only one transcriptome file, write NULL for the second name instead.

      transcriptome_name_1 <- "sample_1"
      transcriptome_name_2 <- "sample_2"



# 4)  SPECIFY your data information:

      # How should the timepoints be called? 
      TimeCateg <- as.factor(c("Timepoint1", "Timepoint2", "Timepoint3", "Timepoint4", "Timepoint5"))
      
      # How should the timepoints be called as numbers?  
      Time <- as.numeric(c(1, 2, 3, 4, 5))
      
      # Decide whether the timepoints should be shown as numbers or as names (TimeCateg) on the x axis of the time series plot.
      # If the names should be shown, write TRUE, if the numbers should be shown, write FALSE.
      show_TimeCateg_on_x_axis <- TRUE


# 5)  DEFINE which selected substances and genes you want to plot and choose a colour for these in the plot.
#     Colours can be chosen from http://www.stat.columbia.edu/~tzheng/files/Rcolor.pdf      

      selected_substances <- c("substance_1", 
                               "substance_2", 
                               "substance_3")
      
      substance_plot_colour <- "black"
      
      
      selected_genes <- c("gene_1",
                          "gene_2",
                          "gene_3",
                          "gene_5",
                          "gene_6")
      
      gene_plot_colour <- "darkgrey"



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
      dir_name <- paste(Sys.Date(), "_", output_name, "_results_substance_transcript_time_series_plots", sep = "")
      ifelse(!dir.exists(dir_name), dir.create(dir_name), FALSE)
      results_dir <- file.path(getwd(), dir_name)
      Sys.chmod(results_dir, mode = "0777", use_umask = TRUE)
      setwd(results_dir)


# FORMAT input files
      substances <- data.frame(substances)
      rownames(substances) <- substances[,1]
      substances <- substances[,-1]
      
      transcriptome_1 <- data.frame(transcriptome_1)
      rownames(transcriptome_1) <- transcriptome_1[,1]
      transcriptome_1 <- transcriptome_1[,-1]
      
      transcriptome_1_fill <- transcriptome_1
      
      if (is.null(gene_plot_colour)) {gene_plot_colour <- "darkgrey"}
      if (is.null(substance_plot_colour)) {substance_plot_colour <- "black"}
      

#####################################################################################################################  
# GENERATE PLOTS WITH TWO AXIS FOR ONE TRANSCRIPT FILE ##############################################################
#####################################################################################################################

      if (is.null(transcriptome_2)) { 
        
          #time <- 1:ncol(transcriptome_1_fill)
          
            if (show_TimeCateg_on_x_axis) {
              
                for (i in 1:length(selected_substances)) {
                  for (j in 1:length(selected_genes)) {
                    
                    if (!(selected_genes[j] %in% rownames(transcriptome_1_fill))) {
                        sink(paste(output_name, selected_genes[j], "_selected_genes_time_series_plot.txt", sep = ""))
                        print("Your selected gene shows no transcripts. The transcript count is at each time point 0, so no plot could be generated.")
                        print("Selected gene:")
                        print(selected_genes[j])
                        sink()
                      
                    } else {
                      
                        ylim_max <- max(transcriptome_1[selected_genes[j],])
                        if (ylim_max < 1) ylim_max <- 1
                        
                        png(paste(output_name, "_", selected_substances[i], "_", selected_genes[j], ".png", sep = ""), height = 4800, width = 4080, res = 600)
                        
                        ## add extra space to right margin of plot within frame
                        par(mar=c(16, 6, 4, 7) + 0.1, xpd=TRUE,  cex.axis=1.5)
                        
                        ## Plot first set of data and draw its axis
                        plot(Time, substances[selected_substances[i],], pch=16, axes=FALSE, ylim=c(0,100), xlab="", ylab="", 
                             type="b",col=substance_plot_colour, main=NULL) # main=paste(selected_substances[i], " and ",  selected_genes[j], " expression", sep = "")
                        axis(2, ylim=c(0,1),col=substance_plot_colour,las=1, col.axis=substance_plot_colour)  ## las=1 makes horizontal labels
                        mtext(paste("Relative ", selected_substances[i], " concentration (%)", sep=""), side=2,line=4, cex = 1.5, col = "black")
                        box()
                        
                        ## Allow a second plot on the same graph
                        par(new=TRUE, xpd=TRUE)
                        
                        ## Plot the second plot and put axis scale on right
                        plot(Time, transcriptome_1_fill[selected_genes[j],], pch=15,  xlab="", ylab="", ylim=c(0, ceiling(ylim_max)), 
                             axes=FALSE, type="b", col=gene_plot_colour)
                        
                        ## Draw the time axis
                        axis(1, Time, labels = FALSE)
                        text(Time, par("usr")[3]-0.15*ylim_max, labels = TimeCateg, srt=45, pos = 1, xpd = TRUE)
                        mtext("Timepoints",side=1,col="black",line=8, cex=1.5) 
                        
                        ## a little farther out (line=4) to make room for labels
                        mtext(paste("Transcript counts for ", selected_genes[j], sep = ""),side=4,col="black",line=5, cex = 1.5) 
                        axis(4, ylim=c(0,1), col=gene_plot_colour,col.axis=gene_plot_colour,las=1)
                        
                        ## Add Legend
                        legend("topright", inset=c(0.6,1.5), legend=c(selected_substances[i],paste(selected_genes[j], transcriptome_name_1)),
                               text.col=c("black", "black"),pch=c(16,15),col=c(substance_plot_colour,gene_plot_colour), xpd = TRUE, bty = "n", lty = c(1,1), text.width = 1, cex = 1.2)
                        
                        dev.off()
                        
                        
                        concentration_counts <- matrix(ncol =length(Time), nrow = 2)
                        concentration_counts <- data.frame(concentration_counts)
                        concentration_counts[1,] <- substances[selected_substances[i],]
                        concentration_counts[2,] <- transcriptome_1_fill[selected_genes[j],]
                        
                        row.names(concentration_counts) <- c(selected_substances[i], selected_genes[j])
                        colnames(concentration_counts) <- TimeCateg
                        
                        write.csv(concentration_counts, paste(output_name, "_", selected_substances[i], "_", selected_genes[j], ".csv", sep = ""))
                      
                    }
                }
              }
            
          } else {
            
              for (i in 1:length(selected_substances)) {
                for (j in 1:length(selected_genes)) {
                  
                    if (!(selected_genes[j] %in% rownames(transcriptome_1_fill))) {
                        sink(paste(output_name, selected_genes[j], "_selected_genes_time_series_plot.txt", sep = ""))
                        print("Your selected gene shows no transcripts. The transcript count is at each time point 0, so no plot could be generated.")
                        print("Selected gene:")
                        print(selected_genes[j])
                        sink()
                      
                    } else {
                      
                        ylim_max <- max(transcriptome_1[selected_genes[j],])
                        if (ylim_max < 1) ylim_max <- 1
                        
                        png(paste(output_name, "_", selected_substances[i], "_", selected_genes[j], ".png", sep = ""), height = 4800, width = 4500, res = 600)
                        
                        ## add extra space to right margin of plot within frame
                        par(mar=c(10, 6, 4, 7) + 0.1, xpd=TRUE, cex.axis=1.5)
                        
                        ## Plot first set of data and draw its axis
                        plot(Time, substances[selected_substances[i],], pch=16, axes=FALSE, ylim=c(0,100), xlab="", ylab="", 
                             type="b",col=substance_plot_colour, main=NULL) # main=paste(selected_substances[i], " and ",  selected_genes[j], " expression", sep = "")
                        axis(2, ylim=c(0,1),col=substance_plot_colour,las=1, col.axis=substance_plot_colour)  ## las=1 makes horizontal labels
                        mtext(paste("Relative ", selected_substances[i], " concentration (%)", sep=""), side=2,line=4, cex = 1.5, col = "black")
                        box()
                        
                        ## Allow a second plot on the same graph
                        par(new=TRUE, xpd=TRUE)
                        
                        ## Plot the second plot and put axis scale on right
                        plot(Time, transcriptome_1_fill[selected_genes[j],], pch=15,  xlab="", ylab="", ylim=c(0, ceiling(ylim_max)), 
                             axes=FALSE, type="b", col=gene_plot_colour)
                        
                        ## Draw the time axis
                        axis(1, Time)
                        mtext("Timepoints",side=1,col="black",line=3, cex=1.5) 
                        
                        ## a little farther out (line=4) to make room for labels
                        mtext(paste("Transcript counts for ", selected_genes[j], sep = ""),side=4,col="black",line=5, cex = 1.5) 
                        axis(4, ylim=c(0,1), col=gene_plot_colour,col.axis=gene_plot_colour,las=1)
                        
                        ## Add Legend
                        legend("topright", inset=c(0.6,1.18), legend=c(selected_substances[i],paste(selected_genes[j], transcriptome_name_1)),
                               text.col=c("black", "black"),pch=c(16,15),col=c(substance_plot_colour,gene_plot_colour), xpd = TRUE, bty = "n", lty = c(1,1), text.width = 1, cex = 1.2)
                        dev.off()
                        
                        
                        concentration_counts <- matrix(ncol =length(Time), nrow = 2)
                        concentration_counts <- data.frame(concentration_counts)
                        concentration_counts[1,] <- substances[selected_substances[i],]
                        concentration_counts[2,] <- transcriptome_1_fill[selected_genes[j],]
                        
                        row.names(concentration_counts) <- c(selected_substances[i], selected_genes[j])
                        colnames(concentration_counts) <- Time
                        
                        write.csv(concentration_counts, paste(output_name, "_", selected_substances[i], "_", selected_genes[j], ".csv", sep = ""))
                        
                    }    
                }
              }
            
          }  
          
        
        
        
        
        
        
        ####################################################################################################################
        # GENERATE PLOTS WITH TWO AXIS FOR TWO TRANSCRIPT FILES ############################################################
        ####################################################################################################################
        
        
      } else if (!is.null(transcriptome_2)) {
        
          transcriptome_2 <- data.frame(transcriptome_2)
          rownames(transcriptome_2) <- transcriptome_2[,1]
          transcriptome_2 <- transcriptome_2[,-1]
          transcriptome_2_fill <- transcriptome_2
          
          all_zeros <- rep(0, ncol(transcriptome_2_fill))
          transcriptome_2_fill[setdiff(rownames(transcriptome_1), rownames(transcriptome_2)),] <- all_zeros
          
          all_zeros <- rep(0, ncol(transcriptome_1_fill))
          transcriptome_1_fill[setdiff(rownames(transcriptome_2), rownames(transcriptome_1)),] <- all_zeros
          
          
          # if first data set has more columns than second
          if (ncol(transcriptome_1) > ncol(transcriptome_2)){
            
              # get the difference between the number of columns
              diff_cols <- ncol(transcriptome_1) - ncol(transcriptome_2)
              cols <- c()
              
              # add new columns to the data frame and make a new order for the columns for later
              for (i in 1:diff_cols) {
                  transcriptome_2_fill[,(ncol(transcriptome_2)+i)] <- NA
                  cols <- c(cols, (ncol(transcriptome_2) + i))
              }
            
              # add old column names to the new order
              colnames(transcriptome_2_fill)[cols] <- setdiff(colnames(transcriptome_1), colnames(transcriptome_2))
              transcriptome_2_fill <- transcriptome_2_fill[,colnames(transcriptome_1)]
              
            
            # if second data set has more columns than first  
          } else if (ncol(transcriptome_2) > ncol(transcriptome_1)){
            
              # get the difference between the number of columns
              diff_cols <- ncol(transcriptome_2) - ncol(transcriptome_1)
              cols <- c()
              
              # add new columns to the data frame and make a vector for new order for the columns for later
              for (i in 1:diff_cols) {
                transcriptome_1_fill[,(ncol(transcriptome_1)+i)] <- NA
                cols <- c(cols, (ncol(transcriptome_1) + i))
              }
              
              # get new column names and add old column names to the vector
              colnames(transcriptome_1_fill)[cols] <- setdiff(colnames(transcriptome_2), colnames(transcriptome_1))
              transcriptome_1_fill <- transcriptome_1_fill[,colnames(transcriptome_2)]
              
            }
            
            
            # set up x axis
            #time <- 1:ncol(transcriptome_1_fill)
            
            if (show_TimeCateg_on_x_axis) {
              
                for (i in 1:length(selected_substances)) {
                  for (j in 1:length(selected_genes)) {
                    
                    
                      # if selected gene can be found in intersection of both transcriptomes, print both transcript graphs
                      if (selected_genes[j] %in% rownames(transcriptome_1_fill)) {
                        
                          ylim_max <- max(max(transcriptome_1[selected_genes[j],]), max(transcriptome_2[selected_genes[j],]), na.rm = TRUE)
                          if (ylim_max < 1) ylim_max <- 1
                          
                          png(paste(output_name, "_", selected_substances[i], "_", selected_genes[j], ".png", sep = ""), height = 4800, width = 4080, res = 600)
                          
                          ## add extra space to right margin of plot within frame
                          par(mar=c(16, 6, 4, 7) + 0.1, xpd=TRUE,  cex.axis=1.5)
                          
                          ## Plot first set of data and draw its axis
                          plot(Time, substances[selected_substances[i],], pch=16, axes=FALSE, ylim=c(0,100), xlab="", ylab="", 
                               type="b",col=substance_plot_colour, main=NULL) # main=paste(selected_substances[i], " and ",  selected_genes[j], " expression", sep = "")
                          axis(2, ylim=c(0,1),col=substance_plot_colour,las=1, col.axis=substance_plot_colour)  ## las=1 makes horizontal labels
                          mtext(paste("Relative ", selected_substances[i], " concentration (%)", sep=""), side=2,line=4, cex = 1.5, col = "black")
                          box()
                          
                          ## Allow a second plot on the same graph
                          par(new=TRUE, xpd=TRUE)
                          
                          ## Plot the second plot and put axis scale on right
                          plot(Time, transcriptome_1_fill[selected_genes[j],], pch=15,  xlab="", ylab="", ylim=c(0, ceiling(ylim_max)), #ceiling(ylim_max+0.2*ylim_max)
                               axes=FALSE, type="b", col=gene_plot_colour)
                          
                          ## Draw the time axis
                          axis(1, Time, labels = FALSE)
                          text(Time, par("usr")[3]-0.15*ylim_max, labels = TimeCateg, srt=45, pos = 1, xpd = TRUE)
                          mtext("Timepoints",side=1,col="black",line=8, cex=1.5) 
                          
                          par(new=TRUE, xpd=TRUE)
                          plot(Time, transcriptome_2_fill[selected_genes[j],], pch=17,  xlab="", ylab="", ylim=c(0, ceiling(ylim_max)), axes=FALSE, type="b", col=gene_plot_colour, lty="longdash")
                          ## a little farther out (line=4) to make room for labels
                          mtext(paste("Transcript counts for ", selected_genes[j], sep = ""),side=4,col="black",line=5, cex = 1.5) 
                          axis(4, ylim=c(0,1), col=gene_plot_colour,col.axis=gene_plot_colour,las=1)
                          
                          ## Add Legend
                          legend("topright", inset=c(0.6,1.5), legend=c(selected_substances[i],paste(selected_genes[j], transcriptome_name_1), paste(selected_genes[j], transcriptome_name_2)),
                                 text.col=c("black","black", "black"),pch=c(16,15,17),col=c(substance_plot_colour,gene_plot_colour, gene_plot_colour), xpd = TRUE, bty = "n", lty = c(1,1,6), text.width = 1, cex = 1.2)
                          dev.off()
                          
                          
                          concentration_counts <- matrix(ncol =length(Time), nrow = 3)
                          concentration_counts <- data.frame(concentration_counts)
                          concentration_counts[1,] <- substances[selected_substances[i],]
                          concentration_counts[2,] <- transcriptome_1_fill[selected_genes[j],]
                          concentration_counts[3,] <- transcriptome_2_fill[selected_genes[j],]
                          
                          row.names(concentration_counts) <- c(selected_substances[i], paste(selected_genes[j], transcriptome_name_1, sep = "."), paste(selected_genes[j], transcriptome_name_2, sep = "."))
                          colnames(concentration_counts) <- TimeCateg
                          
                          write.csv(concentration_counts, paste(output_name, "_", selected_substances[i], "_", selected_genes[j], ".csv", sep = ""))
                          
                      
                      } else {
                        
                          sink(paste(output_name, selected_genes[j], "_selected_genes_time_series_plot.txt", sep = ""))
                          print("Your selected gene shows no transcripts. The transcript count is at each time point 0, so no plot could be generated.")
                          print("Selected gene:")
                          print(selected_genes[j])
                          sink()
                      }
                  
                }
              }
            
            
          } else {
            
              for (i in 1:length(selected_substances)) {
                for (j in 1:length(selected_genes)) {
                  
                  
                    if (selected_genes[j] %in% rownames(transcriptome_1_fill)) {
                      
                        ylim_max <- max(max(transcriptome_1[selected_genes[j],]), max(transcriptome_2[selected_genes[j],]), na.rm = TRUE)
                        if (ylim_max < 1) ylim_max <- 1
                        
                        png(paste(output_name, "_", selected_substances[i], "_", selected_genes[j], ".png", sep = ""), height = 4800, width = 4500, res = 600)
                        
                        ## add extra space to right margin of plot within frame
                        par(mar=c(10, 6, 4, 7) + 0.1, xpd=TRUE,  cex.axis=1.5)
                        
                        ## Plot first set of data and draw its axis
                        plot(Time, substances[selected_substances[i],], pch=16, axes=FALSE, ylim=c(0,100), xlab="", ylab="", 
                             type="b",col=substance_plot_colour, main=NULL) # main=paste(selected_substances[i], " and ",  selected_genes[j], " expression", sep = "")
                        axis(2, ylim=c(0,1),col=substance_plot_colour,las=1, col.axis=substance_plot_colour)  ## las=1 makes horizontal labels
                        mtext(paste("Relative ", selected_substances[i], " concentration (%)", sep=""), side=2,line=4, cex = 1.5, col = "black")
                        box()
                        
                        ## Allow a second plot on the same graph
                        par(new=TRUE, xpd=TRUE)
                        
                        ## Plot the second plot and put axis scale on right
                        plot(Time, transcriptome_1_fill[selected_genes[j],], pch=15,  xlab="", ylab="", ylim=c(0, ceiling(ylim_max)), 
                             axes=FALSE, type="b", col=gene_plot_colour)
                        
                        ## Draw the time axis
                        axis(1, Time)
                        mtext("Timepoints",side=1,col="black",line=2.5, cex=1.5) 
                        
                        par(new=TRUE, xpd=TRUE)
                        plot(Time, transcriptome_2_fill[selected_genes[j],], pch=17,  xlab="", ylab="", ylim=c(0, ceiling(ylim_max)), axes=FALSE, type="b", col=gene_plot_colour, lty="longdash")
                        ## a little farther out (line=4) to make room for labels
                        mtext(paste("Transcript counts for ", selected_genes[j], sep = ""),side=4,col="black",line=5, cex = 1.5) 
                        axis(4, ylim=c(0,1), col=gene_plot_colour,col.axis=gene_plot_colour,las=1)
                        
                        ## Add Legend
                        legend("topright", inset=c(0.6,1.18), legend=c(selected_substances[i],paste(selected_genes[j], transcriptome_name_1), paste(selected_genes[j], transcriptome_name_2)),
                               text.col=c("black","black", "black"),pch=c(16,15,17),col=c(substance_plot_colour,gene_plot_colour, gene_plot_colour), xpd = TRUE, bty = "n", lty = c(1,1,6), text.width = 1, cex = 1.2)
                        dev.off()
                        
                        
                        concentration_counts <- matrix(ncol =length(Time), nrow = 3)
                        concentration_counts <- data.frame(concentration_counts)
                        concentration_counts[1,] <- substances[selected_substances[i],]
                        concentration_counts[2,] <- transcriptome_1_fill[selected_genes[j],]
                        concentration_counts[3,] <- transcriptome_2_fill[selected_genes[j],]
                        
                        row.names(concentration_counts) <- c(selected_substances[i], paste(selected_genes[j], transcriptome_name_1, sep = "."), paste(selected_genes[j], transcriptome_name_2, sep = "."))
                        colnames(concentration_counts) <- Time
                        
                        write.csv(concentration_counts, paste(output_name, "_", selected_substances[i], "_", selected_genes[j], ".csv", sep = ""))
                        
                      
                    } else {
                      
                        sink(paste(output_name, selected_genes[j], "_selected_genes_time_series_plot.txt", sep = ""))
                        print("Your selected gene shows no transcripts. The transcript count is at each time point 0, so no plot could be generated.")
                        print("Selected gene:")
                        print(selected_genes[j])
                        sink()
                    }
                  
                }
              }
              
          }  
          
      }
      
      end_time <- Sys.time()
      
      
      
      
      
      paste("Total time consumed for the analysis:", round(end_time - start_time, 2), attr((end_time - start_time), "unit")) 