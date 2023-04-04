#' ai2metrics: Extraction of aerial image metrics from spectral values linked to 3D point cloud 
#' 
#' \code{ai2metrics} is used to extract metrics (mean, sd, min , max) from 
#' the last 4 columns of an input point cloud. The code is intended
#' for the calculation of spectral metrics fetched from multispectral 
#' aerial images (R, G, B, NIR). The ai2metrics function uses just first echoes.
#' @param pointcloud Specify a point cloud file (text file, delimited with space),
#' Column structure: plot_cell_id, x, y, z, dz, i, echotype, 
#' flightline, terraclass, GPS-time, R, G, B, NIR. String. 
#' Echo type codes: 0 = Only echo, 1 = First of many echo, 
#' 2 = Intermediate echo, 3 = Last of many echo.
#' Note that the ai2metrics computes the metrics based on the last four columns, 
#' and they have fixed column names used in the names of output metrics: R, G, B, NIR.
#' @param cutoff Cut off all echoes smaller or equal to the given threshold value. Numeric.
#' @param min_echo_n Minimum number of echoes to compute metrics. Numeric.
#' @param output A path of the output file, including file name as a .txt format. String.
#' 
#' @return \code{ai2metrics} prints a .txt file to the user-defined path.
#'
#' @examples
#'ai2metrics("C:/Temp/lidar_data.txt",    # Point cloud
#'           2,				                    # Cutoff threshold
#'           10,                        	# Minimum number of echoes
#'           "C:/Temp/ai_metrics.txt"     # Output file having ALS metrics
#')
#' @import data.table
#' @export
#' 
ai2metrics <- function(pointcloud = NULL, cutoff = 0, 
                       min_echo_n = 10, output = NULL, verbose = TRUE){
  
  if (is.null(pointcloud) | is.null(output)) {
    stop("Error. Define the arguments of the ai2metric function.")
  }
  if (!is.numeric(cutoff) | !is.numeric(min_echo_n)) {
    stop("Error. Invalid cutoff or min_echo_n.")
  } 
  
  if (cutoff < 0 | min_echo_n <= 1) {
    stop("Error. Cutoff must be greated than 0 and min_echo_n greater than 1.")
  }
  # Start the clock!
  start <- proc.time()
  #########READING ALS DATA###############################
  
  #data <- fread(pointcloud, header = FALSE, drop = c("V8","V9","V10"))
  data <- fread(pointcloud, header = FALSE) # JR: drop might cause problems.
  data <- as.data.frame(data)
  
  #########Finding unique PLOT/CELL ids###################
  
  plot_cell_id <- sort(unique(data[, 1]))
  
  ########################################################
  #NEGATIVE VALUES TO ZERO
  data[, 5][data[, 5] < 0] <- 0.00
  
  # source("ai_statistics.R")
  
  first_data <- datasets_ai(data)
  remove(data)
  first_data <- as.data.table(first_data)
  
  ####MAIN PROGRAM
  plot_cell_results <- NULL
  
  for (i in 1:length(plot_cell_id)){
    

    if (verbose & ((i %% 100) == 0) & i != length(plot_cell_id)) {
      cat("Processing -", 
          "first_data - completed", i, "of", length(plot_cell_id), fill = TRUE)
      flush.console()
    } 
    
    if (verbose & (i == length(plot_cell_id))) {
      cat("Processing -", 
          "first_data - completed", i, "of", length(plot_cell_id), fill = TRUE)
      flush.console()
    } 
    
   
    DATA <- first_data[V1 == plot_cell_id[i]]   #V1 contains the ID
    DATA <- as.data.frame(DATA)
    
    
    if (length(which(DATA[, 5] > cutoff)) > (min_echo_n)) {
      final_out <- first_stats_ai(DATA, cutoff)
      plot_id <- plot_cell_id[i]
      final_out <- cbind(plot_id, final_out)
      
    } else {
      temp <- matrix(nrow = (100), ncol = 6) # Dummy data to get columns...
      
      temp[, ncol(temp)] <- seq(1, (100), by = 1)
      temp[, ncol(temp) - 1] <- seq(1, (100), by = 1)
      temp[, ncol(temp) - 2] <- seq(1, (100), by = 1)
      temp[, ncol(temp) - 3] <- seq(1, (100), by = 1)
      
      final_out <- first_stats_ai(temp, cutoff)
      plot_id <- plot_cell_id[i]
      final_out[1, ] <- rep(-9999, dim(final_out)[2])
      final_out <- cbind(plot_id, final_out)
    
    }
    plot_cell_results <- rbind(plot_cell_results, final_out)
  }
  
  write.table(round(plot_cell_results, 4), output, 
               row.names = FALSE, quote = FALSE, sep = " ")
  if (verbose) {
    cat("Process time:", proc.time()[3] - start[3], "seconds", fill = TRUE)
  }
}




