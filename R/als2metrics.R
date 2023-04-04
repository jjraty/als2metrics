########################################################
#15.02.2017
#Eetu Kotivuori, Mikko Kukkonen, Janne R?ty & Petteri Packalen
########################################################
#' als2metrics: Extract metrics from an airborne LiDAR point cloud
#'
#' \code{als2metrics} is used to extract metrics from an airborne LiDAR point cloud
#' @param pointcloud Specify an ALS file (text file), Format: plot_cell_id; x; y; z; dz, i; 
#' echotype; flightline; terraclass; GPS-time (delimeter: space). 
#' First seven columns must be in the abovementioned order, additional columns are optional.
#' Echo type codes: 0 = Only echo, 1 = First of many echo, 
#' 2 = Intermediate echo, 3 = Last of many echo. String.
#' @param first Output metrics computed based on  first echoes (first + only)
#' (rows). It is recommended to use row and column names. Logical.
#' @param last Output metrics computed based on  last echoes (last + only). Logical.
#' @param intermediate Output metrics computed based on intermediate echoes (intermediate). Logical.
#' @param all Output metrics computed based on ALL echoes (first, last, only and intermediate). Logical.
#' @param ecat_prop Calculate mean and standard deviation of heights and the proportion of echoes categories. Logical.
#' @param basic_stats Calculates mean, std, med, min, max, skew, kurt.Skew and kurt computed using the functions of the *moments* package. Logical.
#' @param quantiles A vector of percentiles, e.g using seq(...) function. Percentiles are calculated using quantile() -function (using default type=7)
#' @param densities  Calculates densities, i.e. echo proportion under or equal to the determined height value. Logical.
#' @param densities_ft The height values fixed height densities are computed. Vector.
#' @param intensity_stats Calculates mean_int, std_int, med_int, min_int, max_int, skew_int, kurt_int
#' @param intensity_q Calculate quantiles in the same manner as for dZ values ('quantiles' must be defined). Logical.
#' @param cutoff Cut off all echoes smaller or equal to the given threshold value. Numeric.
#' @param min_echo_n Minimum number of echoes to compute metrics. Numeric.
#' @param output A path of the output file, including file name as a .txt format. String.
#' 
#' @return \code{als2metrics} Prints a .txt file to the user-defined path.
#'
#' @examples
#'als2metrics(pointcloud = "C:/Temp/lidar_data.txt",     # A Path of an airborne LiDAR data file
#'            first = TRUE,                        			 # Compute first echo metrics
#'            last = TRUE,                        			 # Compute last echo metrics
#'            intermediate = TRUE,                       # Compute intermediate echo metrics
#'            all = TRUE,                                # Compute all echo metrics
#'            ecat_prop = TRUE,                          # Compute proportions of echo categories
#'            basic_stats = TRUE,                        # Compute basic statistics
#'            quantiles = seq(0.05, 0.95, 0.05),         # Vector of percentiles
#'            densities = TRUE,                          # Compute densities
#'            densities_ft = c(0.5, 2, 5, 10, 15, 20),   # Vector of heights in fixed height densities
#'            intensity_stats = TRUE,                    # Compute intensity statistics
#'            intensity_q = TRUE,                        # Compute intensity percentiles
#'            cutoff = 0.0,                              # Cutoff threshold
#'            min_echo_n = 10,                           # Minimum number of echoes
#'            output = "lidar_metrics.txt"               # Output file 
#' )
#' @import moments
#' @import data.table
#' @export
#' 
als2metrics <- function(pointcloud = NULL, 
                        first = TRUE, 
                        last = TRUE, 
                        intermediate = TRUE, 
                        all = TRUE, 
                        ecat_prop = TRUE,
                        basic_stats = TRUE,
                        quantiles = seq(0.05, 0.95, 0.05),
                        densities = TRUE,
                        densities_ft = c(0.5, 2, 5, 10, 15, 20),
                        intensity_stats = TRUE,
                        intensity_q = TRUE, 
                        cutoff = 0.0, 
                        min_echo_n = 10, 
                        output = NULL,
                        verbose = TRUE){
  
  if (is.null(pointcloud) |
      is.null(output)) {
    stop("Error. Define the arguments of the als2metric function.")
  }
  
  if (!is.numeric(cutoff) | !is.numeric(min_echo_n)) {
    stop("Error. Invalid cutoff or min_echo_n.")
  } 
  
  if (cutoff < 0 | min_echo_n <= 1) {
    stop("Error. Cutoff must be greated than 0 and min_echo_n greater than 1.")
  }
  
  
  # Start the clock!
  start <- proc.time()
  
  ########################################################
  #################THE PROGRAM############################
  ########################################################
  if (is.null(quantiles)) {
    comp_quantiles <- FALSE
  } else {
    comp_quantiles <-  TRUE
  }
  
  ety <- c(first, last, intermediate, all)
  # #Package for skewness and kurtosis
  # if(!is.element("moments", installed.packages()[,1]) == T)
  #   install.packages("moments")
  # require(moments)
  # 
  # #for subsetting and data reading performance
  # if(!is.element("data.table", installed.packages()[,1]) == T)
  #   install.packages("data.table")
  # require(data.table)
  
  ########################################################
  #reading "subprograms"
  # 
  # source("basic_statistics.R")
  # source("main_function.R")
  # source("percentiles.R")
  # source("density.R")
  
  intensity_stat <- intensity_yesORnot(intensity_stats)
  intensity_perc <- intensity_yesORnot(intensity_q)
  
  #########READING ALS DATA###############################
  
  #data <- fread(pointcloud, header = FALSE, drop=c("V8","V9","V10"))
  data <- fread(pointcloud, header = FALSE) # JR: drop might cause problems.
  data <- as.data.frame(data)
  
  #########Finding unique PLOT/CELL ids###################
  
  plot_cell_id <- sort(unique(data[,1]))
  
  ########################################################
  #NEGATIVE VALUES TO ZERO
  data[,5][data[,5] < 0] <- 0.00
  
  chosen <- vektori(ety,data)
  ecat_data <- datasets(ety, data)
  
  ########################################################
  ########################################################
  
  if(first == TRUE)
    first_data <- as.data.table(ecat_data$first_data)
  
  if(last == TRUE)
    last_data <- as.data.table(ecat_data$last_data)
  
  if(intermediate == TRUE)
    intermediate_data <- as.data.table(ecat_data$intermediate_data)
  
  data <- as.data.table(data)
  
  ########################################################
  #####MAIN PROGRAM#######################################
  
  plot_cell_results <- NULL
  
  for(j in 1: length(chosen)){
    
    idcount <- length(plot_cell_id)
    for (i in 1:length(plot_cell_id)){
   
      if (verbose & (i == idcount)) {
        cat("Processing -", chosen[j],"- completed", i,
            "of", idcount, fill = TRUE)
        flush.console()
      }
      
      if (verbose &  ((i %% 100) == 0) & (i != idcount)) {
        cat("Processing -", chosen[j],"- completed", i,
            "of", idcount, fill = TRUE)
        flush.console()
      }

      if(chosen[j] != "all_data"){
        data_part <- get(chosen[j])[V1==plot_cell_id[i]] #V1 contains the ID
        data_part <- as.data.frame(data_part)
        
      }else{data_part = NULL}
      DATA <- data[V1 == plot_cell_id[i]]   #V1 contains the ID
      DATA <- as.data.frame(DATA)
      
      if (is.null(data_part)){
        if (length(which(DATA[,5] > cutoff)) > (min_echo_n)){
          final_out <- switches_function(cutoff, quantiles, densities_ft, DATA,
                                         data_part,intensity_stat,
                                         intensity_perc, basic_stats, ecat_prop,
                                         comp_quantiles, densities, chosen[j])
        } else{
          
          dummydata <- matrix(nrow = (min_echo_n + 100), ncol = 6)
          dummydata[,5] <- seq(1,(min_echo_n + 100), by = 1) + min_echo_n
          dummydata[,6] <- seq(1,(min_echo_n + 100), by = 1) + min_echo_n
          data_part2 <- dummydata
          final_out <- switches_function(cutoff, quantiles, densities_ft, 
                                         dummydata, data_part2,intensity_stat,
                                         intensity_perc, basic_stats, ecat_prop, 
                                         comp_quantiles, densities, chosen[j])
          final_out[,1:dim(final_out)[2]] <- -9999
          
        }}
      
      if(!is.null(data_part)){
        if(length(which(data_part[,5] > cutoff)) > min_echo_n){
          final_out <- switches_function(cutoff, quantiles, densities_ft, 
                                         DATA, data_part,intensity_stat,
                                         intensity_perc, basic_stats, ecat_prop,
                                         comp_quantiles, densities, chosen[j])
        }else{
          dummydata <- matrix(nrow = (min_echo_n + 100), ncol = 6)
          dummydata[,5] <- seq(1,(min_echo_n + 100), by = 1) + min_echo_n
          dummydata[,6] <- seq(1,(min_echo_n + 100), by = 1) + min_echo_n
          data_part2 <- dummydata

          final_out <- switches_function(cutoff, quantiles, densities_ft, 
                                         dummydata, data_part2,intensity_stat,
                                         intensity_perc, basic_stats, ecat_prop, 
                                         comp_quantiles, densities, chosen[j])
          final_out[,1:dim(final_out)[2]] <- -9999
        }}
      
      plot_cell_results <- rbind(plot_cell_results, final_out)
    }
    plot_cell_results <- cbind(plot_cell_id, plot_cell_results)
    assign(paste(chosen[j],"_output",sep = ""), plot_cell_results)
    plot_cell_results = NULL
  }
  #####Writing output tables###############################
  
  final_table <- data.frame(plot_cell_id)
  
  if (first == TRUE){
    col <- colnames(first_data_output)
    names <- paste("f",col, sep="_")
    colnames(first_data_output) <- c(names)
    final_table <- merge(final_table, first_data_output,
                         by.x = "plot_cell_id", by.y= "f_plot_cell_id")
  }
  
  if (last == TRUE){
    col <- colnames(last_data_output)
    names <- paste("l",col, sep="_")
    colnames(last_data_output) <- c(names)
    final_table <- merge(final_table, last_data_output, 
                         by.x = "plot_cell_id", by.y= "l_plot_cell_id")
  }
  
  if (all == TRUE){
    col <- colnames(all_data_output)
    names <- paste("a",col, sep="_")
    colnames(all_data_output) <- c(names)
    final_table <- merge(final_table, all_data_output, 
                         by.x = "plot_cell_id", by.y= "a_plot_cell_id")
  }
  
  if (intermediate == TRUE){
    col <- colnames(intermediate_data_output)
    names <- paste("i",col, sep="_")
    colnames(intermediate_data_output) <- c(names)
    final_table <- merge(final_table, intermediate_data_output, 
                         by.x = "plot_cell_id", by.y= "i_plot_cell_id")
  }

  write.table(round(final_table,4), output, 
              row.names = FALSE, quote = FALSE, sep=" ")
  if (verbose) {
    cat("Process time:", proc.time()[3] - start[3], "seconds", fill = TRUE)
  }
  
}
















