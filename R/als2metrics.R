########################################################
#15.02.2017
#Eetu Kotivuori, Mikko Kukkonen, Janne R?ty & Petteri Packalen
########################################################
#' als2metrics: Extract metrics from an airborne LiDAR point cloud
#'
#' \code{als2metrics} is used to extract metrics from an airborne LiDAR point cloud
#' @param ALSFILE Specify an ALS file (text file), Format: plot_cell_id; x; y; z; dz, i; 
#' echotype; flightline; terraclass; GPS-time (delimeter: space). 
#' First seven columns must be in the abovementioned order, additional columns are optional.
#' Echo type codes: 0 = Only echo, 1 = First of many echo, 
#' 2 = Intermediate echo, 3 = Last of many echo. String.
#' @param FIRST Output metrics computed based on  FIRST echoes (first + only)
#' (rows). It is recommended to use row and column names. Logical.
#' @param LAST Output metrics computed based on  LAST echoes (last + only). Logical.
#' @param INTERMEDIATE Output metrics computed based on INTERMEDIATE echoes (intermediate). Logical.
#' @param ALL_ECHOES Output metrics computed based on ALL echoes (first, last, only and intermediate). Logical.
#' @param PROP_MEAN_ETYP Calculate mean and standard deviation of heights and the proportion of echoes categories. Logical.
#' @param BASIC_STATISTICS Calculates mean, std, med, min, max, skew, kurt. Logical.
#' @param PERCENTILE_SCALE A vector of percentiles, e.g using seq(...) function. Percentiles are calculated using quantile() -function (using default type=7)
#' @param DENSITIES  Calculates densities, i.e. echo proportion under or equal to the determined height value. Logical.
#' @param DENSITIES_FIXED_TRESHOLD The height values fixed height densities are computed. Vector.
#' @param INTENSITY_STATISTICS Calculates mean_int, std_int, med_int, min_int, max_int, skew_int, kurt_int
#' @param INTENSITY_PERCENTILES Calculate perecentiles in the same manner as for dZ values ('Compute percentiles' must be set to TRUE). Logical.
#' @param CUTOFF Cut off all echoes smaller or equal to the given threshold value. Numeric.
#' @param MIN_ECHO_N Minimum number of echoes to compute metrics. Numeric.
#' @param output A path of the output file, including file name as a .txt format. String.
#' 
#' @return \code{als2metrics} Prints a .txt file to the user-defined path.
#'
#' @examples
#'als2metrics(ALSFILE = "C:/Temp/lidar_data.txt",            # A Path of an airborne LiDAR data file
#'            FIRST = TRUE,                        			 # Compute first echo metrics
#'            LAST = TRUE,                        			 # Compute last echo metrics
#'            INTERMEDIATE = TRUE,                           # Compute intermediate echo metrics
#'            ALL_ECHOES = TRUE,                             # Compute all echo metrics
#'            PROP_MEAN_ETYP = TRUE,                         # Compute proportions of echo categories
#'            BASIC_STATISTICS = TRUE,                       # Compute basic statistics
#'            PERCENTILE_SCALE = seq(0.05, 0.95, 0.05),      # Vector of percentiles
#'            DENSITIES = TRUE,                              # Compute densities
#'            DENSITIES_FIXED_TRESHOLD = c(0.5, 2, 5, 10, 15, 20),    # Vector of heights in fixed height densities
#'            INTENSITY_STATISTICS = TRUE,                            # Compute intensity statistics
#'            INTENSITY_PERCENTILES = TRUE,                           # Compute intensity percentiles
#'            CUTOFF = 0.0,                                  # Cutoff threshold
#'            MIN_ECHO_N = 10,                               # Minimum number of echoes
#'            output = "lidar_metrics.txt"                   # Output file 
#' )
#' @import moments
#' @import data.table
#' @export
#' 
als2metrics <- function(ALSFILE = NULL, 
                        FIRST = NULL, 
                        LAST = NULL, 
                        INTERMEDIATE = NULL, 
                        ALL_ECHOES = NULL, 
                        PROP_MEAN_ETYP = NULL,
                        BASIC_STATISTICS = NULL,
                        PERCENTILE_SCALE = NULL,
                        DENSITIES = NULL,
                        DENSITIES_FIXED_TRESHOLD = NULL,
                        INTENSITY_STATISTICS = NULL,
                        INTENSITY_PERCENTILES = NULL, 
                        CUTOFF = NULL, 
                        MIN_ECHO_N = NULL, 
                        output = NULL){
  
  if (is.null(ALSFILE) |
      is.null(FIRST) | 
      is.null(LAST) |
      is.null(INTERMEDIATE) | 
      is.null(ALL_ECHOES) | 
      is.null(PROP_MEAN_ETYP) |
      is.null(BASIC_STATISTICS) |
      is.null(PERCENTILE_SCALE) |
      is.null(DENSITIES) |
      is.null(DENSITIES_FIXED_TRESHOLD) |
      is.null(INTENSITY_STATISTICS) |
      is.null(INTENSITY_PERCENTILES) | 
      is.null(CUTOFF) | 
      is.null(MIN_ECHO_N) | 
      is.null(output)) {
    stop("Error. Define the arguments of the als2metric function.")
  }
  
  
  # Start the clock!
  start <- proc.time()
  
  ########################################################
  #################THE PROGRAM############################
  ########################################################
  PERCENTILES = T
  ety <- c(FIRST, LAST, INTERMEDIATE, ALL_ECHOES)
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
  
  intensity_stat <- intensity_yesORnot(INTENSITY_STATISTICS)
  intensity_perc <- intensity_yesORnot(INTENSITY_PERCENTILES)
  
  #########READING ALS DATA###############################
  
  #data <- fread(ALSFILE, header = FALSE, drop=c("V8","V9","V10"))
  data <- fread(ALSFILE, header = FALSE) # JR: drop might cause problems.
  data <- as.data.frame(data)
  
  #########Finding unique PLOT/CELL ids###################
  
  plot_cell_id <- sort(unique(data[,1]))
  
  ########################################################
  #NEGATIVE VALUES TO ZERO
  data[,5][data[,5] < 0] <- 0.00
  
  chosen <- vektori(ety,data)
  datasets(ety, data)
  
  ########################################################
  ########################################################
  
  if(FIRST==TRUE)
    first_data <- as.data.table(first_data)
  
  if(LAST==TRUE)
    last_data <- as.data.table(last_data)
  
  if(INTERMEDIATE==TRUE)
    intermediate_data <- as.data.table(intermediate_data)
  
  data <- as.data.table(data)
  
  ########################################################
  #####MAIN PROGRAM#######################################
  
  plot_cell_results=NULL
  
  for(j in 1: length(chosen)){
    
    idcount <- length(plot_cell_id)
    for (i in 1:length(plot_cell_id)){
   
      if ( i == idcount ) {
        cat("\n","Processing -", chosen[j],"- completed", i, "of", idcount)
        flush.console()
      }
      else if ( (i %% 10) == 0 ) {
        cat("\n","Processing -", chosen[j],"- completed", i, "of", idcount)
        flush.console()
      }

      if(chosen[j] != "all_data"){
        data_part <- get(chosen[j])[V1==plot_cell_id[i]] #V1 contains the ID
        data_part <- as.data.frame(data_part)
        
      }else{data_part = NULL}
      DATA <- data[V1 == plot_cell_id[i]]   #V1 contains the ID
      DATA <- as.data.frame(DATA)
      
      if(is.null(data_part)){
        if(length(which(DATA[,5] > CUTOFF)) > (MIN_ECHO_N)){
          final_out <- switches_function(CUTOFF, PERCENTILE_SCALE, DENSITIES_FIXED_TRESHOLD, DATA, data_part,intensity_stat,
                                         intensity_perc, BASIC_STATISTICS, PROP_MEAN_ETYP, PERCENTILES, DENSITIES, chosen[j])
        }else{
          
          testidata <- matrix(nrow = (MIN_ECHO_N + 100), ncol = 6)
          testidata[,5] <- seq(1,(MIN_ECHO_N + 100), by = 1) + MIN_ECHO_N
          testidata[,6] <- seq(1,(MIN_ECHO_N + 100), by = 1) + MIN_ECHO_N
          data_part2 <- testidata
          final_out <- switches_function(CUTOFF, PERCENTILE_SCALE, DENSITIES_FIXED_TRESHOLD, testidata, data_part2,intensity_stat,
                                         intensity_perc, BASIC_STATISTICS, PROP_MEAN_ETYP, PERCENTILES, DENSITIES, chosen[j])
          final_out[,1:dim(final_out)[2]] <- -9999
          
        }}
      
      if(!is.null(data_part)){
        if(length(which(data_part[,5] > CUTOFF)) > MIN_ECHO_N){
          final_out <- switches_function(CUTOFF, PERCENTILE_SCALE, DENSITIES_FIXED_TRESHOLD, DATA, data_part,intensity_stat,
                                         intensity_perc, BASIC_STATISTICS, PROP_MEAN_ETYP, PERCENTILES, DENSITIES, chosen[j])
        }else{
          testidata <- matrix(nrow = (MIN_ECHO_N + 100), ncol = 6)
          testidata[,5] <- seq(1,(MIN_ECHO_N + 100), by = 1) + MIN_ECHO_N
          testidata[,6] <- seq(1,(MIN_ECHO_N + 100), by = 1) + MIN_ECHO_N
          data_part2 <- testidata

          final_out <- switches_function(CUTOFF, PERCENTILE_SCALE, DENSITIES_FIXED_TRESHOLD, testidata, data_part2,intensity_stat,
                                         intensity_perc, BASIC_STATISTICS, PROP_MEAN_ETYP, PERCENTILES, DENSITIES, chosen[j])
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
  
  if (FIRST == TRUE){
    col <- colnames(first_data_output)
    names <- paste("f",col, sep="_")
    colnames(first_data_output) <- c(names)
    final_table <- merge(final_table, first_data_output, by.x = "plot_cell_id", by.y= "f_plot_cell_id")
  }
  
  if (LAST == TRUE){
    col <- colnames(last_data_output)
    names <- paste("l",col, sep="_")
    colnames(last_data_output) <- c(names)
    final_table <- merge(final_table, last_data_output, by.x = "plot_cell_id", by.y= "l_plot_cell_id")
  }
  
  if (ALL_ECHOES == TRUE){
    col <- colnames(all_data_output)
    names <- paste("a",col, sep="_")
    colnames(all_data_output) <- c(names)
    final_table <- merge(final_table, all_data_output, by.x = "plot_cell_id", by.y= "a_plot_cell_id")
  }
  
  if (INTERMEDIATE == TRUE){
    col <- colnames(intermediate_data_output)
    names <- paste("i",col, sep="_")
    colnames(intermediate_data_output) <- c(names)
    final_table <- merge(final_table, intermediate_data_output, by.x = "plot_cell_id", by.y= "i_plot_cell_id")
  }

  write.table( round(final_table,4), output, row.names=F, quote=F, sep=" ")
  cat("\n","Process time:", proc.time()[3] - start[3], "seconds")
  
}
















