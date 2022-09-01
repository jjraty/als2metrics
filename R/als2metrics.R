########################################################
#15.02.2017
#Eetu Kotivuori, Mikko Kukkonen, Janne Räty & Petteri Packalen
########################################################

als2metrics <- function(ALSFILE, FIRST, LAST, INTERMEDIATE, ALL_ECHOES, PROP_MEAN_ETYP,BASIC_STATISTICS,PERCENTILE_SCALE,
                        DENSITIES,DENSITIES_FIXED_TRESHOLD,INTENSITY_STATISTICS,INTENSITY_PERCENTILES, CUTOFF, MIN_ECHO_N, output){

  # Start the clock!
  start <- proc.time()
  
  ########################################################
  #################THE PROGRAM############################
  ########################################################
  PERCENTILES = T
  ety <- c(FIRST, LAST, INTERMEDIATE, ALL_ECHOES)
  #Package for skewness and kurtosis
  if(!is.element("moments", installed.packages()[,1]) == T)
    install.packages("moments")
  require(moments)
  
  #for subsetting and data reading performance
  if(!is.element("data.table", installed.packages()[,1]) == T)
    install.packages("data.table")
  require(data.table)
  
  ########################################################
  #reading "subprograms"
  
  source("basic_statistics.R")
  source("main_function.R")
  source("percentiles.R")
  source("density.R")
  
  intensity_stat <- intensity_yesORnot(INTENSITY_STATISTICS)
  intensity_perc <- intensity_yesORnot(INTENSITY_PERCENTILES)
  
  #########READING ALS DATA###############################
  
  data <- fread(ALSFILE, header = FALSE, drop=c("V8","V9","V10"))
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
















