intensity_yesORnot <- function(CALCULATE_INTENSITY_STATS){
if(CALCULATE_INTENSITY_STATS)
  intensity <- c(5,6)

if(!CALCULATE_INTENSITY_STATS)
  intensity <- c(5,0)

 return(intensity)
}


switches_function <- function(cutoff, PERCENTILE_SCALE, DENSITIES_FIXED_TRESHOLD, DATA, data_part,intensity_stats, 
                              intensity_perc,BASIC_STATISTICS,PROP_MEAN_ETYP,PERCENTILES,DENSITY,CHOSEN){
  
  if (CHOSEN == "first_data"){ 

    output_bas <- NULL
    output_inter <- NULL
    output_perc <- NULL
    output_dens <- NULL
    output_first <- NULL
  
    if(BASIC_STATISTICS == T)
      output_bas <- basic_statistics_h(data_part, cutoff, intensity_stats)
    
    if(PROP_MEAN_ETYP==T)
      output_first <- first_stats(DATA, data_part, cutoff)
    
    if(PERCENTILES==T) 
      output_perc <- percentile_h(data_part, cutoff, PERCENTILE_SCALE, intensity_perc)
  
    if(DENSITY == T)
      output_dens <- density(data_part, DENSITIES_FIXED_TRESHOLD)
  
    return(cbind(output_bas,output_perc, output_dens, output_first, output_inter))

  }else if (CHOSEN == "last_data" ){
    
      output_bas <- NULL
      output_inter <- NULL
      output_perc <- NULL
      output_dens <- NULL
      output_first <- NULL
      output_last <- NULL
        
      if(BASIC_STATISTICS == T)
        output_bas <- basic_statistics_h(data_part, cutoff, intensity_stats)
        
      if(PROP_MEAN_ETYP==T)
        output_last <- last_stats(DATA, data_part, cutoff)
        
        
      if(PERCENTILES==T) 
        output_perc <- percentile_h(data_part, cutoff, PERCENTILE_SCALE, intensity_perc)
        
      if(DENSITY == T)
          output_dens <- density(data_part, DENSITIES_FIXED_TRESHOLD)
          
        return(cbind(output_bas,output_perc, output_dens, output_first, output_last, output_inter))
        
  }else if(CHOSEN == "all_data") {
    
      output_bas <- NULL
      output_inter <- NULL
      output_perc <- NULL
      output_dens <- NULL
      output_first <- NULL
      output_last <- NULL
      output_veg <- NULL
     
      if(BASIC_STATISTICS == T)
        output_bas <- basic_statistics_h(DATA, cutoff, intensity_stats)
      
      if(PERCENTILES==T) 
        output_perc <- percentile_h(DATA, cutoff, PERCENTILE_SCALE, intensity_perc)
    
      
      if(DENSITY == T)
        output_dens <- density(DATA, DENSITIES_FIXED_TRESHOLD)
        
      return(cbind(output_bas,output_perc, output_dens, output_inter,output_veg))
      
  }else if (CHOSEN == "intermediate_data") {
        output_inter <- intermediate_stats(DATA, data_part, cutoff)
        return(cbind(output_inter))
  }
}
  
vektori <- function(echoTypes,data){
  
  veccc <- NULL
  
  if(echoTypes[1] == T){ # FIRST
    veccc <- append(veccc,"first_data")
  }
  if(echoTypes[2] == T){ # LAST
    veccc <- append(veccc,"last_data")
  }    
  if(echoTypes[3] == T){ # middle
    veccc <- append(veccc,"intermediate_data")
  }   
  if(echoTypes[4] == T){ # ALL
    veccc <- append(veccc,"all_data")
  }
  return(veccc)
}


datasets <- function(echoTypes,data){
  out_list <- list(first_data = NULL, 
                   last_data = NULL, 
                   intermediate_data = NULL)
  if(echoTypes[1] == TRUE){ # FIRST
    first <- subset(data, data[, 7] == 0 | data[, 7] == 1)
    out_list[["first_data"]] <- first
    #assign("first_data", first, envir = .GlobalEnv)
  }
  if(echoTypes[2] == TRUE){ # LAST
    last <- subset(data, data[, 7] == 0 | data[, 7] == 3)
    out_list[["last_data"]] <- last
    #assign("last_data", last, envir = .GlobalEnv)
  }    
  if(echoTypes[3] == TRUE){ # middle
    middle <- subset(data, data[, 7] == 2)
    out_list[["intermediate_data"]] <- middle
    #assign("intermediate_data", middle, envir = .GlobalEnv)
  }   
  
  return(out_list)
}

