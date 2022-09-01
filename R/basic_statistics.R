basic_statistics_h <- function(input, cutoff, variable){
  
  if(5 %in% variable & !(6 %in% variable)){
    abs_n <- length(input[,5])
    input <- subset(input, input[,5] >= cutoff)
    hmax <- max(input[,5])
    hmin <- min(input[,5])
    
    
    
    
    hstd <- sd(input[,5])
    hmed <- median(input[,5])
    hmean <- mean(input[,5])
    hskew <- skewness(input[,5])
    hkurt <- kurtosis(input[,5])
    
    return(cbind(hmax,hmin,hstd,hmed,hmean,abs_n, hskew, hkurt))
  }
  
  if(6 %in% variable){
    abs_n <- length(input[,5])
    input <- subset(input, input[,5] >= cutoff)

    hmax <- max(input[,5])
    hmin <- min(input[,5])
    hstd <- sd(input[,5])
    hmed <- median(input[,5])
    hmean <- mean(input[,5])
    hskew <- skewness(input[,5])
    hkurt <- kurtosis(input[,5])
    
    intmax <- max(input[,6])
    intmin <- min(input[,6])
    intstd <- sd(input[,6])
    intmed <- median(input[,6])
    intmean <- mean(input[,6])
    intskew <- skewness(input[,6])
    intkurt <- kurtosis(input[,6])
    
    return(cbind(hmax,hmin,hstd,hmed,hmean,abs_n,hskew, hkurt, intmax,intmin,intstd,intmed,intmean,intskew, intkurt))
   }
}


intermediate_stats <- function(input, input_part, cutoff){
  
  abs_n <- length(input_part[,5])
  
  input <- subset(input, input[,5] >= cutoff)
  input_part <- subset(input_part, input_part[,5] >= cutoff)
  
  hmean <- mean(input_part[,5])
  hstd <- sd(input_part[,5])
  echo_prop <- length(input_part[,5])/dim(input)[1] * 100
  
  return(cbind(abs_n,hmean,hstd, echo_prop))
  
}

first_stats <- function(input, input_part, cutoff){
  
  input <- subset(input, input[,5] >= cutoff)
  input_part <- subset(input_part, input_part[,5] >= cutoff)
  echo_prop <- length(input_part[,5])/dim(input)[1] * 100
  
  return(cbind(echo_prop))
  
}

last_stats <- function(input,input_part, cutoff){
  
  input <- subset(input, input[,5] >= cutoff)
  input_part <- subset(input_part, input_part[,5] >= cutoff)
  echo_prop <- length(input_part[,5])/dim(input)[1] * 100
  
  return(cbind(echo_prop))

  
}


