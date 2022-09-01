
percentile_h <- function(input, cutoff, interval, variable){
    
    input <- subset(input, input[,5] >= cutoff)
  
    if(5 %in% variable & !(6 %in% variable)){
    
      h_per <- quantile(input[,5], interval)
      perc <- rbind(h_per)
      names <- interval * 100
    
      names <- noquote(paste("h", names, sep=""))
  
      colnames(perc) <- c(names)

      return(perc)

    }
    
    if(6 %in% variable){
      
      h_per <- quantile(input[,5], interval)
      h_per_i <- quantile(input[,6], interval)
      perc <- rbind(h_per)
      perc_i <- rbind(h_per_i)
      n <- interval * 100
      
      names <- noquote(paste("h", n, sep=""))
      names_i <- noquote(paste("int", n, sep=""))
      
      colnames(perc) <- c(names)
      colnames(perc_i) <- c(names_i)
      
      perc <- cbind(perc,perc_i)
      return(perc)
      
    }
}