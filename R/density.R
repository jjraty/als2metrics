
density <- function(input, interval){
  
  density = c()
  
  for(i in 1: length(interval))
    density[i] <- length(which(input[,5] <= interval[i])) / dim(input)[1]

    density <- as.data.frame(t(as.data.frame(density)))
        
    names <- interval
    names <- noquote(paste("d", names, sep=""))

    colnames(density) <- c(names)
    
    return(as.matrix(density))
  }
