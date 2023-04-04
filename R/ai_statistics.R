
first_stats_ai <- function(input_part, cutoff){
  
  input_part <- subset(input_part, input_part[, 5] >= cutoff)
  
  Rmax <- max(input_part[, dim(input_part)[2] - 3])
  Rmin <- min(input_part[, dim(input_part)[2] - 3])
  Rstd <- sd(input_part[, dim(input_part)[2] - 3])
  Rmean <- mean(input_part[, dim(input_part)[2] - 3])
  
  Gmax <- max(input_part[, dim(input_part)[2] - 2])
  Gmin <- min(input_part[, dim(input_part)[2] - 2])
  Gstd <- sd(input_part[, dim(input_part)[2] - 2])
  Gmean <- mean(input_part[, dim(input_part)[2] - 2])
  
  Bmax <- max(input_part[, dim(input_part)[2] - 1])
  Bmin <- min(input_part[, dim(input_part)[2] - 1])
  Bstd <- sd(input_part[, dim(input_part)[2] - 1])
  Bmean <- mean(input_part[, dim(input_part)[2] - 1])
  
  Nmax <- max(input_part[, dim(input_part)[2]])
  Nmin <- min(input_part[, dim(input_part)[2]])
  Nstd <- sd(input_part[, dim(input_part)[2]])
  Nmean <- mean(input_part[, dim(input_part)[2]])
  
  # ratios
  R_NImrat <- Rmean / Nmean
  G_NImrat <- Gmean / Nmean
  B_NImrat <- Bmean / Nmean
  R_Gmrat <- Rmean / Gmean
  R_Bmrat <- Rmean / Bmean
  G_Bmrat <- Gmean / Bmean
  
  R_NIstdrat <- Rstd / Nstd
  G_NIstdrat <- Gstd / Nstd
  B_NIstdrat <- Bstd / Nstd
  R_Gstdrat <- Rstd / Gstd
  R_Bstdrat <- Rstd / Bstd
  G_Bstdrat <- Gstd / Bstd
  
  return(cbind(Rmax, Rmin, Rstd, Rmean, Gmax, Gmin, Gstd,
               Gmean, Bmax, Bmin, Bstd, Bmean, Nmax, Nmin, Nstd, Nmean,
               R_NImrat, G_NImrat, B_NImrat, R_Gmrat, R_Bmrat, G_Bmrat,
               R_NIstdrat, G_NIstdrat, B_NIstdrat, 
               R_Gstdrat, R_Bstdrat, G_Bstdrat))
  
}

datasets_ai <- function(data){
  
    first <- subset(data, data[, 7] == 0 | data[, 7] == 1)
    #assign("first_data", first, envir = .GlobalEnv)
    return(first)
}



