#!/usr/bin/env Rscript
args <-  commandArgs(trailingOnly = TRUE)
#print(args)

# Check arguments
if (length(args) == 0 | (length(args) %% 2 != 0)) {
  stop(paste0("Please input arguments: input and output file missing ",
            "or argument missing for a defined parameter name."), call. = FALSE)
} else {
  # parse arguments by pairs, every second has name and every second is arg
  parsed_parnams <- args[seq(1, (length(args) - 1), 2)]
  parsed_args <- args[seq(2, (length(args)), 2)]

  # rm delivered nulls
  rm_s <- parsed_args == "-9999"
  parsed_parnams <- parsed_parnams[!rm_s]
  parsed_args <- parsed_args[!rm_s]
  
  allowd_parnam <- c("-i", "-o", "-hcut", "-q", "-d", 
                     "-aisw", "-gforce", "-gtoken", "-mecho",
                     "-localp", "-prepo") # fixed 
  allowd_argdefs <- list(input = NULL, 
                         output = NULL, 
                         hcut = 0, 
                         quantiles = seq(0.05, 0.95, 0.05), 
                         densities = c(0.5, 2, 5, 10, 15, 20), 
                         switch = 0,
                         force_ins = 0,
                         token = NULL,
                         min_echo = 10,
                         local_path = NULL,
                         pkg_repo = NULL) 
  
  for (i in 1:length(allowd_parnam)) {

    if (any(parsed_parnams == allowd_parnam[i])) { 
      if (allowd_parnam[i] == "-i" | 
          allowd_parnam[i] == "-o" | 
          allowd_parnam[i] == "-gtoken" |
          allowd_parnam[i] == "-localp" |
          allowd_parnam[i] == "-prepo") {
        allowd_argdefs[[i]] <- parsed_args[which(parsed_parnams == 
                                                  allowd_parnam[i])]
      }
      
      if (allowd_parnam[i] == "-hcut") {
        allowd_argdefs[[i]] <- as.numeric(parsed_args[which(parsed_parnams == 
                                                            allowd_parnam[i])])
      }
      
      if (allowd_parnam[i] == "-aisw" | allowd_parnam[i] == "-gforce" |
          allowd_parnam[i] == "-mecho") {
        allowd_argdefs[[i]] <- as.integer(parsed_args[which(parsed_parnams == 
                                                            allowd_parnam[i])])
      }
      
      if (allowd_parnam[i] == "-q" | allowd_parnam[i] == "-d") {
        allowd_argdefs[[i]] <- eval(parse(text = parsed_args[which(
                                      parsed_parnams == allowd_parnam[i])]))
      }
      
    } else if (any(!(parsed_parnams %in% allowd_parnam))) {
      cat("Warning: an unknown argument.", fill = TRUE)
    } else {
      next
    }
  }
}

if (is.null(allowd_argdefs$input) | is.null(allowd_argdefs$output)) {
  stop("Please define input, and output.")
}

# debug prints
# print(allowd_argdefs$input)
# print(allowd_argdefs$output)
# print(parsed_args)
#print(allowd_argdefs)
# print(args)
# 
# Run als2metrics; options for the computation of ALS or AI or Both
# metrics. The purpose of this script is to provide a user-interface via .sh

# Install package from github or from a local tar.gz
if (is.null(allowd_argdefs$local_path)) { # install from github
  list_of_packages <- c("als2metrics", "remotes")
  new_packages <- list_of_packages[!(list_of_packages %in%
            installed.packages(lib.loc = allowd_argdefs$pkg_repo)[, "Package"])]
  if (length(new_packages) > 0 | allowd_argdefs$force_ins == 1) {
    if (is.null(allowd_argdefs$token)) {
      stop(paste0("Installing for the first time or forced install == TRUE.", 
                  " Please provide your author token to install from",  
                  "a private repo. See 'pointcloud2metrics.sh -h'"))
    }
    if (any(new_packages %in% "remotes")) {
      install.packages("remotes", repos = "https://cloud.r-project.org", 
                       lib = ifelse(!is.null(allowd_argdefs$pkg_repo), 
                                    allowd_argdefs$pkg_repo, .libPaths()[1]))
    }
    
    if (any(new_packages %in% "als2metrics") | allowd_argdefs$force_ins == 1) {
    remotes::install_github("jjraty/als2metrics", ref = "main",
                            force = TRUE, auth_token = allowd_argdefs$token,
                            lib = ifelse(!is.null(allowd_argdefs$pkg_repo), 
                                      allowd_argdefs$pkg_repo, .libPaths()[1]))
    }
  }
  
  succ_bool <- lapply(list_of_packages, FUN = function(x) {
                          require(x, lib.loc = allowd_argdefs$pkg_repo, 
                                  character.only = TRUE)})
} else { # from a local, no need for remotes
  list_of_packages <- c("als2metrics", "remotes")
  new_packages <- list_of_packages[!(list_of_packages %in%
       installed.packages(lib.loc = allowd_argdefs$pkg_repo)[, "Package"])]
  if (length(new_packages) > 0 | allowd_argdefs$force_ins == 1) {
    
    if (any(new_packages %in% "remotes")) {
      install.packages("remotes", repos = "https://cloud.r-project.org", 
                       lib = ifelse(!is.null(allowd_argdefs$pkg_repo), 
                                    allowd_argdefs$pkg_repo, .libPaths()[1]))
    }
    
    if (any(new_packages %in% "als2metrics") | allowd_argdefs$force_ins == 1) {
    remotes::install_local(allowd_argdefs$local_path,  
                     force = TRUE, repos = "https://cloud.r-project.org",
                     lib = ifelse(!is.null(allowd_argdefs$pkg_repo), 
                                  allowd_argdefs$pkg_repo, .libPaths()[1]))
    }
  }
  succ_bool <- lapply(list_of_packages, FUN = function(x) {
                    require(x, lib.loc = allowd_argdefs$pkg_repo, 
                            character.only = TRUE)})
}

# switch = 0, compute ALS metrics
# parameter names removed (they may be modified in the als2metrics code later..)
if (allowd_argdefs$switch == 0) {
  als2metrics(allowd_argdefs$input,          # A Path of a pc data file
              TRUE,                          # Compute first echo metrics
              TRUE,                          # Compute last echo metrics
              TRUE,                          # Compute intermediate echo metrics
              TRUE,                          # Compute all echo metrics
              TRUE,                 # Compute proportions of echo categories
              TRUE,                          # Compute basic statistics
              allowd_argdefs$quantiles,      # Vector of percentiles
              TRUE,                          # Compute densities
              allowd_argdefs$densities,      #  Vector of hin fix ht densities
              TRUE,                          # Compute intensity statistics
              TRUE,                          # Compute intensity percentiles
              allowd_argdefs$hcut,           # Cutoff threshold
              allowd_argdefs$min_echo,      # Minimum number of echoes
              allowd_argdefs$output)         # Output file having ALS metrics

} else if (allowd_argdefs$switch == 1) {            # compute ai metrics
  ai2metrics(allowd_argdefs$input,           # Point cloud
             allowd_argdefs$hcut,				     # Cutoff threshold
             allowd_argdefs$min_echo,       # Minimum number of echoes
             allowd_argdefs$output)          # Output file having ALS metrics

} else if (allowd_argdefs$switch == 2) { # compute both als and ai metrics
  # Create temporary outputs, will be removed and merged
  als_outfnam <- paste0(unlist(strsplit(allowd_argdefs$output, split = ".txt")),
                        "_pc.txt")
  ai_outfnam <- paste0(unlist(strsplit(allowd_argdefs$output, split = ".txt")),
                        "_ai.txt")
  als2metrics(allowd_argdefs$input,          # A Path of a pc data file
              TRUE,                          # Compute first echo metrics
              TRUE,                          # Compute last echo metrics
              TRUE,                          # Compute intermediate echo metrics
              TRUE,                          # Compute all echo metrics
              TRUE,                     # Compute proportions of echo categories
              TRUE,                          # Compute basic statistics
              allowd_argdefs$quantiles,      # Vector of percentiles
              TRUE,                          # Compute densities
              allowd_argdefs$densities,      #  Vector of hin fix ht densities
              TRUE,                          # Compute intensity statistics
              TRUE,                          # Compute intensity percentiles
              allowd_argdefs$hcut,           # Cutoff threshold
              allowd_argdefs$min_echo,      # Minimum number of echoes
              als_outfnam)                   # Output file having ALS metrics

  ai2metrics(allowd_argdefs$input,           # Point cloud
             allowd_argdefs$hcut,				     # Cutoff threshold
             allowd_argdefs$min_echo,       # Minimum number of echoes
             ai_outfnam)                     # Output file having ALS metrics

  # Merge metrics, remove separate files
  als_m <- read.table(als_outfnam, header = TRUE)
  ai_m <- read.table(ai_outfnam, header = TRUE)
  file.remove(c(als_outfnam, ai_outfnam))

  all_m <- merge(als_m, ai_m, by.x = "plot_cell_id", by.y = "plot_id")
  write.table(all_m, allowd_argdefs$output, col.names = TRUE,
              row.names = FALSE, quote = FALSE)
} else {
  stop("Error. Invalid switch argument. Allowed: 0, 1 and 2.")
}

cat("\n")