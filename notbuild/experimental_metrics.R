# Experimental metrics, Documentation missing, metric documentation
# commented in the code.
experimental_metrics <- function(ALSDATA, output) {
  als_data <- fread(paste0(ALSDATA))
  # Loop over plots and compute some additional metrics
  # Cover indices: FCI; LCI
  # average square height: qav: sum(X^2)/n
  # vc with 1 and 2 meter bins
  unique_plots <- unique(als_data$V1)
  
  out_frame <- data.frame(plot_cell_id = unique_plots, 
                          fci_korhetal2011_1.3m = 0, 
                          lci_korhetal2011_1.3m = 0,
                          sci_solbergetal2009 = 0,
                          dns_1.3m = 0,
                          cov_1.3m = 0,
                          f_gav = 0,
                          l_gav = 0,
                          i_gav = 0,
                          vc_1m = 0,
                          vc_2m = 0)
  
  for (i in 1:length(unique_plots)) {
    plot_points <- als_data[V1 == unique_plots[i], ]
    
    first <- plot_points[V7 == 0 | V7 == 1]
    last <- plot_points[V7 == 0 | V7 == 3]
    mid <- plot_points[V7 == 2]
    
    # gav
    gav <- function (x) {sum(x^2) / length(x)}
    
    # fci Korhonen et al. 2011
    fci_lci <- function (als, threshold = 1.3) {
      # fci
      R_Tsingle <- nrow(als[V7 == 0 & V5 >= threshold])
      R_Tfirst <- nrow(als[V7 == 1 & V5 >= threshold])
      R_allsingle <- nrow(als[V7 == 0])
      R_allfirst <- nrow(als[(V7 == 1)])
      
      # lci
      R_Tlast <- nrow(als[V7 == 3 & V5 >= threshold])
      R_alllast <- nrow(als[(V7 == 3)])
      
      return(data.frame(fci = (R_Tsingle + R_Tfirst) / (R_allsingle + R_allfirst),
                        lci = (R_Tsingle + R_Tlast) / (R_allsingle + R_alllast)))
    }
    
    cov_dns <- function (als, threshold = 1.3) {
      Tfirst <- nrow(als[(V7 == 1 | V7 == 0) & V5 >= threshold])
      all_first <- nrow(als[(V7 == 1 | V7 == 0)])
      
      all_T <-   nrow(als[V5 >= threshold])
      all <- nrow(als)
      
      return(data.frame(cov = Tfirst / all_first * 100, 
             dns = all_T / all * 100))
    }
    
    solbergcover <- function(als) {
      single_ground <- nrow(als[V9 == 2 & V7 == 0])
      single_all <- nrow(als[V7 == 0])
      first_ground <- nrow(als[V9 == 2 & V7 == 1])
      last_ground <- nrow(als[V9 == 2 & V7 == 3])
      first_all <- nrow(als[V7 == 1])
      last_all <- nrow(als[V7 == 3])
      
      upper <- single_ground + 0.5 * (first_ground + last_ground)
      bottom <- single_all + 0.5 * (first_all + last_all)
      
      return(1 - upper / bottom)
    }
    
    vc <- function(als, bin = 1) {
      # establish height bins
      # Define max bin
      if (bin == 1) {
        maxbin <- floor(max(als$V5))
      } else {
        maxbin <- floor(max(als$V5))
        if (maxbin %% bin != 0) {
          while (maxbin %% 2 != 0) {
            maxbin <- maxbin - 1
          }
        }
      }
      
      if (maxbin >= bin) {
        bins <- cut(als$V5, breaks = seq(0, maxbin, bin), right = FALSE, 
                    labels = (seq(0, (maxbin - bin), bin) + bin / 2))
        # remove NAs that are outside the "complete" bins
        bins <- bins[!is.na(bins)]
        fracts <- as.numeric(table(bins) / sum(table(bins)))
        
        nbins <- length(fracts)
        
        vc <- -sum(fracts * log(fracts) / log(nbins))
      } else {
        vc <- NaN
      }
      
      return(vc)
    }
    
    
    out_frame[i, "fci_korhetal2011_1.3m"] <- fci_lci(als = plot_points, threshold = 1.3)$fci
    out_frame[i, "lci_korhetal2011_1.3m"] <- fci_lci(als = plot_points, threshold = 1.3)$lci
    out_frame[i, "sci_solbergetal2009"] <- solbergcover(als = plot_points)
    out_frame[i, "dns_1.3m"] <- cov_dns(als = plot_points, threshold = 1.3)$dns
    out_frame[i, "cov_1.3m"] <- cov_dns(als = plot_points, threshold = 1.3)$cov
    out_frame[i, "f_gav"] <- gav(x = first$V5)
    out_frame[i, "l_gav"] <- gav(x = last$V5)
    out_frame[i, "i_gav"] <- gav(x = mid$V5)
    out_frame[i, "vc_1m"] <- vc(als = plot_points, bin = 1)
    out_frame[i, "vc_2m"] <-   vc(als = plot_points, bin = 2)
  }
  write.table(out_frame, paste0(output), col.names = TRUE, 
              row.names = F, quote = F)
}

