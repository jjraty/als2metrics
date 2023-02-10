#' convlas2txt: Convert and combine las/laz files to a format compatible with the als2metrics functions
#'
#' \code{convlas2txt} is used to convert multiple las/laz files into the txt format used in the als2metrics and ai2metrics functions.
#' @param las_folder Path of folder that contains las files by plots/units. 
#' File names must have numeric ID separable by "_". String.
#' @param outfile Output file, full name including .txt suffix. String.
#' @param parse_element An indicator referring to the ID part of file name. Numeric.
#' @param rgbn_cols An indicator vector referring to the columns added 
#' to the places of R, G, B, NIR in the output file (see the format in the Return section). 
#'  Numeric vector or NULL
#' @return \code{convlas2txt} outputs a .txt file to the user-defined path. 
#' The output file follows the format: id, x, y, z, dz, i, echotype, 
#' flightline, terraclass, numofret, retnum, R, G, B, NIR.
#' Returns the outfile path (String) that can be passed to the other als2metrics functions.
#'
#' @examples
#'convertlas2txt(las_folder = "C:/Temp/las_plots", # files: 1_plot.las, 2_plot.las... 
#'               outfile = "C:/Temp/las_plots/plotpoints.txt", 
#'               parse_element = 1,                # 2_plot.las, parse_element value 1 collects ID = 2 from the name of the las file.
#'               rgbn_cols = c(16, 17, 18, 19))    # For example. If NULL, function returns values -1 for the last four columns.
#' )
#' @import rlas
#' @import data.table
#' @export
#' 
#'
# JR 10 Feb 2023
convlas2txt <- function(las_folder = NULL, 
                           outfile = NULL, 
                           parse_element = 1, 
                           rgbn_cols = c(1, 2, 5, 6)) {
  if (is.null(las_folder) |
      is.null(outfile) | 
      is.null(parse_element)) {
    stop("Error. Define the arguments of the als2metric function.")
  }
  
  if (file.exists(outfile)) {
    stop("Error. Outfile exists. Rename or remove.")
  }
  
  las_fs <- list.files(las_folder, pattern = ".las|.laz")
  ids <- sapply(las_fs, function(x) {
                as.numeric(unlist(strsplit(x, split = "_"))[parse_element])
                })
  
  for (i in 1:length(las_fs)) {
      
    if (i == 1 | (i %% 20 == 0)) {
      cat("Converting las/laz to a single txt file...", i, "/", length(las_fs), fill = TRUE)
    }
      
    las_f <- read.las(paste0(las_folder, "\\" ,las_fs[i]))
    
    if (any(rgbn_cols > dim(las_f)[2])) {
      stop(paste0("Error. rgbn_cols indices out of bounds:", las_fs[i]))
    }
    
    # Construct plot clip output txts from LAS file
    txt_out <- data.table(id = rep(as.numeric(ids[i]), dim(las_f)[1]),
                          x = las_f$X,
                          y = las_f$Y, 
                          z = ifelse("Zref" %in% names(las_f), 
                                     las_f[["Zref"]], -1),
                          dZ = las_f$Z,
                          int = las_f$Intensity,
                          etype = 0,
                          flightline = las_f$EdgeOfFlightline,
                          terraclass = las_f$Classification,
                          numofret = las_f$NumberOfReturns,
                          retnum = las_f$ReturnNumber,
                          R = ifelse(!is.null(rgbn_cols), 
                                     las_f[[names(las_f)[rgbn_cols[1]]]], -1),
                          G = ifelse(!is.null(rgbn_cols), 
                                     las_f[[names(las_f)[rgbn_cols[2]]]], -1),
                          B = ifelse(!is.null(rgbn_cols), 
                                     las_f[[names(las_f)[rgbn_cols[3]]]], -1),
                          NIR = ifelse(!is.null(rgbn_cols), 
                                     las_f[[names(las_f)[rgbn_cols[4]]]], -1)) 
    
    # Update etype col (0 = ONLY, 1 = FOM, 2 = MID, 3 = LOM)
    txt_out$etype[txt_out$numofret == 1] <- 0 # ONLY
    txt_out$etype[txt_out$numofret > 1 & 
                    txt_out$retnum == txt_out$numofret] <- 3 # last of many
    txt_out$etype[txt_out$numofret > 1 & 
                    txt_out$retnum == 1] <- 1 # first of many
    txt_out$etype[txt_out$numofret > 2 & 
                        (txt_out$retnum != 1 & 
                           txt_out$retnum < txt_out$numofret)] <- 2 # interm.,mid

    fwrite(txt_out, outfile, col.names = FALSE, 
           row.names = FALSE, append = TRUE, sep = " ")
  }
  # Return path to the output file
  return(outfile)
}
