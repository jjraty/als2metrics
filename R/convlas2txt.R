#' convlas2txt: Convert and combine las/laz files to a format compatible with the als2metrics functions
#'
#' \code{convlas2txt} is used to convert multiple las/laz files into the txt format used in the als2metrics and ai2metrics functions.
#' @param las_folder Path of folder that contains las files by plots/units. 
#' File names must have numeric ID separable by "_". String.
#' @param outfile Output file, full name including .txt suffix. String.
#' @param parse_element An indicator referring to the ID part or parts of file name. Numeric or a vector of numeric.
#' @param rgbn_cols An indicator vector referring to the columns added 
#' to the places of R, G, B, NIR in the output file (see the format in the Return section). 
#'  Numeric vector or NULL
#' @param sub_clip A list including arguments for sub-clipping, e.g. circular plot. Note that no overlap checks are carried out.
#' The code assumes that there exists a polygon for sub-clipping for each plot/unit file. The linkage between las/laz files
#' and polygons are made based on IDs (id_col). The sub_id_col argument is used if several polygons per txt file exist.
#'  An example argument: list(path_pols = "C:/Temp/circ_plots.gpkg", id_col = 1, sub_id_col = 2, crs = 3067).
#'  Note: id_col and sub_id_col strings will be concatenated using "_".
#' @param drop_tailc Drops some columns that may be redundant in some applications. This saves memory.
#' This drops terraclass, numberofreturns, return number and RGBNIR. Logical.
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
#' With a sub-clipping
#' convertlas2txt(las_folder = "C:/Temp/las_plots", # files: 1_plot.las, 2_plot.las... 
#'               outfile = "C:/Temp/las_plots/plotpoints.txt", 
#'               parse_element = 1,                # 2_plot.las, parse_element value 1 collects ID = 2 from the name of the las file.
#'               rgbn_cols = c(16, 17, 18, 19),    # For example. If NULL, function returns values -1 for the last four columns.
#'               sub_clip = list(path_pols = "C:/Temp/circ_plots.gpkg", id_col = 1, crs = 3067)) # id in the first column
#' )
#' convertlas2txt(las_folder = "C:/Temp/las_plots", # files: 1_plot.las, 2_plot.las... 
#'               outfile = "C:/Temp/las_plots/plotpoints.txt", 
#'               parse_element = c(1,2),                # plot_3_plot.las, parse_element specified as c(1, 2) returns "plot_3" for the processing
#'               sub_clip = list(path_pols = "C:/Temp/circ_plots.gpkg", id_col = 1, sub_id_col = 3, crs = 3067)) # id in the first column, one pol per tile
#'               # sub_id_col is used if there are several clips per plot
#' @import rlas
#' @import data.table
#' @import sf
#' @export
#'
# JR 10 Feb 2023
convlas2txt <- function(las_folder = NULL, 
                        outfile = NULL, 
                        parse_element = 1, 
                        rgbn_cols = NULL,
                        sub_clip = NULL, 
                        drop_tailc = FALSE) {
  if (is.null(las_folder) |
      is.null(outfile) | 
      is.null(parse_element)) {
    stop("Error. Define the arguments of the als2metric function.")
  }
  
  if (!is.logical(drop_tailc)) {
    stop("Error: Invalid drop_tailc, should be logical.")
  }
  
  if (file.exists(outfile)) {
    stop("Error. Outfile exists. Rename or remove.")
  }
  
  if (!is.null(sub_clip)) {
    clip_polys <- st_read(paste0(sub_clip$path_pols), quiet = TRUE)
  }
  
  fpattern <- ".las|.laz"
  las_fs <- list.files(las_folder, pattern = fpattern)
  # String id, omit file extension: Note later on that R converts string to num
  # while comparing but not opposite!
  ids <- paste0(sapply(las_fs, function(x) {
    idw_ext <- paste0(unlist(strsplit(x, split = "_"))[parse_element], 
                      collapse = "_")
    idwo_ext <- unlist(strsplit(idw_ext, split = fpattern))[1]
    return(idwo_ext)}))
  
  for (i in 1:length(las_fs)) {
      
    if (i == 1 | (i %% 20 == 0)) {
      cat("Converting las/laz to a single txt file...", i, "/", length(las_fs), fill = TRUE)
    }
    
    # Check if not polygons in a tile; skip if no; check id columns
    if (!any(as.character(clip_polys[[sub_clip$id_col]]) %in% ids[i])) {
      cat(paste0("Warning: No polygons overlapping with a tile: ", ids[i]), 
          fill = TRUE)
      next
    }
      
    las_f <- read.las(paste0(las_folder, "/" ,las_fs[i]))
    
    if (any(rgbn_cols > dim(las_f)[2])) {
      stop(paste0("Error. rgbn_cols indices out of bounds:", las_fs[i]))
    }
    
    # Construct plot output txts from LAS file
    txt_out <- data.table(plot_cell_id = rep(ids[i], dim(las_f)[1]),
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
                           txt_out$retnum < txt_out$numofret)] <- 2 # inte.,mid
    # drop some cols if requrested, saves memory
    if (drop_tailc) {
      txt_out <- txt_out[, 1:7]
    }
    
    # sub Clip 
    if (!is.null(sub_clip)) {
      point_geom <- st_as_sf(txt_out, coords = c("x", "y"), crs = sub_clip$crs, 
                             remove = FALSE)
      # Polygons used for clipping, keep cols
      polys <- subset(clip_polys, as.character(clip_polys[[sub_clip$id_col]]) == 
                                               unique(txt_out$plot_cell_id))
      # check if many, use sub_col_id
      if (dim(polys)[1] > 1) {
        if (is.null(sub_clip$sub_id_col) | !is.numeric(sub_clip$sub_id_col)) {
          stop("Error: Invalid sub_id_col arg!")
        }
        # spatial join, code written without dplyr...
        sub_txt <- st_join(x = point_geom, y = polys) 
        # Pick up ID, column index, append
        sub_txt$plot_cell_id <- sub_txt[[which(names(sub_txt) == 
                                      names(clip_polys)[sub_clip$sub_id_col])]]

        sub_txt <- sub_txt[, names(txt_out)]
        sub_txt <- st_drop_geometry(sub_txt)
        sub_txt <- sub_txt[!is.na(sub_txt$plot_cell_id), ] # Select desired
        # update plot_cell_id: concatenate col_id and sub_col_id
        sub_txt$plot_cell_id <- paste0(ids[i], "_", sub_txt$plot_cell_id)
          
        if (length(unique(sub_txt$plot_cell_id)) != dim(polys)[1]) {
          cat(paste0("WARNING: Sub-clipping failed, no points found. Perhaps ", 
                     "mismatching IDs between las and polygon files?"), 
              fill = TRUE)
          next
        }
        # write
        fwrite(sub_txt, outfile, col.names = FALSE,
               row.names = FALSE, append = TRUE, sep = " ")
      } else {
        sub_c <- st_intersects(x = st_geometry(point_geom), y = polys)
        
        txt_out <- txt_out[lengths(sub_c) > 0, ] 
        if (dim(txt_out)[1] == 0) {
          cat(paste0("WARNING: Sub-clipping failed. Perhaps ", 
                     "mismatch IDs between las and polygyon files?"), 
              fill = TRUE)
          next
        }
        # update id with cat when sub_id_col used
        if (!is.null(sub_clip$sub_id_col) & !is.na(sub_clip$sub_id_col)) { 
          sub_id_name <- polys[[sub_clip$sub_id_col]][
                          which(polys[[sub_clip$id_col]] == ids[i])]
          # update plot_cell_id: concatenate col_id and sub_col_id
          txt_out$plot_cell_id <- paste0(ids[i], "_", sub_id_name)
        }
        
        fwrite(txt_out, outfile, col.names = FALSE, 
               row.names = FALSE, append = TRUE, sep = " ")
      }
      

    } else { # No subclip requested
      fwrite(txt_out, outfile, col.names = FALSE, 
             row.names = FALSE, append = TRUE, sep = " ")
    }
  }
  # Return path to the output file
  return(outfile)
}
