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
#'  @param sub_clip A list including arguments for sub-clipping, e.g. circular plot. Note that no overlap checks are carried out.
#'  The code assumes that there exists a polygon for sub-clipping for each plot/unit file. The linkage between las/laz files
#'  and polygons are made based on IDs. An example argument: list(path_pols = "C:/Temp/circ_plots.gpkg", id_col = 1, crs = 3067).
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
#' @import rlas
#' @import data.table
#' @import sf
#' @export
#' 
#'
# JR 10 Feb 2023
convlas2txt <- function(las_folder = NULL, 
                        outfile = NULL, 
                        parse_element = 1, 
                        rgbn_cols = NULL,
                        sub_clip = NULL) {
  if (is.null(las_folder) |
      is.null(outfile) | 
      is.null(parse_element)) {
    stop("Error. Define the arguments of the als2metric function.")
  }
  
  if (file.exists(outfile)) {
    stop("Error. Outfile exists. Rename or remove.")
  }
  
  if (!is.null(sub_clip)) {
    clip_polys <- st_read(paste0(sub_clip$path_pols), quiet = TRUE)
  }
  
  las_fs <- list.files(las_folder, pattern = ".las|.laz")
  ids <- paste0(sapply(las_fs, function(x) {
                paste0(unlist(strsplit(x, split = "_"))[parse_element], 
                       collapse = "_")}))
  
  for (i in 1:length(las_fs)) {
      
    if (i == 1 | (i %% 20 == 0)) {
      cat("Converting las/laz to a single txt file...", i, "/", length(las_fs), fill = TRUE)
    }
      
    las_f <- read.las(paste0(las_folder, "\\" ,las_fs[i]))
    
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
                           txt_out$retnum < txt_out$numofret)] <- 2 # interm.,mid
    
    # sub Clip 
    if (!is.null(sub_clip)) {
      point_geom <- st_as_sf(txt_out, coords = c("x", "y"), crs = sub_clip$crs)
      sub_c <- st_intersects(x = st_geometry(point_geom), 
                                y = subset(st_geometry(clip_polys), 
                                           clip_polys[[ 
                             sub_clip$id_col]] == unique(txt_out$plot_cell_id))) 
      txt_out <- txt_out[lengths(sub_c) > 0, ] 
      if (dim(txt_out)[1] == 0) {
        stop(paste0("Sub-clipping failed. Perhaps ", 
                    "mismatch IDs between las and polygyon files?"))
      }
    }

    fwrite(txt_out, outfile, col.names = FALSE, 
           row.names = FALSE, append = TRUE, sep = " ")
    
  }
  # Return path to the output file
  return(outfile)
}
