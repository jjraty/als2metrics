#' subcliptxt: Convert and combine txt files to a format compatible with the als2metrics functions
#'
#' \code{convlas2txt} is used to convert multiple txt files (tiles) into the txt format used in the als2metrics and ai2metrics functions.
#' The function also allows subclips based on polygons.
#' @param txt_folder Path of folder that contains las files by plots/units. 
#' File names must have numeric ID separable by "_". String.
#' @param outfile Output file, full name including .txt suffix. String.
#' @param parse_element An indicator referring to the ID part of file name. Numeric.
#' @param sub_clip A list including arguments for sub-clipping, e.g. circular plot. Note that no overlap checks are carried out.
#' The code assumes that there exists a polygon for sub-clipping for each plot/unit file. The linkage between las/laz files
#' and polygons are made based on IDs (id_col). The sub_id_col argument is used if several polygons per txt file exist.
#'  An example argument: list(path_pols = "C:/Temp/circ_plots.gpkg", id_col = 1, sub_id_col = 2, crs = 3067).
#' @return \code{subcli2txt} outputs a .txt file to the user-defined path. 
#' The output file follows the format: id, x, y, z, dz, i, echotype, 
#' flightline, terraclass, numofret, retnum, R, G, B, NIR.
#' Returns the outfile path (String) that can be passed to the other als2metrics functions.
#'
#' @examples
#'subcliptxt(txt_folder = "C:/Temp/las_plots", # files: 1_plot.txt, 2_plot.txt... 
#'               outfile = "C:/Temp/las_plots/plotpoints.txt", 
#'               parse_element = 1)                # 2_plot.las, parse_element value 1 collects ID = 2 from the name of the las file.
#' )
#' With a sub-clipping
#' subcliptxt(txt_folder = "C:/Temp/las_plots", # files: 1_plot.las, 2_plot.las... 
#'               outfile = "C:/Temp/las_plots/plotpoints.txt", 
#'               parse_element = 1,                # 2_plot.las, parse_element value 1 collects ID = 2 from the name of the las file.
#'               sub_clip = list(path_pols = "C:/Temp/circ_plots.gpkg", id_col = 1, crs = 3067)) # id in the first column, one pol per tile
#'               # sub_id_col is used if there are several clips per plot
#' )
#' @import data.table
#' @import sf
#' @export
#' 
#'
# JR 17 Feb 2023
subcliptxt <- function(txt_folder = NULL, 
                        outfile = NULL, 
                        parse_element = 1,
                        sub_clip = NULL) {
  if (is.null(txt_folder) |
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
  
  txt_fs <- list.files(txt_folder, pattern = ".txt")
  txt_fs_full <- list.files(txt_folder, pattern = ".txt", full.names = TRUE)
  ids <- sapply(txt_fs, function(x) {
    as.numeric(unlist(strsplit(x, split = "_"))[parse_element])
  })
  
  for (i in 1:length(txt_fs)) {
    
    if (i == 1 | (i %% 10 == 0)) {
      cat("Writing txt files to a single txt file...", i, "/", 
          length(txt_fs), fill = TRUE)
    }
    
    # Check if not polygons in a tile; skip if no; check id columns
    if (!any(clip_polys[[sub_clip$id_col]] %in% as.numeric(ids[i]))) {
      next
    }

    txt_f <- fread(paste0(txt_fs_full[i]))
    names <- colnames(txt_f)
    
    txt_f$plot_cell_id <- rep(as.numeric(ids[i]), dim(txt_f)[1])
    setcolorder(txt_f, neworder = c("plot_cell_id", names))
  
    # sub Clip 
    if (!is.null(sub_clip)) {
      point_geom <- st_as_sf(txt_f, 
                             coords = c("V1", "V2"), crs = sub_clip$crs, 
                             remove = FALSE)
      
      # Polygons used for clipping
      polys <- subset(clip_polys, 
                clip_polys[[sub_clip$id_col]] == unique(txt_f$plot_cell_id))
      
      # check if many, use sub_col_id
      if (dim(polys)[1] > 1) {
        # spatial join, code written without dplyr...
        sub_txt <- st_join(x = point_geom, y = polys) 
        sub_txt$plot_cell_id <- sub_txt[[which(names(sub_txt) == 
                                      names(clip_polys)[sub_clip$sub_id_col])]]
        sub_txt <- sub_txt[, names(txt_f)]
        sub_txt <- st_drop_geometry(sub_txt)
        sub_txt <- sub_txt[!is.na(sub_txt$plot_cell_id), ]
        
        if (length(unique(sub_txt$plot_cell_id)) != dim(polys)[1]) {
          stop(paste0("Sub-clipping failed, no points found. Perhaps ", 
                      "mismatching IDs between txt and polygon files?"))
        }
        # write
        fwrite(sub_txt, outfile, col.names = FALSE,
               row.names = FALSE, append = TRUE, sep = " ")
      } else {
        sub_c <- st_intersects(x = st_geometry(point_geom), 
                               y = polys) 
        txt_f <- txt_f[lengths(sub_c) > 0, ] 
        if (dim(txt_f)[1] == 0) {
          stop(paste0("Sub-clipping failed, no points found. Perhaps ", 
                      "mismatching IDs between las and polygon files?"))
        }
        fwrite(txt_f, outfile, col.names = FALSE, 
               row.names = FALSE, append = TRUE, sep = " ")
      }
    }
    
    if (is.null(sub_clip)) {
      fwrite(txt_f, outfile, col.names = FALSE, 
             row.names = FALSE, append = TRUE, sep = " ")
    }
    
  }
  # Return path to the output file
  return(outfile)
}
