
# Load libraries ----------------------------------------------------------
library(pacman)
p_load(fs, glue, terra)


# Set directories ---------------------------------------------------------
group <- 'coastal' #'arctic', 'aridlands', boreal_forest', 'western_forests'
yr <- '2055'
season <- 'breeding'
dir <- glue('./outputs/{group}')
sp_dirs <- list.dirs(dir, full.names = TRUE)
species <- basename(sp_dirs)
pattern <- '.*breeding_2055_85_ENSEMBLE_classifiedchange.tif$'

files <- list.files(path = sp_dirs, pattern = pattern, 
                      recursive = TRUE, full.names = TRUE)
  
fls <- files[!grepl('nonbreeding', files)]
# Read each matching raster file
raster_list<- lapply(fls, rast)
rclmat <- matrix(c(
   0, 0, 0,  # 0 = 'never'
   1, 1, 2,   # 1 = 'extirpation'
   7, 7, 1,#7 = 'colonization'
   2, 6, 3#  3, stable
), ncol = 3, byrow = TRUE)

# Process the raster files as needed
processed_rasters <- function(raster_list, output_dir){
  pro_ras <- lapply(seq_along(raster_list), function(i){
   
    #reclassify the raster
    r <- terra::classify(raster_list[[i]], rclmat)
    #clamp to values to a maximum of 3
    r <- terra::clamp(r, upper = 3)
    browser()
    name <- names(r)
    
    # Extract parts of the raster name for the new file name
    parts <- strsplit(name, "_")[[1]]
    new_name <- glue("{parts[1]}_{parts[3]}_{parts[5]}_colonizationextirpation.tif")
    # parts <- strsplit(name, '_')
    # new_name <- glue('{parts[1]}_{parts[2]}_{parts[4]}_{parts[5]}_{parts[6]}_{parts[7]}_colonizationextirpation.tif')
    # 
    # Save the reclassified raster with the new file name
    output_file <- file.path(output_dir, new_name)
    writeRaster(r, filename = output_file, overwrite = TRUE)
    return(r)
  })
}
 output_dir <- './ras_fut'

processed_rasters <- processed_rasters(raster_list[1:2], output_dir = output_dir)
