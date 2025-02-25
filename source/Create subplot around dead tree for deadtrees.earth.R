# Load required libraries
library(terra)
library(sf)

# Define folder paths
raster_folder <- "E:/Thesis Thomas/2024_D'hulsterThomas_Detectie van vitaliteitsverlies grove den/Field data/uitgesneden bosbestanden/RGB"
shapefile_folder <- "E:/2025_Bosvitaliteit/Code/vitaliteit-grove-den/data/Subset RGB deadtrees/punten"
output_folder <- "E:/2025_Bosvitaliteit/Code/vitaliteit-grove-den/data/Subset RGB deadtrees/clipped_RGB"

# Create output directory if it doesn't exist
if (!dir.exists(output_folder)) dir.create(output_folder, recursive = TRUE)

# Get list of all shapefiles in the 'punten' subfolder
shapefile_list <- list.files(shapefile_folder, pattern = "^Punt_.*\\.shp$", full.names = TRUE)

# Define buffer size in meters (10m in each direction → 20m x 20m bounding box)
buffer_size <- 10

# Loop through each shapefile
for (shapefile_path in shapefile_list) {

  # Extract the original orthophoto name from the shapefile
  shapefile_name <- basename(shapefile_path)  # Get only filename
  orthophoto_name <- sub("^Punt_", "", tools::file_path_sans_ext(shapefile_name))  # Remove "Punt_" prefix

  # Construct the expected raster filename
  raster_filename <- paste0("RGB_", orthophoto_name, ".tif")
  raster_path <- file.path(raster_folder, raster_filename)

  # Check if the corresponding raster exists
  if (file.exists(raster_path)) {

    # Load the RGB raster
    rgb_raster <- rast(raster_path)

    # Load the point shapefile
    point_sf <- st_read(shapefile_path, quiet = TRUE)

    # Ensure it has at least one point
    if (nrow(point_sf) > 0) {

      # Reproject point to Lambert72 (EPSG:31370) for accurate buffering
      point_sf_lambert72 <- st_transform(point_sf, crs = 31370)

      # Apply a 10m buffer (creates a 20m x 20m bounding box)
      buffered_sf_lambert72 <- st_buffer(point_sf_lambert72, dist = buffer_size)

      # Reproject buffered area back to WGS84 (EPSG:4326)
      buffered_sf_wgs84 <- st_transform(buffered_sf_lambert72, crs = 4326)

      # Convert to terra extent for cropping
      buffered_extent <- vect(buffered_sf_wgs84)  # Convert sf to terra object
      clip_extent <- ext(buffered_extent)  # Get the bounding box

      # Ensure the bounding box intersects with the raster before cropping
      raster_extent <- ext(rgb_raster)
      intersects <- !is.null(intersect(clip_extent, raster_extent))

      if (intersects) {
        # Crop the raster to the buffered extent
        clipped_raster <- crop(rgb_raster, clip_extent, snap="out")

        # Ensure the cropped raster has valid data
        if (nrow(clipped_raster) > 0 & ncol(clipped_raster) > 0) {

          # Define output file path
          output_file <- file.path(output_folder, paste0("Clipped_", orthophoto_name, ".tif"))

          # Save clipped raster
          writeRaster(clipped_raster, output_file, overwrite=TRUE)

          # Print status
          cat("Processed:", raster_filename, "->", output_file, "\n")
        } else {
          cat("Skipping (Clipping resulted in empty raster):", raster_filename, "\n")
        }
      } else {
        cat("Skipping (Point outside raster extent):", shapefile_name, "\n")
      }

    } else {
      cat("Skipping (empty shapefile):", shapefile_name, "\n")
    }
  } else {
    cat("Skipping (no matching raster for shapefile):", shapefile_name, "\n")
  }
}

cat("All processing complete!\n")
