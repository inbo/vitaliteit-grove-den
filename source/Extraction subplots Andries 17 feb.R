# Load necessary libraries
library(terra)  # For raster operations
library(sf)     # For vector operations
library(dplyr)  # For data manipulation

# 1. Define paths
sentinel_folder <- "E:/2025_Bosvitaliteit/Code/vitaliteit-grove-den/data/Sentinel2/"
output_folder <- "output/plots_Andries_Sen2_v2/"
raster_folder <- paste0(output_folder, "buffer_rasters/")
shapefile_folder <- paste0(output_folder, "shapefiles/")
boskartering_path <- "Z:/Vlaanderen/Natuur_Bos/Boskartering/bosvl2001.shp"
plots_path <- "data/Andries plots/plots Andries 17feb2025.shp"  # Path to plot center shapefile

# Create directories if they don't exist
dir.create(output_folder, showWarnings = FALSE, recursive = TRUE)
dir.create(raster_folder, showWarnings = FALSE, recursive = TRUE)
dir.create(shapefile_folder, showWarnings = FALSE, recursive = TRUE)

# 2. Load plot center points
plots_sf <- st_read(plots_path)
plots_sf$ID <- paste0("Plot", 1:nrow(plots_sf))

# 3. Load `bosvl2001`
bosvl2001 <- st_read(boskartering_path)

# 4. Load a Sentinel-2 tile to determine its CRS
tile_files <- list.files(sentinel_folder, pattern = "\\.tif$", full.names = TRUE)
s2_sample_tile <- rast(tile_files[1])  # Load a sample tile
sentinel_crs <- crs(s2_sample_tile)  # Get CRS of Sentinel-2 data

# 5. Reproject plot centers and `bosvl2001` to match Sentinel-2 CRS
plots_sf <- st_transform(plots_sf, crs = sentinel_crs)
bosvl2001 <- st_transform(bosvl2001, crs = sentinel_crs)

# 6. Create 500 m buffers around plot centers
buffers <- st_buffer(plots_sf, dist = 250)  # 250 m = half of 500 m

# 7. Process each plot
for (i in seq_along(plots_sf$geometry)) {
  point_geom <- plots_sf[i, ]  # Get the current plot

  # Find the Sentinel-2 tile containing this point
  tile_index <- which(sapply(tile_files, function(f) {
    tile <- rast(f)
    !is.na(terra::extract(tile, as.matrix(st_coordinates(point_geom)), cells = TRUE)[, 1])
  }))

  if (length(tile_index) > 0) {
    tile <- rast(tile_files[tile_index[1]])  # Select the first matching tile

    # Crop the Sentinel-2 tile to the buffer
    cropped_tile <- crop(tile, vect(buffers[i, ]))

    # Save the clipped raster
    raster_path <- paste0(raster_folder, plots_sf$ID[i], "_buffer.tif")
    writeRaster(cropped_tile, raster_path, overwrite = TRUE)

    # Convert raster to points (each pixel center)
    pixel_points <- as.data.frame(terra::xyFromCell(cropped_tile, 1:ncell(cropped_tile)))
    pixel_values <- values(cropped_tile)
    pixel_data <- cbind(pixel_points, pixel_values)
    pixel_data <- pixel_data[complete.cases(pixel_data), ]  # Remove NA values

    # Convert to sf object
    pixel_sf <- st_as_sf(pixel_data, coords = c("x", "y"), crs = sentinel_crs)

    # Select points that overlap with the correct `KLASSE`
    if (plots_sf$ID[i] == "Plot1") {
      selected_bosvl <- bosvl2001 %>% filter(KLASSE == 500)
    } else if (plots_sf$ID[i] == "Plot2") {
      selected_bosvl <- bosvl2001 %>% filter(KLASSE %in% c(500, 300))
    }

    selected_points <- st_intersection(pixel_sf, selected_bosvl)

    # Convert selected points to polygons
    pixel_size <- res(cropped_tile)[1]  # Sentinel-2 pixel size
    selected_polygons <- lapply(1:nrow(selected_points), function(j) {
      x <- st_coordinates(selected_points)[j, 1]
      y <- st_coordinates(selected_points)[j, 2]

      # Create square polygon around pixel center
      polygon_coords <- matrix(
        c(x - pixel_size / 2, y - pixel_size / 2,
          x + pixel_size / 2, y - pixel_size / 2,
          x + pixel_size / 2, y + pixel_size / 2,
          x - pixel_size / 2, y + pixel_size / 2,
          x - pixel_size / 2, y - pixel_size / 2),  # Close the polygon
        ncol = 2, byrow = TRUE
      )

      st_polygon(list(polygon_coords))
    })

    # Convert list to sf object
    selected_polygons_sf <- st_sf(
      geometry = st_sfc(selected_polygons, crs = sentinel_crs),
      KLASSE = selected_points$KLASSE,  # Retain original KLASSE values
      plot_id = rep(plots_sf$ID[i], length(selected_polygons))
    )

    # Reproject polygons back to Lambert72
    selected_polygons_sf <- st_transform(selected_polygons_sf, crs = 31370)

    # Systematically select 30 polygons (every nth pixel)
    if (nrow(selected_polygons_sf) > 30) {
      step <- floor(nrow(selected_polygons_sf) / 30)
      selected_polygons_sf <- selected_polygons_sf[seq(1, nrow(selected_polygons_sf), by = step), ][1:30, ]
    }

    # Save as shapefile
    shapefile_path <- paste0(shapefile_folder, plots_sf$ID[i], "_pixels.shp")
    st_write(selected_polygons_sf, shapefile_path, delete_layer = TRUE)

  } else {
    warning(paste("No Sentinel-2 tile found for plot ID", plots_sf$ID[i]))
  }
}

print("Processing completed: Buffer rasters saved, pixel polygons created, reprojected, systematically filtered, and saved.")

