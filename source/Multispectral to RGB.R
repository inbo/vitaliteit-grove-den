# Load required package
library(terra)

# Define input and output directories
input_folder <- "E:/Thesis Thomas/2024_D'hulsterThomas_Detectie van vitaliteitsverlies grove den/Field data/uitgesneden bosbestanden/orthomozaiek"
output_folder <- "E:/Thesis Thomas/2024_D'hulsterThomas_Detectie van vitaliteitsverlies grove den/Field data/uitgesneden bosbestanden/RGB"

# Create output directory if it doesn't exist
if (!dir.exists(output_folder)) dir.create(output_folder, recursive = TRUE)

# Get list of all GeoTIFF files in input folder
tif_files <- list.files(input_folder, pattern = "\\.tif$", full.names = TRUE)

# Loop through each file and extract RGB bands
for (file in tif_files) {
  # Load raster
  orthophoto <- rast(file)

  # Check number of bands
  if (nlyr(orthophoto) >= 3) {
    # Extract bands in R-G-B order (Red = 3, Green = 2, Blue = 1)
    rgb_image <- orthophoto[[c(3, 2, 1)]]

    # Modify filename: replace "geclipt_" with "RGB_"
    filename <- basename(file)  # Get only filename
    new_filename <- sub("^Geclipt_", "RGB_", filename)  # Replace prefix

    # Define output file path
    output_file <- file.path(output_folder, new_filename)

    # Save RGB image
    writeRaster(rgb_image, output_file, overwrite=TRUE)

    # Print status
    cat("Processed:", filename, "->", new_filename, "\n")
  } else {
    cat("Skipping (not enough bands):", basename(file), "\n")
  }
}

cat("All processing complete!\n")
