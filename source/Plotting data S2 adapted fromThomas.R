# **(1) Load Required Libraries**
packages <- c('dplyr', 'ggplot2', 'caret', 'tidyverse')

# Install missing packages
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

# Load packages
invisible(lapply(packages, library, character.only = TRUE))

# **(2) Define Output Folder**
output_folder <- "output/sen2"
if (!dir.exists(output_folder)) {
  dir.create(output_folder, recursive = TRUE)
}

# **(3) Read Training & Test Datasets**
data_train <- read.csv("E:/Thesis Thomas/2024_D'hulsterThomas_Detectie van vitaliteitsverlies grove den/results/detectie vitaliteit met satellietbeelden/Sentinel-2/trainingset.csv")
data_test <- read.csv("E:/Thesis Thomas/2024_D'hulsterThomas_Detectie van vitaliteitsverlies grove den/results/detectie vitaliteit met satellietbeelden/Sentinel-2/testset.csv")

# **(4) Unified Preprocessing Function (Applies to Both Datasets)**
preprocess_data <- function(df) {
  df <- df %>%
    filter(!is.na(vitklasse) & KB > 0.9) %>%  # Remove NAs and filter KB > 0.9
    mutate(vitality = case_when(
      vitklasse %in% c(1, 2) ~ "vital",
      vitklasse == 3 ~ "non-vital"
    )) %>%
    mutate(vitality = as.factor(vitality))  # Ensure vitality is a factor

  return(df)
}

# **(5) Apply Preprocessing**
data_train <- preprocess_data(data_train)
data_test <- preprocess_data(data_test)

# **(6) Downsample the Training Set to Balance Classes**
set.seed(42)
data_balanced <- downSample(
  x = data_train[, !names(data_train) %in% "vitality"],
  y = data_train$vitality
)

# Rename target variable back to 'vitality'
colnames(data_balanced)[ncol(data_balanced)] <- "vitality"
data_balanced$vitality <- as.factor(data_balanced$vitality)

print("✅ Training dataset downsampled to balance class distribution.")

# **(7) Rename Sentinel-2 Bands for Clarity**
rename_bands <- function(df) {
  df <- df %>%
    rename(
      B2_summer = B2mean, B3_summer = B3mean, B4_summer = B4mean, B5_summer = B5mean,
      B6_summer = B6mean, B7_summer = B7mean, B8_summer = B8mean, B11_summer = B11mean, B12_summer = B12mean,
      B2_winter = B2psmean, B3_winter = B3psmean, B4_winter = B4psmean, B5_winter = B5psmean,
      B6_winter = B6psmean, B7_winter = B7psmean, B8_winter = B8psmean, B11_winter = B11psmean, B12_winter = B12psmean
    )
  return(df)
}

data_balanced <- rename_bands(data_balanced)
data_test <- rename_bands(data_test)

# **(8) Apply Scaling (Offset Correction)**
scale_bands <- function(df) {
  sentinel_bands <- grep("^B[0-9]+_(summer|winter)$", names(df), value = TRUE)
  df <- df %>%
    mutate(across(all_of(sentinel_bands), ~ (. - 1000) / 10000))
  return(df)
}

data_balanced <- scale_bands(data_balanced)
data_test <- scale_bands(data_test)

# **(9) Compute Vegetation Indices**
compute_vegetation_indices <- function(df) {
  df <- df %>%
    mutate(
      NDVI_summer = (B8_summer - B4_summer) / (B8_summer + B4_summer),
      NDVI_winter = (B8_winter - B4_winter) / (B8_winter + B4_winter),
      SAVI_summer = 1.5 * (B8_summer - B4_summer) / (B8_summer + B4_summer + 0.5),
      SAVI_winter = 1.5 * (B8_winter - B4_winter) / (B8_winter + B4_winter + 0.5),
      GNDVI_summer = (B8_summer - B3_summer) / (B8_summer + B3_summer),
      GNDVI_winter = (B8_winter - B3_winter) / (B8_winter + B3_winter),
      NDRE_summer = (B8_summer - B5_summer) / (B8_summer + B5_summer),
      NDRE_winter = (B8_winter - B5_winter) / (B8_winter + B5_winter),
      NDMI_summer = (B8_summer - B12_summer) / (B8_summer + B12_summer),
      NDMI_winter = (B8_winter - B12_winter) / (B8_winter + B12_winter)
    )
  return(df)
}

data_balanced <- compute_vegetation_indices(data_balanced)
data_test <- compute_vegetation_indices(data_test)

print("✅ Vegetation indices computed for both training and test datasets.")

# **(10) Convert Data to Long Format for Visualization**
index_list <- c("NDVI_summer", "SAVI_summer", "GNDVI_summer", "NDRE_summer", "NDMI_summer",
                "NDVI_winter", "SAVI_winter", "GNDVI_winter", "NDRE_winter", "NDMI_winter")

data_indices <- data_balanced %>%
  select(vitality, all_of(index_list)) %>%
  pivot_longer(cols = -vitality, names_to = "Index", values_to = "Value") %>%
  mutate(
    Season = ifelse(grepl("_summer", Index), "Summer", "Winter"),
    Index = gsub("_summer|_winter", "", Index),
    Index = factor(Index, levels = c("NDVI", "SAVI", "GNDVI", "NDRE", "NDMI")),
    Season = factor(Season, levels = c("Summer", "Winter"))
  )

# **(11) Generate and Save Histograms**
plot_index_histogram <- function(df, index_name, filename) {
  p <- ggplot(df %>% filter(Index == index_name), aes(x = Value, fill = vitality)) +
    geom_histogram(alpha = 0.7, position = "identity", aes(y = ..density..), color = "black", bins = 30) +
    geom_density(alpha = 0.5) +
    facet_wrap(~Season, scales = "free_y") +
    labs(x = paste(index_name, "Value"), y = "Density", fill = "Vitality") +
    scale_fill_manual(values = c("lightsalmon4", "olivedrab4")) +
    theme_bw(base_size = 18) +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      legend.position = "top",
      strip.background = element_blank(),
      strip.text = element_text(size = 16, face = "bold")
    )

  ggsave(filename = file.path(output_folder, filename), plot = p, width = 10, height = 6, dpi = 600)
}

plot_index_histogram(data_indices, "NDVI", "histogram_NDVI.png")
plot_index_histogram(data_indices, "NDRE", "histogram_NDRE.png")

print("✅ NDVI & NDRE histograms saved successfully in 'output/sen2' at 600 DPI.")

# **(7) Prepare Data for Line Plot with Shaded Area (IQR)**
band_stats <- data_balanced %>%
  pivot_longer(cols = c(B1, B2, B3, B4), names_to = "Band", values_to = "Reflectance") %>%
  group_by(Band, vitklasse) %>%
  summarise(
    median = median(Reflectance),
    p25 = quantile(Reflectance, 0.25),
    p75 = quantile(Reflectance, 0.75),
    .groups = "drop"
  )

# **(8) Generate Line Plot with Shaded Area**
p <- ggplot(band_stats, aes(x = Band, y = median, group = vitklasse, color = vitklasse, fill = vitklasse)) +
  geom_line(size = 1.2) +  # Median line
  geom_ribbon(aes(ymin = p25, ymax = p75), alpha = 0.3, linetype = 0) +  # Shaded area (IQR)
  labs(x = "Spectral bands", y = "Median reflectance (with IQR)", color = "Vitality Class", fill = "Vitality Class") +
  scale_color_manual(values = c("lightsalmon4", "olivedrab4")) +
  scale_fill_manual(values = c("lightsalmon4", "olivedrab4")) +
  theme_bw(base_size = 18) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "top",
    strip.background = element_blank(),
    strip.text = element_text(size = 16, face = "bold")
  )

ggsave(filename = file.path(output_folder, "lineplot_bands_vitality.png"), plot = p, width = 8
       , height = 6, dpi = 600)

print("✅ Line plot with shaded IQR saved successfully in 'output/planet' at 600 DPI.")
