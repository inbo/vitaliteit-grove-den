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
output_folder <- "output/planet"
if (!dir.exists(output_folder)) {
  dir.create(output_folder, recursive = TRUE)
}

# **(3) Read Training & Test Datasets**
data_train <- read.csv("E:/Thesis Thomas/2024_D'hulsterThomas_Detectie van vitaliteitsverlies grove den/results/detectie vitaliteit met satellietbeelden/PlanetScope/planettraining.csv")
data_test <- read.csv("E:/Thesis Thomas/2024_D'hulsterThomas_Detectie van vitaliteitsverlies grove den/results/detectie vitaliteit met satellietbeelden/PlanetScope/planettest.csv")

# **(4) Unified Preprocessing Function (Applies to Both Datasets)**
preprocess_data <- function(df) {
  df <- df %>%
    filter(!is.na(percdood)) %>%  # Remove NAs
    mutate(KB = (HISTO_1 + HISTO_0) / (HISTO_NODATA + HISTO_1 + HISTO_0)) %>%
    filter(KB > 0.9) %>%  # Quality filter
    mutate(
      vitklasse = case_when(
        percdood >= 0.10 ~ "non-vital",
        percdood < 0.10  ~ "vital"
      ),
      vitklasse = as.factor(vitklasse)
    )
  return(df)
}

# **(5) Apply Preprocessing**
data_train <- preprocess_data(data_train)
data_test <- preprocess_data(data_test)

# **(6) Downsample the Training Set to Balance Classes**
set.seed(42)
data_balanced <- downSample(
  x = data_train[, !names(data_train) %in% "vitklasse"],
  y = data_train$vitklasse
)

# Rename target variable back to 'vitklasse'
colnames(data_balanced)[ncol(data_balanced)] <- "vitklasse"
data_balanced$vitklasse <- as.factor(data_balanced$vitklasse)

print("✅ Training dataset downsampled to balance class distribution.")

# **(7) Scale Reflectance Values (Divide by 10,000)**
scale_bands <- function(df) {
  df <- df %>%
    mutate(
      B1 = X_b1_mean / 10000,
      B2 = X_b2_mean / 10000,
      B3 = X_b3_mean / 10000,
      B4 = X_b4_mean / 10000
    ) %>%
    select(B1, B2, B3, B4, vitklasse)  # Keep only relevant columns
  return(df)
}

data_balanced <- scale_bands(data_balanced)
data_test <- scale_bands(data_test)

# **(8) Compute Vegetation Indices**
compute_vegetation_indices <- function(df) {
  df <- df %>%
    mutate(
      NDVI = (B4 - B3) / (B4 + B3),
      GNDVI = (B4 - B2) / (B4 + B2)
    )
  return(df)
}

data_balanced <- compute_vegetation_indices(data_balanced)
data_test <- compute_vegetation_indices(data_test)

print("✅ Vegetation indices computed for both training and test datasets.")

# **(9) Convert Data to Long Format for Visualization**
index_list <- c("NDVI", "GNDVI")

data_indices <- data_balanced %>%
  select(vitklasse, all_of(index_list)) %>%
  pivot_longer(cols = -vitklasse, names_to = "Index", values_to = "Value") %>%
  mutate(Index = factor(Index, levels = c("NDVI", "GNDVI")))

# **(10) Generate and Save Histograms**
plot_index_histogram <- function(df, index_name, filename) {
  p <- ggplot(df %>% filter(Index == index_name), aes(x = Value, fill = vitklasse)) +
    geom_histogram(alpha = 0.7, position = "identity", aes(y = ..density..), color = "black", bins = 30) +
    geom_density(alpha = 0.5) +
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
plot_index_histogram(data_indices, "GNDVI", "histogram_GNDVI.png")

print("✅ NDVI & GNDVI histograms saved successfully in 'output/planet' at 600 DPI.")

# **(11) Train a Random Forest Model**
set.seed(400)
repeat_cv <- trainControl(method = 'repeatedcv', number = 5, repeats = 3)

# Prepare dataset for training
training_set <- data_balanced

set.seed(400)
forest <- train(
  vitklasse ~ .,
  data = training_set,
  method = 'rf',
  trControl = repeat_cv,
  metric = 'Kappa'
)

# Print model summary
forest$finalModel
p1 <- predict(forest, training_set)
confusionMatrix(forest)

# **(12) Feature Importance Plot**
var_imp <- varImp(forest, scale = TRUE)$importance
var_imp <- data.frame(variables = row.names(var_imp), importance = var_imp$Overall)

ggplot(var_imp, aes(x = reorder(variables, importance), y = importance)) +
  geom_bar(stat = 'identity', fill = "olivedrab4") +
  coord_flip() +
  xlab('Variables') +
  ylab('Importance') +
  theme_minimal()

# **(13) Predict on Test Data**
dftest <- data_test

set.seed(400)
y_hats <- predict(forest, newdata = dftest)

accuracy <- mean(y_hats == dftest$vitklasse) * 100
cat('Accuracy on testing data: ', round(accuracy, 2), '%', sep = '')

confusionMatrix(y_hats, as.factor(dftest$vitklasse))
plot(forest)

print("✅ Random Forest model trained and tested successfully.")
