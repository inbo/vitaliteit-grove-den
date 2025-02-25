# **(1) Load Required Libraries**
packages <- c('dplyr', 'ggplot2', 'caret', 'tidyverse', 'randomForest')

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
      vitality = case_when(
        percdood >= 0.10 ~ "non_vital",  # ✅ Replace "-" with "_" for caret compatibility
        percdood < 0.10  ~ "vital"
      ),
      vitality = factor(vitality, levels = make.names(c("vital", "non_vital")))  # ✅ Ensure valid names
    )
  return(df)
}

# **(5) Apply Preprocessing**
data_train <- preprocess_data(data_train)
data_test <- preprocess_data(data_test)

# **(6) Scale Reflectance Values (Divide by 10,000)**
scale_bands <- function(df) {
  df <- df %>%
    mutate(
      B1 = X_b1_mean / 10000,
      B2 = X_b2_mean / 10000,
      B3 = X_b3_mean / 10000,
      B4 = X_b4_mean / 10000
    ) %>%
    select(B1, B2, B3, B4, vitality)  # Keep only relevant columns
  return(df)
}

data_train <- scale_bands(data_train)
data_test <- scale_bands(data_test)

# **(7) Downsample the Training Set to Balance Classes**
set.seed(42)
data_balanced <- downSample(
  x = data_train[, !names(data_train) %in% "vitality"],  # Features only
  y = data_train$vitality  # Target variable
)

# Rename target variable back to "vitality"
colnames(data_balanced)[ncol(data_balanced)] <- "vitality"
data_balanced$vitality <- as.factor(data_balanced$vitality)

print("✅ Training dataset downsampled to balance class distribution.")

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

# **(9) Normalize Training & Test Sets**
features <- c("NDVI", "GNDVI", "B1", "B2", "B3", "B4")

process <- preProcess(data_balanced %>% select(all_of(features)), method = "range")
train_scaled <- predict(process, data_balanced %>% select(all_of(features)))
train_scaled$vitality <- data_balanced$vitality

test_scaled <- predict(process, data_test %>% select(all_of(features)))
test_scaled$vitality <- data_test$vitality

# **(10) Train Random Forest Model**
set.seed(400)

repeat_cv <- trainControl(
  method = "repeatedcv",
  number = 10,
  repeats = 5,
  classProbs = TRUE,
  verboseIter = TRUE  # ✅ Show progress in console
)

forest <- train(
  vitality ~ .,
  data = train_scaled,
  method = "rf",
  trControl = repeat_cv,
  metric = "Kappa",
  tuneLength = 5
)

# **(11) Evaluate Model on Test Set**
predictions <- predict(forest, test_scaled)
conf_matrix <- confusionMatrix(predictions, test_scaled$vitality)
print(conf_matrix)

# **(12) Save Model & Variable Importance**
saveRDS(forest, file = file.path(output_folder, "random_forest_model.rds"))

var_imp <- varImp(forest, scale = TRUE)$importance
var_imp <- data.frame(variables = row.names(var_imp), importance = var_imp$Overall)

write.csv(var_imp, file = file.path(output_folder, "variable_importance.csv"), row.names = FALSE)

# **(13) Assign Colors to Bands & Indices**
var_imp$category <- case_when(
  grepl("^B[1-4]$", var_imp$variables) ~ "Band",   # PlanetScope bands
  grepl("^NDVI|GNDVI$", var_imp$variables) ~ "Index"
)

# **(14) Generate Variable Importance Plot with Custom Colors**
p_varimp <- ggplot(var_imp %>% arrange(desc(importance)),
                   aes(x = reorder(variables, importance), y = importance, fill = category)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  xlab("Variables") +
  ylab("Importance") +
  scale_fill_manual(values = c("Band" = "purple4", "Index" = "mediumpurple")) +  # Darker for bands, lighter for indices
  theme_bw(base_size = 16) +
  theme(
    legend.title = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )

ggsave(filename = file.path(output_folder, "variable_importance_colored.png"),
       plot = p_varimp, width = 8, height = 6, dpi = 600)

print("✅ Random Forest model trained, evaluated, and variable importance plot saved.")

# **(15) Compute Confusion Matrix & Performance Metrics**
cm_table <- as.data.frame(conf_matrix$table)
colnames(cm_table) <- c("Actual", "Predicted", "Count")

# Convert to Traditional Confusion Matrix Format
cm_matrix <- xtabs(Count ~ Predicted + Actual, data = cm_table)
cm_df <- as.data.frame(as.table(cm_matrix))

# Convert to Wide Format
cm_wide <- cm_df %>%
  pivot_wider(names_from = Actual, values_from = Freq)

# Rename columns
colnames(cm_wide) <- c("Predicted Class", "Actual Vital", "Actual Non-Vital")

# Print Confusion Matrix
cat("\n✅ Confusion Matrix (Predicted in Rows, Actual in Columns):\n")
print(cm_wide)

# Extract Model Performance Metrics
accuracy <- conf_matrix$overall["Accuracy"]
kappa <- conf_matrix$overall["Kappa"]
precision <- conf_matrix$byClass["Pos Pred Value"]
recall <- conf_matrix$byClass["Sensitivity"]
f1_score <- conf_matrix$byClass["F1"]

# Print Results
cat("Overall Accuracy:", round(accuracy, 4), "\n")
cat("Kappa:", round(kappa, 4), "\n")
cat("Precision:", round(precision, 4), "\n")
cat("Recall (Sensitivity):", round(recall, 4), "\n")
cat("F1 Score:", round(f1_score, 4), "\n")
