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
    mutate(
      vitality = case_when(
        vitklasse %in% c(1, 2) ~ "vital",
        vitklasse == 3 ~ "non-vital"
      ),
      vitality = factor(make.names(vitality), levels = make.names(c("vital", "non-vital")))  # ✅ Ensures valid factor names
    ) %>%

    # Rename Sentinel-2 bands for consistency
    rename(
      B2_summer = B2mean, B3_summer = B3mean, B4_summer = B4mean, B5_summer = B5mean,
      B6_summer = B6mean, B7_summer = B7mean, B8_summer = B8mean, B11_summer = B11mean, B12_summer = B12mean,
      B2_winter = B2psmean, B3_winter = B3psmean, B4_winter = B4psmean, B5_winter = B5psmean,
      B6_winter = B6psmean, B7_winter = B7psmean, B8_winter = B8psmean, B11_winter = B11psmean, B12_winter = B12psmean
    )

  # Identify valid Sentinel-2 bands for scaling
  sentinel_bands <- grep("^B[0-9]+_(summer|winter)$", names(df), value = TRUE)

  # Apply scaling (offset correction)
  df <- df %>%
    mutate(across(all_of(sentinel_bands), ~ (. - 1000) / 10000))

  # Compute vegetation indices only once
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
      NDMI_winter = (B8_winter - B12_winter) / (B8_winter + B12_winter),

      MCARI2_summer = 1.2 * (2.5 * (B8_summer - B4_summer) - 1.3 * (B8_summer - B3_summer)),
      MCARI2_winter = 1.2 * (2.5 * (B8_winter - B4_winter) - 1.3 * (B8_winter - B3_winter))
    )

  return(df)
}

# **(5) Apply Preprocessing to Both Datasets**
data_train <- preprocess_data(data_train)
data_test <- preprocess_data(data_test)

# **(6) Downsample the Training Set to Balance Classes**
set.seed(42)
data_balanced <- downSample(
  x = data_train[, !names(data_train) %in% "vitality"],
  y = data_train$vitality
)
colnames(data_balanced)[ncol(data_balanced)] <- "vitality"
data_balanced$vitality <- as.factor(data_balanced$vitality)

# **(7) Prepare Features for Model Training**
features <- c(
  "NDVI_summer", "SAVI_summer", "GNDVI_summer", "NDRE_summer", "NDMI_summer", "MCARI2_summer",
  "NDVI_winter", "SAVI_winter", "GNDVI_winter", "NDRE_winter", "NDMI_winter", "MCARI2_winter",
  "B2_summer", "B3_summer", "B4_summer", "B5_summer", "B6_summer", "B7_summer", "B8_summer", "B11_summer", "B12_summer",
  "B2_winter", "B3_winter", "B4_winter", "B5_winter", "B6_winter", "B7_winter", "B8_winter", "B11_winter", "B12_winter"
)

# **(8) Normalize Training & Test Sets**
process <- preProcess(data_balanced %>% select(all_of(features)), method = "range")
train_scaled <- predict(process, data_balanced %>% select(all_of(features)))
train_scaled$vitality <- data_balanced$vitality

test_scaled <- predict(process, data_test %>% select(all_of(features)))
test_scaled$vitality <- data_test$vitality

# **(9) Train Random Forest Model**
set.seed(400)
repeat_cv <- trainControl(method = "repeatedcv", number = 10, repeats = 5, classProbs = TRUE)

forest <- train(
  vitality ~ .,
  data = train_scaled,
  method = "rf",
  trControl = repeat_cv,
  metric = "Kappa",
  tuneLength = 5
)

# **(10) Evaluate Model on Test Set**
predictions <- predict(forest, test_scaled)
conf_matrix <- confusionMatrix(predictions, test_scaled$vitality)
print(conf_matrix)

# **(11) Save Model & Variable Importance**
saveRDS(forest, file = file.path(output_folder, "random_forest_model.rds"))

var_imp <- varImp(forest, scale = TRUE)$importance
var_imp <- data.frame(variables = row.names(var_imp), importance = var_imp$Overall)

write.csv(var_imp, file = file.path(output_folder, "variable_importance.csv"), row.names = FALSE)

# Define colors for different categories
var_imp$category <- case_when(
  grepl("_summer", var_imp$variables) & grepl("^NDVI|SAVI|GNDVI|NDRE|NDMI|MCARI2", var_imp$variables) ~ "Summer - Index",
  grepl("_winter", var_imp$variables) & grepl("^NDVI|SAVI|GNDVI|NDRE|NDMI|MCARI2", var_imp$variables) ~ "Winter - Index",
  grepl("_summer", var_imp$variables) ~ "Summer - Band",
  grepl("_winter", var_imp$variables) ~ "Winter - Band"
)

# Create the variable importance plot with four colors
p_varimp <- ggplot(var_imp %>% arrange(desc(importance)),
                   aes(x = reorder(variables, importance), y = importance, fill = category)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  xlab("Variables") +
  ylab("Importance") +
  scale_fill_manual(values = c("Summer - Index" = "goldenrod2",
                               "Winter - Index" = "deepskyblue",
                               "Summer - Band" = "darkorange",
                               "Winter - Band" = "darkblue")) +
  theme_bw(base_size = 16) +
  theme(legend.title = element_blank(),
        panel.grid.major = element_blank(),  # Remove major grid lines
        panel.grid.minor = element_blank())  # Remove minor grid lines

# Save the updated plot
ggsave(filename = file.path(output_folder, "variable_importance_colored.png"),
       plot = p_varimp, width = 8, height = 6, dpi = 600)

print("✅ Random Forest model trained, evaluated, and variable importance (colored) saved successfully.")


# **📌 Compute Confusion Matrix**
conf_matrix <- confusionMatrix(predictions, test_scaled$vitality)

# **Extract Confusion Matrix Table**
cm_table <- as.data.frame(conf_matrix$table)

# Rename columns for clarity
colnames(cm_table) <- c("Actual", "Predicted", "Count")

# **Reformat to Traditional Confusion Matrix**
cm_matrix <- xtabs(Count ~ Predicted + Actual, data = cm_table)

# Convert to a DataFrame for formatting
cm_df <- as.data.frame(as.table(cm_matrix))

# Convert to Wide Format
cm_wide <- cm_df %>%
  pivot_wider(names_from = Actual, values_from = Freq)

# Rename columns for better readability
colnames(cm_wide) <- c("Predicted Class", "Actual Vital", "Actual Non-Vital")

# **📌 Print Confusion Matrix**
cat("\n✅ Confusion Matrix (Predicted in Rows, Actual in Columns):\n")
print(cm_wide)

# Extract key metrics
precision <- conf_matrix$byClass["Pos Pred Value"]  # Precision
recall <- conf_matrix$byClass["Sensitivity"]        # Recall (Sensitivity)
f1_score <- conf_matrix$byClass["F1"]               # F1-score

# Extract overall model performance
accuracy <- conf_matrix$overall["Accuracy"]
kappa <- conf_matrix$overall["Kappa"]

# Print results
cat("Overall Accuracy:", round(accuracy, 4), "\n")
cat("Kappa:", round(kappa, 4), "\n")
cat("Precision:", round(precision, 4), "\n")
cat("Recall (Sensitivity):", round(recall, 4), "\n")
cat("F1 Score:", round(f1_score, 4), "\n")

