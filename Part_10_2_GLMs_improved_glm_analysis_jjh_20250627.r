# Modern GLM Analysis for Species Distribution Modeling
# Updated version addressing deprecated functions and adding improvements

# Load required libraries
library(biomod2)
library(MASS)
library(ggplot2)
library(gridExtra)  # For arranging multiple plots
library(dplyr)      # For data manipulation
library(viridis)    # For better color palettes

# Optional libraries (uncomment if needed for other models)
# library(mda)
# library(gam)
# library(earth)
# library(maxnet)
# library(randomForest)
# library(xgboost)

# Load the dataset
mammals_data <- read.csv("mammals_and_bioclim_table.csv", row.names = 1)

# Data exploration and summary
cat("=== Dataset Summary ===\n")
cat("Total observations:", nrow(mammals_data), "\n")
cat("Red fox presence records:", sum(mammals_data$VulpesVulpes == 1), "\n")
cat("Red fox absence records:", sum(mammals_data$VulpesVulpes == 0), "\n")
cat("Prevalence:", round(mean(mammals_data$VulpesVulpes), 3), "\n\n")

# Check for missing values
missing_summary <- colSums(is.na(mammals_data))
if(any(missing_summary > 0)) {
  cat("Missing values found:\n")
  print(missing_summary[missing_summary > 0])
} else {
  cat("No missing values detected.\n")
}

# =============================================================================
# CHAPTER 10.2: Generalized Linear Models (GLM)
# =============================================================================

# Model 1: GLM with linear terms
glm1 <- glm(VulpesVulpes ~ bio3 + bio7 + bio11 + bio12, 
            data = mammals_data, 
            family = "binomial")

# Model 2: GLM with quadratic terms
glm2 <- glm(VulpesVulpes ~ poly(bio3,2) + poly(bio7,2) + poly(bio11,2) + poly(bio12,2), 
            data = mammals_data, 
            family = "binomial")

# Model 3: GLM with interactions (improved version of your formula)
glm3 <- glm(VulpesVulpes ~ poly(bio3,2) + poly(bio7,2) + poly(bio11,2) + poly(bio12,2) + 
            bio3:bio7 + bio3:bio11 + bio3:bio12 + bio7:bio11 + bio7:bio12 + bio11:bio12,
            data = mammals_data, 
            family = "binomial")

# Model summaries
cat("=== Model Summaries ===\n")
cat("GLM1 (linear) AIC:", AIC(glm1), "\n")
cat("GLM2 (quadratic) AIC:", AIC(glm2), "\n")
cat("GLM3 (quadratic + interactions) AIC:", AIC(glm3), "\n\n")

# =============================================================================
# MODERN PLOTTING FUNCTIONS (replacing deprecated level.plot)
# =============================================================================

# Function to create distribution plots (replaces level.plot)
modern_distribution_plot <- function(values, coords, title, 
                                   plot_type = "continuous", 
                                   point_size = 0.3) {
  
  # Prepare data
  plot_data <- data.frame(
    X = coords[,"X_WGS84"],
    Y = coords[,"Y_WGS84"],
    Values = values
  )
  
  if(plot_type == "binary") {
    # For binary data (presence/absence)
    p <- ggplot(plot_data, aes(x = X, y = Y)) +
      geom_point(aes(color = factor(Values)), size = point_size, alpha = 0.7) +
      scale_color_manual(values = c("0" = "lightgray", "1" = "black"),
                        labels = c("Absence", "Presence"),
                        name = "") +
      theme_minimal()
  } else {
    # For continuous data (probabilities)
    p <- ggplot(plot_data, aes(x = X, y = Y)) +
      geom_point(aes(color = Values), size = point_size, alpha = 0.7) +
      scale_color_gradient(low = "lightgray", high = "black",
                          name = "Probability") +
      theme_minimal()
  }
  
  p <- p +
    labs(title = title,
         x = "Longitude (WGS84)",
         y = "Latitude (WGS84)") +
    theme(
      plot.title = element_text(hjust = 0.5, size = 11),
      axis.text = element_text(size = 8),
      legend.position = "right",
      legend.title = element_text(size = 9),
      legend.text = element_text(size = 8)
    ) +
    coord_fixed(ratio = 1)
  
  return(p)
}

# =============================================================================
# CREATE DISTRIBUTION PLOTS
# =============================================================================

# Generate plots for all models
plot1 <- modern_distribution_plot(
  values = mammals_data$VulpesVulpes,
  coords = mammals_data[,c("X_WGS84", "Y_WGS84")],
  title = "Original Data",
  plot_type = "binary"
)

plot2 <- modern_distribution_plot(
  values = fitted(glm1),
  coords = mammals_data[,c("X_WGS84", "Y_WGS84")],
  title = "GLM with Linear Terms",
  plot_type = "continuous"
)

plot3 <- modern_distribution_plot(
  values = fitted(glm2),
  coords = mammals_data[,c("X_WGS84", "Y_WGS84")],
  title = "GLM with Quadratic Terms",
  plot_type = "continuous"
)

plot4 <- modern_distribution_plot(
  values = fitted(glm3),
  coords = mammals_data[,c("X_WGS84", "Y_WGS84")],
  title = "GLM with Interactions",
  plot_type = "continuous"
)

# Display plots in 2x2 arrangement
grid.arrange(plot1, plot2, plot3, plot4, ncol = 2, nrow = 2)

# =============================================================================
# RESPONSE CURVES (Modern approach)
# =============================================================================

# Create response curves manually since response.plot2 may also be deprecated
create_response_curves <- function(model, data, var_names, model_name) {
  
  response_data <- data.frame()
  
  for(var in var_names) {
    # Create sequence for the variable
    var_seq <- seq(min(data[[var]], na.rm = TRUE), 
                   max(data[[var]], na.rm = TRUE), 
                   length.out = 100)
    
    # Create prediction data with other variables at mean
    pred_data <- data
    for(other_var in var_names) {
      if(other_var != var) {
        pred_data[[other_var]] <- mean(data[[other_var]], na.rm = TRUE)
      }
    }
    
    # Replace the focal variable with the sequence
    pred_data_seq <- pred_data[rep(1, length(var_seq)), ]
    pred_data_seq[[var]] <- var_seq
    
    # Make predictions
    predictions <- predict(model, pred_data_seq, type = "response")
    
    # Store results
    temp_data <- data.frame(
      Variable = var,
      Value = var_seq,
      Prediction = predictions,
      Model = model_name
    )
    
    response_data <- rbind(response_data, temp_data)
  }
  
  return(response_data)
}

# Generate response curves for both models
var_names <- c("bio3", "bio7", "bio11", "bio12")

response_glm1 <- create_response_curves(glm1, mammals_data, var_names, "GLM Linear")
response_glm2 <- create_response_curves(glm2, mammals_data, var_names, "GLM Quadratic")
response_glm3 <- create_response_curves(glm3, mammals_data, var_names, "GLM Interactions")

# Combine all response data
all_responses <- rbind(response_glm1, response_glm2, response_glm3)

# Create response curve plot
response_plot <- ggplot(all_responses, aes(x = Value, y = Prediction, color = Model)) +
  geom_line(size = 1) +
  facet_wrap(~ Variable, scales = "free_x", ncol = 2) +
  labs(title = "Response Curves for Different GLM Models",
       x = "Environmental Variable Value",
       y = "Probability of Occurrence") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14),
    strip.text = element_text(size = 10),
    legend.position = "bottom"
  ) +
  scale_color_viridis_d(name = "Model Type")

print(response_plot)

# =============================================================================
# MODEL EVALUATION AND COMPARISON
# =============================================================================

# Calculate AUC for model comparison
library(pROC)

# Function to calculate model metrics
evaluate_model <- function(model, data) {
  predictions <- predict(model, type = "response")
  
  # AUC
  roc_obj <- roc(data$VulpesVulpes, predictions, quiet = TRUE)
  auc_value <- as.numeric(auc(roc_obj))
  
  # Deviance explained (pseudo R-squared)
  null_deviance <- model$null.deviance
  residual_deviance <- model$deviance
  dev_explained <- (null_deviance - residual_deviance) / null_deviance
  
  return(list(
    AIC = AIC(model),
    AUC = auc_value,
    DevExplained = dev_explained,
    df = model$df.residual
  ))
}

# Evaluate all models
eval_glm1 <- evaluate_model(glm1, mammals_data)
eval_glm2 <- evaluate_model(glm2, mammals_data)
eval_glm3 <- evaluate_model(glm3, mammals_data)

# Create comparison table
comparison_table <- data.frame(
  Model = c("GLM Linear", "GLM Quadratic", "GLM Interactions"),
  AIC = c(eval_glm1$AIC, eval_glm2$AIC, eval_glm3$AIC),
  AUC = c(eval_glm1$AUC, eval_glm2$AUC, eval_glm3$AUC),
  DevExplained = c(eval_glm1$DevExplained, eval_glm2$DevExplained, eval_glm3$DevExplained),
  df = c(eval_glm1$df, eval_glm2$df, eval_glm3$df)
)

# Display results
cat("\n=== Model Comparison ===\n")
print(round(comparison_table, 4))

# Determine best model based on AIC
best_model_idx <- which.min(comparison_table$AIC)
cat("\nBest model based on AIC:", comparison_table$Model[best_model_idx], "\n")

# =============================================================================
# ALTERNATIVE: Using Base R plotting (similar to original level.plot)
# =============================================================================

# Function that mimics level.plot behavior
base_distribution_plot <- function(values, coords, title, is_binary = FALSE) {
  
  if(is_binary) {
    colors <- c("lightgray", "black")[values + 1]
  } else {
    # Create grayscale based on values
    gray_levels <- gray(1 - values)  # Invert so high values are dark
    colors <- gray_levels
  }
  
  plot(coords[,"X_WGS84"], coords[,"Y_WGS84"],
       col = colors,
       pch = 19, cex = 0.3,
       main = title,
       xlab = "Longitude (WGS84)",
       ylab = "Latitude (WGS84)",
       asp = 1)
  
  if(is_binary) {
    legend("topright", 
           legend = c("Absence", "Presence"),
           col = c("lightgray", "black"),
           pch = 19,
           cex = 0.7)
  }
}

# Create base R plots (uncomment if you prefer base R plotting)
# par(mfrow = c(2, 2))
# 
# base_distribution_plot(mammals_data$VulpesVulpes, 
#                       mammals_data[,c("X_WGS84", "Y_WGS84")],
#                       "Original data", is_binary = TRUE)
# 
# base_distribution_plot(fitted(glm1), 
#                       mammals_data[,c("X_WGS84", "Y_WGS84")],
#                       "GLM with linear terms")
# 
# base_distribution_plot(fitted(glm2), 
#                       mammals_data[,c("X_WGS84", "Y_WGS84")],
#                       "GLM with quadratic terms")
# 
# base_distribution_plot(fitted(glm3), 
#                       mammals_data[,c("X_WGS84", "Y_WGS84")],
#                       "GLM with interactions")
# 
# par(mfrow = c(1, 1))

cat("\n=== Analysis Complete ===\n")
cat("All plots generated successfully using modern functions!\n")
