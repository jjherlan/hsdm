# Modern Biomod2 Envelope Approaches with Updated Plotting Functions
# Updated version using current biomod2 plotting functions
# Replaces deprecated level.plot() with modern alternatives

# Load required libraries
library(biomod2)
library(terra)        # Modern raster package (replaces raster)
library(ggplot2)      # For enhanced plotting
library(dplyr)        # For data manipulation
library(viridis)      # For color scales

# Load the species and environmental datasets
mammals_data <- read.csv("mammals_and_bioclim_table.csv", row.names = 1)

head(mammals_data)
print(paste("Dataset contains", nrow(mammals_data), "observations"))
print(paste("Red fox presence records:", sum(mammals_data$VulpesVulpes == 1)))
print(paste("Red fox absence records:", sum(mammals_data$VulpesVulpes == 0)))

# =============================================================================
# STEP 1: Create SRE (Surface Range Envelope) models with different quantiles
# =============================================================================

# BIOCLIM model with 100% envelope (quantile = 0)
pred_BIOCLIM <- bm_SRE(resp.var = mammals_data$VulpesVulpes,
                       expl.var = mammals_data[,c("bio3", "bio7", "bio11", "bio12")],
                       new.env = mammals_data[,c("bio3", "bio7", "bio11", "bio12")],
                       quant = 0)

# BIOCLIM model with 97.5% envelope (quantile = 0.025)
pred_BIOCLIM_025 <- bm_SRE(resp.var = mammals_data$VulpesVulpes,
                           expl.var = mammals_data[,c("bio3", "bio7", "bio11", "bio12")],
                           new.env = mammals_data[,c("bio3", "bio7", "bio11", "bio12")],
                           quant = 0.025)

# BIOCLIM model with 95% envelope (quantile = 0.05)
pred_BIOCLIM_05 <- bm_SRE(resp.var = mammals_data$VulpesVulpes,
                          expl.var = mammals_data[,c("bio3", "bio7", "bio11", "bio12")],
                          new.env = mammals_data[,c("bio3", "bio7", "bio11", "bio12")],
                          quant = 0.05)

print("SRE models completed successfully!")

# =============================================================================
# STEP 2: Modern Plotting Approach - Custom ggplot2 visualization
# =============================================================================

# Create a comprehensive plotting function using ggplot2
create_distribution_plots <- function(data, coords, predictions, titles) {
  
  # Create a list to store plots
  plot_list <- list()
  
  for (i in 1:length(predictions)) {
    
    # Prepare data for plotting
    plot_data <- data.frame(
      X = coords[,"X_WGS84"],
      Y = coords[,"Y_WGS84"],
      Original = data$VulpesVulpes,
      Predicted = predictions[[i]]
    )
    
    # Create the plot
    p <- ggplot(plot_data, aes(x = X, y = Y)) +
      geom_point(aes(color = factor(Predicted)), size = 0.5, alpha = 0.7) +
      scale_color_manual(values = c("0" = "lightgray", "1" = "black"),
                        labels = c("Absence", "Presence"),
                        name = "Prediction") +
      labs(title = titles[i],
           x = "Longitude (WGS84)",
           y = "Latitude (WGS84)") +
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0.5, size = 12),
        axis.text = element_text(size = 8),
        legend.position = "bottom",
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 8)
      ) +
      coord_fixed()
    
    plot_list[[i]] <- p
  }
  
  return(plot_list)
}

# =============================================================================
# STEP 3: Create and display the plots
# =============================================================================

# Prepare data for plotting
predictions_list <- list(
  mammals_data$VulpesVulpes,
  pred_BIOCLIM,
  pred_BIOCLIM_025,
  pred_BIOCLIM_05
)

plot_titles <- c(
  "Original Data",
  "BIOCLIM 100%",
  "BIOCLIM 97.5%", 
  "BIOCLIM 95%"
)

# Generate the plots
distribution_plots <- create_distribution_plots(
  data = mammals_data,
  coords = mammals_data[,c("X_WGS84", "Y_WGS84")],
  predictions = predictions_list,
  titles = plot_titles
)

# Display plots in a 2x2 layout
library(gridExtra)
grid.arrange(grobs = distribution_plots, ncol = 2, nrow = 2)

# =============================================================================
# STEP 4: Alternative plotting using base R (similar to original level.plot)
# =============================================================================

# Function to mimic level.plot functionality with base R
modern_level_plot <- function(response, coords, title, color_gradient = "grey") {
  
  # Create color palette
  if (color_gradient == "grey") {
    colors <- c("lightgray", "black")
  } else {
    colors <- c("lightblue", "darkblue")
  }
  
  # Create the plot
  plot(coords[,"X_WGS84"], coords[,"Y_WGS84"],
       col = colors[response + 1],
       pch = 19, cex = 0.3,
       main = title,
       xlab = "Longitude (WGS84)",
       ylab = "Latitude (WGS84)",
       asp = 1)
  
  # Add legend
  legend("topright", 
         legend = c("Absence", "Presence"),
         col = colors,
         pch = 19,
         cex = 0.7)
}

# Create 2x2 plot layout using base R
par(mfrow = c(2, 2))

# Plot original data
modern_level_plot(mammals_data$VulpesVulpes, 
                  mammals_data[,c("X_WGS84", "Y_WGS84")],
                  "Original data")

# Plot BIOCLIM 100%
modern_level_plot(pred_BIOCLIM, 
                  mammals_data[,c("X_WGS84", "Y_WGS84")],
                  "BIOCLIM 100%")

# Plot BIOCLIM 97.5%
modern_level_plot(pred_BIOCLIM_025, 
                  mammals_data[,c("X_WGS84", "Y_WGS84")],
                  "BIOCLIM 97.5%")

# Plot BIOCLIM 95%
modern_level_plot(pred_BIOCLIM_05, 
                  mammals_data[,c("X_WGS84", "Y_WGS84")],
                  "BIOCLIM 95%")

# Reset plot layout
par(mfrow = c(1, 1))

# =============================================================================
# STEP 5: Advanced biomod2 visualization (if using full biomod2 workflow)
# =============================================================================

# Note: The following code shows how to use modern biomod2 plotting functions
# if you want to build a complete biomod2 model instead of just SRE

# Uncomment and modify this section if you want to use full biomod2 workflow:

# # Format data for biomod2
# myRespName <- 'VulpesVulpes'
# myResp <- mammals_data$VulpesVulpes
# myRespXY <- mammals_data[, c('X_WGS84', 'Y_WGS84')]
# 
# # Create environmental raster (optional - for spatial projections)
# # myExpl <- terra::rast(environmental_rasters)  # if you have raster files
# 
# # Format the data
# myBiomodData <- BIOMOD_FormatingData(resp.var = myResp,
#                                      expl.var = mammals_data[,c("bio3", "bio7", "bio11", "bio12")],
#                                      resp.xy = myRespXY,
#                                      resp.name = myRespName)
# 
# # Build models
# myBiomodModelOut <- BIOMOD_Modeling(bm.format = myBiomodData,
#                                     modeling.id = 'RedFoxModels',
#                                     models = c('SRE', 'GLM', 'RF'),
#                                     CV.strategy = 'random',
#                                     CV.nb.rep = 2,
#                                     CV.perc = 0.8,
#                                     metric.eval = c('TSS', 'ROC'))
# 
# # Use modern biomod2 plotting functions
# bm_PlotEvalMean(bm.out = myBiomodModelOut)
# bm_PlotEvalBoxplot(bm.out = myBiomodModelOut)
# 
# # Plot response curves (replaces old response.plot)
# bm_PlotResponseCurves(bm.out = myBiomodModelOut,
#                       models.chosen = 'all',
#                       fixed.var = 'median')

# =============================================================================
# STEP 6: Summary and model comparison
# =============================================================================

# Calculate and display model statistics
cat("\n=== Model Performance Summary ===\n")
cat("Original presence points:", sum(mammals_data$VulpesVulpes), "\n")
cat("BIOCLIM 100% predicted presence:", sum(pred_BIOCLIM), "\n")
cat("BIOCLIM 97.5% predicted presence:", sum(pred_BIOCLIM_025), "\n")
cat("BIOCLIM 95% predicted presence:", sum(pred_BIOCLIM_05), "\n")

# Calculate overlap with original data
overlap_100 <- sum(mammals_data$VulpesVulpes == 1 & pred_BIOCLIM == 1)
overlap_975 <- sum(mammals_data$VulpesVulpes == 1 & pred_BIOCLIM_025 == 1)
overlap_95 <- sum(mammals_data$VulpesVulpes == 1 & pred_BIOCLIM_05 == 1)

cat("\n=== Overlap with Original Presence Data ===\n")
cat("BIOCLIM 100% overlap:", overlap_100, "out of", sum(mammals_data$VulpesVulpes), "\n")
cat("BIOCLIM 97.5% overlap:", overlap_975, "out of", sum(mammals_data$VulpesVulpes), "\n")
cat("BIOCLIM 95% overlap:", overlap_95, "out of", sum(mammals_data$VulpesVulpes), "\n")

cat("\n=== Analysis Complete ===\n")
cat("All visualizations created successfully using modern biomod2 and ggplot2 functions!\n")
