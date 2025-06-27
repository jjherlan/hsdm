# Envelope_approaches_b1 9.1, 
#opts.label = 'fig_half_page', 
#fig.cap = "Figure 9.1: Observed and potential distribution of the red fox using a rectilinear envelope model (sre function in the biomod2 package). 
#The potential distributions differ by the use of different percentiles to delineate the envelope. In both maps, black=presence, light gray= absence.

library(mda)
library(gam)
library(earth)
library(maxnet)
library(randomForest)
library(xgboost)
install.packages("xgboost")
library(biomod2) 
library(lattice)

# install.packages("biomod2", repos="http://R-Forge.R-project.org")
## Load the species and environmental datasets

#mammals_data <- read.csv("tabular/species/mammals_and_bioclim_table.csv", row.names=1)
mammals_data <- read.csv("data/tabular/species/mammals_and_bioclim_table.csv", row.names = 1)

head(mammals_data)

# <<<<<<< Updated upstream
# =======
# #Deprecated
# >>>>>>> Stashed changes
# # pred_BIOCLIM = sre(Response = mammals_data$VulpesVulpes, 
# #                    Explanatory = mammals_data[,c("bio3", "bio7", "bio11", "bio12")], 
# #                    NewData = mammals_data[,c("bio3", "bio7", "bio11", "bio12")], 
# #                    Quant = 0)
# <<<<<<< Updated upstream
# =======
# 
# pred_BIOCLIM = bm_SRE(resp.var = mammals_data$VulpesVulpes,
#                    expl.var = mammals_data[,c("bio3", "bio7", "bio11", "bio12")],
#                    new.env = mammals_data[,c("bio3", "bio7", "bio11", "bio12")],
#                    quant = 0)
# >>>>>>> Stashed changes

# Revised using bm_SRE()

pred_BIOCLIM = bm_SRE(resp.var = mammals_data$VulpesVulpes,
                   expl.var = mammals_data[,c("bio3", "bio7", "bio11", "bio12")],
                   new.env = mammals_data[,c("bio3", "bio7", "bio11", "bio12")],
                   quant = 0)

# pred_BIOCLIM_025 = sre(Response = mammals_data$VulpesVulpes, 
#                        Explanatory = mammals_data[,c("bio3", "bio7", "bio11", "bio12")], 
#                        NewData = mammals_data[,c("bio3",  "bio7", "bio11", "bio12")], 
#                        Quant = 0.025)

# Revised using bm_SRE()
# BIOCLIM_025

pred_BIOCLIM_025 = bm_SRE(resp.var = mammals_data$VulpesVulpes,
                       expl.var = mammals_data[,c("bio3", "bio7", "bio11", "bio12")],
                       new.env = mammals_data[,c("bio3",  "bio7", "bio11", "bio12")],
                       quant = 0.025)

# pred_BIOCLIM_05 = sre(Response = mammals_data$VulpesVulpes, 
#                       Explanatory = mammals_data[,c("bio3",  "bio7", "bio11", "bio12")], 
#                       NewData = mammals_data[,c("bio3",  "bio7", "bio11", "bio12")], 
#                       Quant = 0.05)

# Revised using bm_SRE()
# BIOCLIM_05

pred_BIOCLIM_05 = bm_SRE(resp.var = mammals_data$VulpesVulpes, 
                      expl.var = mammals_data[,c("bio3",  "bio7", "bio11", "bio12")], 
                      new.env = mammals_data[,c("bio3",  "bio7", "bio11", "bio12")], 
                      quant = 0.05)

par(mfrow = c (2, 2)
    )

# level.plot(mammals_data$VulpesVulpes, 
#            XY = mammals_data[,c("X_WGS84", "Y_WGS84")], 
#            color.gradient = "grey", 
#            cex=0.3,show.scale = F, 
#            title = "Original data") 

level.plot(mammals_data$VulpesVulpes,
           XY = mammals_data[,c("X_WGS84", "Y_WGS84")],
           color.gradient = "grey",
           cex = 0.3, show.scale = F,
           title =  "Original data")

# Error in level.plot(mammals_data$VulpesVulpes, XY = mammals_data[, c("X_WGS84",  : 
# could not find function "level.plot"

# level.plot(pred_BIOCLIM, 
#            XY = mammals_data[,c("X_WGS84", "Y_WGS84")], 
#            color.gradient = "grey", 
#            cex = 0.3, show.scale = F, 
#            title = "BIOCLIM 100%")

level.plot(pred_BIOCLIM,
           XY = mammals_data[,c("X_WGS84", "Y_WGS84")],
           color.gradient = "grey",
           cex = 0.3, show.scale = F,
           title = "BIOCLIM 100%")

# Error in level.plot(pred_BIOCLIM, XY = mammals_data[, c("X_WGS84", "Y_WGS84")],  : 
#                      could not find function "level.plot"

# level.plot(pred_BIOCLIM_025, 
#            XY=mammals_data[,c("X_WGS84", "Y_WGS84")], 
#            color.gradient = "grey", 
#            cex=0.3,show.scale=F, 
#            title="BIOCLIM 97.5%")

level.plot(pred_BIOCLIM_025, 
           XY=mammals_data[,c("X_WGS84", "Y_WGS84")], 
           color.gradient = "grey", 
           cex=0.3,show.scale=F, 
           title="BIOCLIM 97.5%")

# Error in level.plot(pred_BIOCLIM_025, XY = mammals_data[, c("X_WGS84",  : 
# could not find function "level.plot"

# level.plot(pred_BIOCLIM_05, 
#            XY=mammals_data[,c("X_WGS84", "Y_WGS84")], 
#            color.gradient = "grey", 
#            cex=0.3,show.scale=F, 
#            title="BIOCLIM 95%")

level.plot(pred_BIOCLIM_05, 
           XY=mammals_data[,c("X_WGS84", "Y_WGS84")], 
           color.gradient = "grey", 
           cex=0.3,show.scale=F, 
           title="BIOCLIM 95%")

# Error in level.plot(pred_BIOCLIM_05, XY = mammals_data[, c("X_WGS84",  : 
#   could not find function "level.plot"

par(mfrow = c(1, 1)
    )
