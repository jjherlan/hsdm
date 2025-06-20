# Envelope_approaches_b1 9.1, 
#opts.label = 'fig_half_page', 
#fig.cap = "Figure 9.1: Observed and potential distribution of the red fox using a rectilinear envelope model (sre function in the biomod2 package). 
#The potential distributions differ by the use of different percentiles to delineate the envelope. In both maps, black=presence, light gray= absence.

library(biomod2) 

# install.packages("biomod2", repos="http://R-Forge.R-project.org")
## Load the species and environmental datasets

#mammals_data <- read.csv("tabular/species/mammals_and_bioclim_table.csv", row.names=1)
mammals_data <- read.csv("data/tabular/species/mammals_and_bioclim_table.csv", row.names = 1)

head(mammals_data)

pred_BIOCLIM = sre(Response = mammals_data$VulpesVulpes, 
                   Explanatory = mammals_data[,c("bio3", "bio7", "bio11", "bio12")], 
                   NewData = mammals_data[,c("bio3", "bio7", "bio11", "bio12")], 
                   Quant = 0)

pred_BIOCLIM_025 = sre(Response = mammals_data$VulpesVulpes, 
                       Explanatory = mammals_data[,c("bio3", "bio7", "bio11", "bio12")], 
                       NewData = mammals_data[,c("bio3",  "bio7", "bio11", "bio12")], 
                       Quant = 0.025)

pred_BIOCLIM_05 = sre(Response = mammals_data$VulpesVulpes, 
                      Explanatory = mammals_data[,c("bio3",  "bio7", "bio11", "bio12")], 
                      NewData = mammals_data[,c("bio3",  "bio7", "bio11", "bio12")], 
                      Quant = 0.05)

par(mfrow=c(2,2))
level.plot(mammals_data$VulpesVulpes, 
           XY=mammals_data[,c("X_WGS84", "Y_WGS84")], 
           color.gradient = "grey", 
           cex=0.3,show.scale=F, 
           title="Original data")

level.plot(pred_BIOCLIM, 
           XY=mammals_data[,c("X_WGS84", "Y_WGS84")], 
           color.gradient = "grey", 
           cex=0.3,show.scale=F, 
           title="BIOCLIM 100%")

level.plot(pred_BIOCLIM_025, 
           XY=mammals_data[,c("X_WGS84", "Y_WGS84")], 
           color.gradient = "grey", 
           cex=0.3,show.scale=F, 
           title="BIOCLIM 97.5%")

level.plot(pred_BIOCLIM_05, 
           XY=mammals_data[,c("X_WGS84", "Y_WGS84")], 
           color.gradient = "grey", 
           cex=0.3,show.scale=F, 
           title="BIOCLIM 95%")
par(mfrow=c(1,1))