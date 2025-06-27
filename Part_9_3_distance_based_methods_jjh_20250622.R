### 9.3	Distance-based methods

# r code_9.3_Distance-based_methods_b1
library(adehabitatHS)
library(pROC)

# r code_9.3_Distance-based_methods_b2
pc <- dudi.pca(mammals_data[,c("bio3", "bio7", "bio11", "bio12")], scannf=FALSE, nf = 2)

# r code_9.3_Distance-based_methods_b3}
en <- enfa(pc, mammals_data$VulpesVulpes, scan=FALSE)

# r Distance-based_methods 9.2, opts.label = 'fig_half_page', fig.cap = 'Figure 9. 2: Ecological niche description of the red fox (function enfa() in the package adehabitatHS)'}

par(mfrow=c(2,2))

barplot(en$s) # the specialization diagram

scatterniche(en$li,mammals_data$VulpesVulpes, pts=T)  # plot the niche

s.arrow(cor(pc$tab,en$li)) # meaning of the axes

#r Distance-based_methods 9.3, opts.label = 'fig_half_page', fig.cap = 'Figure 9. 3: Observed and potential distribution of the red fox modeled using ENFA. The potential distribution is either expressed along a scale of habitat suitability values (light= low suitability to dark = high suitability), or in a binary form picturing presence-absence (black=presence, light gray= absence). '}

par(mfrow=c(2,2))

level.plot(mammals_data$VulpesVulpes, XY=mammals_data[,c("X_WGS84", "Y_WGS84")], 
           color.gradient = "grey", cex=0.3,show.scale=F, title="Original data")

level.plot(en$li[,1], XY=mammals_data[,c("X_WGS84", "Y_WGS84")], 
           color.gradient = "grey", cex=0.3,show.scale=F, title="ENFA")

roc_enfa <- roc(mammals_data$VulpesVulpes, en$li[,1])

threshold_enfa <- coords(roc_enfa, "best", ret=c("threshold"))

Pred01 = as.numeric(en$li[,1] > threshold_enfa)

level.plot(Pred01, XY=mammals_data[,c("X_WGS84", "Y_WGS84")], 
           color.gradient = "grey", cex=0.3,show.scale=F, title="ENFA binary")












