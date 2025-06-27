library(biomod2)
library(MASS)
library(ggplot2)

library(mda)
library(gam)
library(earth)
library(maxnet)
library(randomForest)
library(xgboost)
#install.packages("xgboost")
library(lattice)

mammals_data <- read.csv("data/tabular/species/mammals_and_bioclim_table.csv", row.names = 1)

## Chapter 10	Regression-based approaches

### 10.2	Generalized Linear Models (GLM)

# r code_10.2_Generalized_Linear_Models_GLM_b1, opts.label = 'fig_half_page', 
# fig.cap = 'Figure 10.1: Observed (black=presence, light gray= absence) and potential distribution of species Sp290 modeled by different GLM differing by the complexity of the parameters (linear, quadratic and 2nd order polynomials). 
# The gray scale of predictions (up-right and lower panels) shows habitat suitability values between 0 (light, unsuitable) and 1 (dark, highly suitable)'

glm1 = glm(VulpesVulpes ~ 1+bio3+bio7+bio11+bio12, data=mammals_data, family="binomial")

glm2 = glm(VulpesVulpes ~ 1+poly(bio3,2)+poly(bio7,2)+poly(bio11,2)+poly(bio12,2), data=mammals_data, family="binomial")

# r GLM1 10.1, fig.height=8,fig.width=8

#library(biomod2)

par(mfrow=c(2,2))

level.plot(mammals_data$VulpesVulpes, XY=mammals_data[,c("X_WGS84", "Y_WGS84")], color.gradient = "grey", cex=0.3,show.scale=F, title="Original data")

level.plot(fitted(glm1), XY=mammals_data[,c("X_WGS84", "Y_WGS84")], color.gradient = "grey", cex=0.3,show.scale=F, title="GLM with linear terms")

level.plot(fitted(glm2), XY=mammals_data[,c("X_WGS84", "Y_WGS84")], color.gradient = "grey", cex=0.3,show.scale=F, title="GLM with quadratic terms")

# r GLM_b2 10.2, message=FALSE,warning=FALSE, opts.label = 'fig_quarter_page', fig.cap = 'Figure 10. 2: Response curves of model glm1 (linear terms) and glm2 (only quadratic terms).'

#library(ggplot2)

## create the response plot 

rp <- response.plot2(models = c('glm1','glm2'),
                     Data = mammals_data[,c("bio3", "bio7", "bio11", "bio12")],
                     show.variables = c("bio3",  "bio7", "bio11", "bio12"),
                     fixed.var.metric = 'mean', plot = FALSE, use.formal.names = TRUE)

## define a custom ggplot2 theme

rp.gg.theme <- theme(legend.title = element_blank(),
                     axis.text.x = element_text(angle = 90, vjust = .5),
                     panel.background = element_rect(fill = NA, colour = "gray70"),
                     strip.background = element_rect(fill = NA, colour = "gray70"),
                     panel.grid.major = element_line(colour = "grey90"),
                     legend.key = element_rect(fill = NA, colour = "gray70"))

## display the reponse plot

gg.rp <- ggplot(rp, aes(x = expl.val, y = pred.val, lty = pred.name)) +
  geom_line() + ylab("prob of occ") + xlab("") + 
  rp.gg.theme + 
  facet_grid(~ expl.name, scales = 'free_x')

print(gg.rp)

# r code_10.2_Generalized_Linear_Models_GLM_b3

#library(MASS)

glmStart <- glm(VulpesVulpes~1, data=mammals_data, family=binomial)

glm.formula <- formula("VulpesVulpes ~ 1 + 1 + poly(bio3,2) + poly(bio7,2) + poly(bio11,2) + poly(bio12,2) + bio3:bio7 + bio3:bio11 + bio3:bio12 + bio7:bio11 + bio7:bio12 + bio11:bio12")

glm.formula

```{r GLM_b4 10.3, opts.label = 'fig_quarter_page', fig.cap = 'Figure 10. 3: 2D response curves for the different fitted models'}
glmModAIC <- stepAIC( glmStart, 
                      glm.formula,
                      data = mammals_data,
                      direction = "both", trace = FALSE, 
                      k = 2, 
                      control=glm.control(maxit=100))

glmModBIC <- stepAIC( glmStart, 
                      glm.formula, 
                      direction="both", trace=FALSE,
                      k=log(nrow(mammals_data)),
                      control=glm.control(maxit=100))


rp <- response.plot2(models = c('glm1','glm2','glmModAIC','glmModBIC'),
                     Data = mammals_data[,c("bio3", "bio7", "bio11", "bio12")],
                     show.variables = c("bio3",  "bio7", "bio11", "bio12"),
                     fixed.var.metric = 'mean', plot = FALSE, use.formal.names = TRUE)

gg.rp <- ggplot(rp, aes(x = expl.val, y = pred.val, lty = pred.name)) +
  geom_line() + ylab("prob of occ") + xlab("") + 
  rp.gg.theme + 
  facet_grid(~ expl.name, scales = 'free_x')
print(gg.rp)
```



```{r GLM_b5 10.4, opts.label = 'fig_quarter_page', fig.cap = 'Figure 10. 4: Bivariate response curves from the model glmModAIC for four predictor variables.'}
rp.2D <- response.plot2( models = c('glmModAIC'),
                         Data = mammals_data[,c("bio3", "bio7", "bio11", "bio12")],
                         show.variables = c("bio3",  "bio7", "bio11", "bio12"),
                         fixed.var.metric = 'median', 
                         do.bivariate=T,
                         plot = FALSE, use.formal.names = TRUE )

gg.rp.2D <- ggplot(rp.2D, aes(x = expl1.val, y = expl2.val, fill = pred.val)) +
  geom_raster() + rp.gg.theme  + ylab("") + xlab("") + theme(legend.title = element_text()) +
  scale_fill_gradient(name = "prob of occ.", low = "#f0f0f0", high = "#000000") + 
  facet_grid(expl2.name ~ expl1.name, scales = 'free')
print(gg.rp.2D)
```


```{r code_10.2_Generalized_Linear_Models_GLM_b6}
anova(glmModAIC)
```

```{r GLM_b7 10.5, opts.label = 'fig_half_page', fig.cap = 'Figure 10. 5: Observed (black=presence, light gray= absence) and potential distribution of red fox extracted from glmModAIC and glmModBIC models. The gray scale of predictions shows habitat suitability values between 0 (light, unsuitable) and 1 (dark, highly suitable).'}
par(mfrow=c(2,2))
level.plot(mammals_data$VulpesVulpes, XY=mammals_data[,c("X_WGS84", "Y_WGS84")], color.gradient = "grey", cex=0.3,level.range=c(0,1), show.scale=F, title="Original data")
level.plot(fitted(glmModAIC), XY=mammals_data[,c("X_WGS84", "Y_WGS84")], color.gradient = "grey", cex=0.3, level.range=c(0,1), show.scale=F, title="Stepwise GLM with AIC")
level.plot(fitted(glmModBIC), XY=mammals_data[,c("X_WGS84", "Y_WGS84")], color.gradient = "grey", cex=0.3,level.range=c(0,1), show.scale=F, title="Stepwise GLM with BIC")
