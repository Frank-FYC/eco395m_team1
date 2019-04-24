# PCA analysis####
ip <- installed.packages() 
pkgs.to.remove <- ip[!(ip[,"Priority"] %in% c("base", "recommended")), 1] 

library(ISLR)
library(tidyverse)
library(ggplot2)
library(psych)

wine <- read.csv("wine.csv", header = TRUE)

# Pick out the pca columns
Z = wine[,c(1:9)]
# Standardize (center/scale) the data
Z_std = scale(Z, center=TRUE, scale=TRUE)
plot(Z_std)

# fancy plot matrix with stuff, see http://www.sthda.com/english/wiki/scatter-plot-matrices-r-base-graphs
pairs.panels(Z_std[,1:4], 
             method = "pearson", # correlation method
             hist.col = "#00AFBB",
             density = TRUE,  # show density plots
             ellipses = TRUE # show correlation ellipses
)

pairs(wine[,1:4], pch = 19)
pairs(wine[,1:4], pch = 19, lower.panel = NULL)
my_cols <- c("red", "grey")  
pairs(wine[,1:4], pch = 19,  cex = 0.5,
      col = my_cols[wine$color],
      lower.panel=NULL)
# Correlation panel
panel.cor <- function(x, y){
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- round(cor(x, y), digits=2)
  txt <- paste0("R = ", r)
  cex.cor <- 0.8/strwidth(txt)
  text(0.5, 0.5, txt, cex = cex.cor * r)
}
# Customize upper panel
upper.panel<-function(x, y){
  points(x,y, pch = 19, col = my_cols[wine$color])
}
# Create the plots
pairs(wine[,1:4], 
      lower.panel = panel.cor,
      upper.panel = upper.panel)




x_wine = as.matrix(wine[,1:11])
y_wine_quality= wine[,12]
y_wine_color= wine[,13]

# Let's try dimensionality reduction via PCA
pc_wine = prcomp(x_wine, scale=TRUE)

# pc_wine$x has the summary variables
# Regress on the first K
K = 3
scores = pc_wine$x[,1:K]
pcr1 = lm(y_wine_quality ~ scores)

summary(pcr1)

# Show the model fit
par(mfrow=c(2,2))
plot(fitted(pcr1), y_wine_quality, main="Wine Quality as a measure of PC 1")
plot(seq(1,11,by=1), pc_wine$rotation[,1])
plot(seq(1,11,by=1), pc_wine$rotation[,2])
plot(seq(1,11,by=1), pc_wine$rotation[,3])

#Cluster Analysis####
ip <- installed.packages() 
pkgs.to.remove <- ip[!(ip[,"Priority"] %in% c("base", "recommended")), 1] 

library(ggplot2)
library(LICORS)  # for kmeans++
library(foreach)
library(mosaic)

# Center and scale the data
X = wine[,c(1:9)]
X = scale(X, center=TRUE, scale=TRUE)

# Extract the centers and scales from the rescaled data (which are named attributes)
mu = attr(X,"scaled:center")
sigma = attr(X,"scaled:scale")

# Run k-means with 2 clusters and 25 starts
clust1 = kmeans(X, 2, nstart=25)

# What are the clusters?
clust1$center  # not super helpful
clust1$center[1,]*sigma + mu
clust1$center[2,]*sigma + mu


# Which wines are in which clusters?
which(clust1$cluster == 1)
which(clust1$cluster == 2)

# Using kmeans++ initialization
clust2 = kmeanspp(X, k=2, nstart=25)

clust2$center[1,]*sigma + mu
clust2$center[2,]*sigma + mu

# Which cars are in which clusters?
which(clust2$cluster == 1)
which(clust2$cluster == 2)

# Compare versus within-cluster average distances from the first run
clust1$withinss
clust2$withinss
sum(clust1$withinss)
sum(clust2$withinss)
clust1$tot.withinss
clust2$tot.withinss
clust1$betweenss
clust2$betweenss

# A few plots with cluster membership shown
# qplot is in the ggplot2 library
qplot(fixed.acidity, volatile.acidity, data=wine, color=factor(clust2$cluster))
qplot(citric.acid, residual.sugar, data=wine, color=factor(clust2$cluster))
qplot(quality, color, data=wine, color=factor(clust2$cluster))