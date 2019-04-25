# What are the clusters?
clust1$center  # not super helpful
clust1$center[1,]*sigma + mu
clust1$center[2,]*sigma + mu

# Which wines are in which clusters?
which(clust1$cluster == 1)
which(clust1$cluster == 2)

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
       