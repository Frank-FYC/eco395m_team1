library(foreach)
library(cluster)
library(corrplot)
library(plotly)
library(GGally)
library(HSAUR)


#36 different categories
SocialData <- read.csv("https://raw.githubusercontent.com/jgscott/ECO395M/master/data/social_marketing.csv", row.names=1)

#delete users with spam
SocialData<-SocialData[(SocialData$spam==0),]

#delete uncategorized label "chatter"
SocialData <- subset(SocialData, select = -c(chatter, uncategorized))

#add total tweets & calculate adult ratio & delete adult ratio more than 30%
SocialData <- cbind(tot_twt = rowSums(SocialData), SocialData)
SocialData <- cbind(adult_ratio = 1, SocialData)
SocialData$adult_ratio <- SocialData$adult/SocialData$tot_twt
SocialData<-SocialData[(SocialData$adult_ratio<0.3),]

#delete uncategorized label "unused attributes"
SocialData <- subset(SocialData, select = -c(adult_ratio, tot_twt, spam))

# Center/scale the data
SocialData_scaled <- scale(SocialData, center=TRUE, scale=TRUE) 

#K-grid to find the optimal K
k_grid = seq(2, 20, by=1)
SSE_grid = foreach(k = k_grid, .combine='c') %do% {
  cluster_k = kmeans(SocialData_scaled, k, nstart=50)
  cluster_k$tot.withinss
}
plot(k_grid, SSE_grid, xlab="K",ylab="SSE Grid", sub="SSE Grid vs K")

#CH-grid to find the optimal K
N=nrow(SocialData)
CH_grid = foreach(k = k_grid, .combine='c') %do% {
  cluster_k = kmeans(SocialData_scaled, k, nstart=20)
  W = cluster_k$tot.withinss
  B = cluster_k$betweenss
  CH = (B/W)*((N-k)/(k-1))
  CH
}
plot(k_grid, CH_grid, xlab="K",
     ylab="CH Grid",
     sub="CH Grid vs K")

#Gap statistics
Market_gap = clusGap(SocialData_scaled, FUN = kmeans, nstart = 20, K.max = 10, B = 10)
plot(Market_gap)

#correlation and visualization
corr <- cor(SocialData_scaled)
corrplot(corr, type="upper", tl.cex = 0.5, tl.col="black", order="hclust", col=c("black", "white"), bg="darkgreen")


# k-means analysis
clust1 = kmeans(SocialData_scaled, centers=6, nstart=25)
D1 = subset(SocialData,select = c("personal_fitness","health_nutrition","outdoors"))
ggpairs(D1, aes(col = factor(clust1$cluster), alpha=0.5), columns = 1:ncol(D1), title = "Clusters on personal fitness, health and outdoors",
        upper = list(continuous = "cor", combo = "box_no_facet", discrete = "facetbar", na = "na"), 
        lower = list(continuous = "points", combo = "facethist", discrete = "facetbar", na = "na"), 
        diag = list(continuous =  "densityDiag", discrete = "barDiag", na = "naDiag"), params = NULL,
        xlab = NULL, ylab = NULL, axisLabels = c("show"),
        labeller = "label_value",
        switch = NULL, showStrips = NULL, legend = NULL,
        cardinality_threshold = 15, progress = NULL)

D2 = subset(SocialData,select = c("fashion","cooking","beauty", "shopping", "photo_sharing"))
ggpairs(D2, aes(col = factor(clust1$cluster), alpha=0.5), columns = 1:ncol(D1), title = "Clusters on fashion, cooking, beauty, shopping and photosharing",
        upper = list(continuous = "cor", combo = "box_no_facet", discrete = "facetbar", na = "na"), 
        lower = list(continuous = "points", combo = "facethist", discrete = "facetbar", na = "na"), 
        diag = list(continuous =  "densityDiag", discrete = "barDiag", na = "naDiag"), params = NULL,
        xlab = NULL, ylab = NULL, axisLabels = c("show"),
        labeller = "label_value",
        switch = NULL, showStrips = NULL, legend = NULL,
        cardinality_threshold = 15, progress = NULL)


D3 = subset(SocialData,select = c("online_gaming","college_uni","sports_playing"))
ggpairs(D3, aes(col = factor(clust1$cluster), alpha=0.5), columns = 1:ncol(D1), title = "Clusters on gaming, university and sports",
        upper = list(continuous = "cor", combo = "box_no_facet", discrete = "facetbar", na = "na"), 
        lower = list(continuous = "points", combo = "facethist", discrete = "facetbar", na = "na"), 
        diag = list(continuous =  "densityDiag", discrete = "barDiag", na = "naDiag"), params = NULL,
        xlab = NULL, ylab = NULL, axisLabels = c("show"),
        labeller = "label_value",
        switch = NULL, showStrips = NULL, legend = NULL,
        cardinality_threshold = 15, progress = NULL)

D4 = subset(SocialData,select = c("sports_fandom","parenting","school","food", "family"))
ggpairs(D4, aes(col = factor(clust1$cluster), alpha=0.5), columns = 1:ncol(D1), title = "Clusters on sports, parenting, school, food, family",
        upper = list(continuous = "cor", combo = "box_no_facet", discrete = "facetbar", na = "na"), 
        lower = list(continuous = "points", combo = "facethist", discrete = "facetbar", na = "na"), 
        diag = list(continuous =  "densityDiag", discrete = "barDiag", na = "naDiag"), params = NULL,
        xlab = NULL, ylab = NULL, axisLabels = c("show"),
        labeller = "label_value",
        switch = NULL, showStrips = NULL, legend = NULL,
        cardinality_threshold = 15, progress = NULL)

D5 = subset(SocialData,select = c("politics","news","computers", "travel", "automotive"))
ggpairs(D5, aes(col = factor(clust1$cluster), alpha=0.5), columns = 1:ncol(D1), title = "Clusters on politics, news, computers, travel, automotive",
        upper = list(continuous = "cor", combo = "box_no_facet", discrete = "facetbar", na = "na"), 
        lower = list(continuous = "points", combo = "facethist", discrete = "facetbar", na = "na"), 
        diag = list(continuous =  "densityDiag", discrete = "barDiag", na = "naDiag"), params = NULL,
        xlab = NULL, ylab = NULL, axisLabels = c("show"),
        labeller = "label_value",
        switch = NULL, showStrips = NULL, legend = NULL,
        cardinality_threshold = 15, progress = NULL)

D6 = subset(SocialData,select = c("tv_film","art","music"))
ggpairs(D6, aes(col = factor(clust1$cluster), alpha=0.5), columns = 1:ncol(D1), title = "Clusters on tv, art, music",
        upper = list(continuous = "cor", combo = "box_no_facet", discrete = "facetbar", na = "na"), 
        lower = list(continuous = "points", combo = "facethist", discrete = "facetbar", na = "na"), 
        diag = list(continuous =  "densityDiag", discrete = "barDiag", na = "naDiag"), params = NULL,
        xlab = NULL, ylab = NULL, axisLabels = c("show"),
        labeller = "label_value",
        switch = NULL, showStrips = NULL, legend = NULL,
        cardinality_threshold = 15, progress = NULL)



#PCA Analysis

SocialMarket <- read.csv("https://raw.githubusercontent.com/jgscott/ECO395M/master/data/social_marketing.csv", row.names=1)

#delete users with spam
SocialMarket<-SocialMarket[(SocialMarket$spam==0),]

#delete uncategorized label "chatter"
SocialMarket <- subset(SocialMarket, select = -c(chatter, uncategorized))

#add total tweets & calculate adult ratio & delete adult ratio more than 30%
SocialMarket <- cbind(tot_twt = rowSums(SocialMarket), SocialMarket)
SocialMarket <- cbind(adult_ratio = 1, SocialMarket)
SocialMarket$adult_ratio <- SocialMarket$adult/SocialMarket$tot_twt
SocialMarket<-SocialMarket[(SocialMarket$adult_ratio<0.3),]

#delete uncategorized label "unused attributes"
SocialMarket <- subset(SocialMarket, select = -c(adult_ratio, tot_twt, spam))

#center and scale the data
SocialMarket = scale(SocialMarket, center=TRUE, scale=TRUE)

# correlation
corr=cor(SocialMarket)

# PCA
pca = prcomp(SocialMarket,scale=TRUE)
loadings = pca$rotation
scores = pca$x

# PVE
V = pca$sdev^2
PVE = V / sum(V)
round(PVE, 2)

Plot1 = qplot(c(1:33), PVE) + 
  geom_line() + 
  xlab("Principal Component") + 
  ylab("PVE") +
  ggtitle("Scree Plot") +
  ylim(0, 0.15)
Plot1

Plot2 = qplot(c(1:33), cumsum(PVE)) + 
  geom_line() + 
  xlab("Principal Component") + 
  ylab(NULL) +
  ggtitle("Cumulative Scree Plot") +
  ylim(0,1)
Plot2


# extract market segments
o1 = order(loadings[,1], decreasing=TRUE)
colnames(SocialMarket)[head(o1,5)]
o2 = order(loadings[,2], decreasing=TRUE)
colnames(SocialMarket)[head(o2,5)]
o3 = order(loadings[,3], decreasing=TRUE)
colnames(SocialMarket)[head(o3,5)]
o4 = order(loadings[,4], decreasing=TRUE)
colnames(SocialMarket)[head(o4,5)]
o5 = order(loadings[,5], decreasing=TRUE)
colnames(SocialMarket)[head(o5,5)]
o6 = order(loadings[,6], decreasing=TRUE)
colnames(SocialMarket)[head(o6,5)]
o7 = order(loadings[,7], decreasing=TRUE)
colnames(SocialMarket)[head(o7,5)]
o8 = order(loadings[,8], decreasing=TRUE)
colnames(SocialMarket)[head(o8,5)]
