#OLDPCA Analysis####

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
plot(fitted(pcr1), y_wine_quality)

# we can find best k, through train test splits, data validation. Auto-encoder = PCA. Dimensionality reduction. 

# Visualize the first few principal components:
# these are the coefficients in the linear combination for each summary
plot(seq(1,11,by=1), pc_wine$rotation[,1])
plot(seq(1,11,by=1), pc_wine$rotation[,2])
plot(seq(1,11,by=1), pc_wine$rotation[,3])


