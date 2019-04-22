gasoline = read.csv('gasoline.csv', header=TRUE)
head(gasoline)

x_gasoline = as.matrix(gasoline[,-1])
y_gasoline = gasoline[,1]


# Ordinary least squares
lm1 = lm(y_gasoline ~ x_gasoline)

# Yikes!
summary(lm1)
dim(x_gasoline)

# Plot a random sample of the NIR spectra
mu_x = colMeans(x_gasoline)
nir_wavelength = seq(900, 1700, by=2)
par(mfrow=c(2,2))
for(i in sample.int(nrow(x_gasoline), 4)) {
	plot(nir_wavelength, x_gasoline[i,] - mu_x, main=i, ylim=c(-0.1,0.1))
}

# They all differ from the mean in very structured ways
# Extremely strong collinearity among the predictor variables
sigma_X = cor(x_gasoline)
sigma_X[1:10,1:10]


# Let's try dimensionality reduction via PCA
pc_gasoline = prcomp(x_gasoline, scale=TRUE)

# pc_gasoline$x_gasoline has the summary variables
# Regress on the first K
K = 3
scores = pc_gasoline$x[,1:K]
pcr1 = lm(y_gasoline ~ scores)

summary(pcr1)

# Show the model fit
plot(fitted(pcr1), y_gasoline)

# we can find best k, through train test splits, data validation. Auto-encoder = PCA. Dimensionality reduction. 

# Visualize the first few principal components:
# these are the coefficients in the linear combination for each summary
plot(nir_wavelength, pc_gasoline$rotation[,1], ylim=c(-0.15,0.15))
plot(nir_wavelength, pc_gasoline$rotation[,2], ylim=c(-0.15,0.15))
plot(nir_wavelength, pc_gasoline$rotation[,3], ylim=c(-0.15,0.15))
