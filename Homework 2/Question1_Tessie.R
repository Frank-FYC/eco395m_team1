library(tidyverse)
library(FNN)
library(mosaic)
library(psycho)
data(SaratogaHouses)

summary(SaratogaHouses)

z_SaratogaHouses <- SaratogaHouses %>% 
  psycho::standardize() 

summary(z_SaratogaHouses)

## Taking back-up of the input file, in case the original data is required later
z_shbc <- z_SaratogaHouses


## Convert the dependent var to factor. Normalize the numeric variables  

##z_SaratogaHouses$Default <- factor(z_SaratogaHouses$Default)
num.vars <- sapply(z_SaratogaHouses, is.numeric)
z_SaratogaHouses[num.vars] <- lapply(z_SaratogaHouses[num.vars], scale)

summary(z_SaratogaHouses)

##Generate caterical variable dummies
results <- fastDummies::dummy_cols(z_SaratogaHouses)
summary(results)

myvars <- c("landValue", "livingArea", "bedrooms", "centralAir_Yes", "bathrooms", "waterfront_Yes", "age", "fuel_electric", "fuel_gas", "fuel_oil", "lotSize", "newConstruction_Yes")
results.subset <- results[myvars]

summary(results.subset)

# Baseline model
lm_small = lm(price ~ bedrooms + centralAir + landValue + livingArea, data=results)

# 10 main effects
lm_medium = lm(price ~ . - sewer - rooms - fireplaces - heating - pctCollege, data=results)

coef(lm_small)
coef(lm_medium)

# All interactions
# the ()^2 says "include all pairwise interactions"
lm_big = lm(price ~ (. - sewer - rooms - fireplaces - heating - pctCollege)^2, data=results)


####
# Compare out-of-sample predictive performance
####

# Split into training and testing sets
n = nrow(results)
n_train = round(0.8*n)  # round to nearest integer
n_test = n - n_train
train_cases = sample.int(n, n_train, replace=FALSE)
test_cases = setdiff(1:n, train_cases)
saratoga_train = results[train_cases,]
saratoga_test = results[test_cases,]

# Fit to the training data
lm1 = lm(price ~ bedrooms + centralAir + landValue + livingArea, data=saratoga_train)
lm2 = lm(price ~ . - sewer - rooms - fireplaces - heating - pctCollege, data=saratoga_train)
lm3 = lm(price ~ (. - sewer - rooms - fireplaces - heating - pctCollege)^2, data=saratoga_train)

# Predictions out of sample
yhat_test1 = predict(lm1, saratoga_test)
yhat_test2 = predict(lm2, saratoga_test)
yhat_test3 = predict(lm3, saratoga_test)

rmse = function(y, yhat) {
  sqrt( mean( (y - yhat)^2 ) )
}

# Root mean-squared prediction error
rmse(saratoga_test$price, yhat_test1)
rmse(saratoga_test$price, yhat_test2)
rmse(saratoga_test$price, yhat_test3)

# easy averaging over train/test splits
library(mosaic)

rmse_vals = do(100)*{
  
  # re-split into train and test cases
  n_train = round(0.8*n)  # round to nearest integer
  n_test = n - n_train
  train_cases = sample.int(n, n_train, replace=FALSE)
  test_cases = setdiff(1:n, train_cases)
  saratoga_train = results[train_cases,]
  saratoga_test = results[test_cases,]
  
  # fit to this training set
  lm2 = lm(price ~ . - sewer - rooms - fireplaces - heating - pctCollege, data=saratoga_train)
  
  lm_boom = lm(price ~ bedrooms + centralAir + landValue + livingArea + bathrooms +
                 landValue*livingArea + bedrooms*bathrooms + bedrooms*landValue, data=saratoga_train)
  
  lm_biggerboom = lm(price ~ bedrooms + centralAir + landValue + livingArea + bathrooms + waterfront + lotSize + age +
                       fuel + newConstruction + 
                       landValue*lotSize + bathrooms*rooms + age*newConstruction + 
                       bedrooms*bathrooms + centralAir*fuel + landValue*waterfront, data=saratoga_train)
  
  
  # predict on this testing set
  yhat_test2 = predict(lm2, saratoga_test)
  yhat_testboom = predict(lm_boom, saratoga_test)
  yhat_testbiggerboom = predict(lm_biggerboom, saratoga_test)
  c(rmse(saratoga_test$price, yhat_test2),
    rmse(saratoga_test$price, yhat_testboom),
    rmse(saratoga_test$price, yhat_testbiggerboom))
}

rmse_vals
colMeans(rmse_vals)
V1 <- mean(rmse_vals$V1)
V2 <- mean(rmse_vals$V2)
V3 <- mean(rmse_vals$V3)



####
# Compare out-of-sample predictive performance using KNN
####

# Split into training and testing sets
n = nrow(results)
n_train = round(0.8*n)  # round to nearest integer
n_test = n - n_train
train_cases = sample.int(n, n_train, replace=FALSE)
test_cases = setdiff(1:n, train_cases)
saratoga_train = results[train_cases,]
saratoga_test = results[test_cases,]


#####
# Train/test split
#####

# randomly sample a set of data points to include in the training set
train_ind = sample.int(n, n_train, replace=FALSE)

# Define the training and testing set
D_train = results[train_ind,]
D_test = results[-train_ind,]

X_train = dplyr::select(D_train, myvars)
y_train = dplyr::select(D_train, price)
X_test = dplyr::select(D_test, myvars)
y_test = dplyr::select(D_test, price)

# Running KNN
rmse = function(y, ypred) {
  sqrt(mean(data.matrix((y - ypred) ^ 2)))
}


# Multiple K values function
knn_max = nrow(X_train)
khat <- c()
for (i in 3:knn_max)
{
  knn = knn.reg(
    train = X_train,
    test = X_test,
    y = y_train,
    k = i
  )
  names(knn)
  ypred_knn = knn$pred
  #####
  # Compare the models by RMSE_out
  #####
  khat[i] = rmse(y_test, ypred_knn)
 }

####
# plot the fit
####

K = c(1:knn_max)
RMSE = (khat)
rmse_k = ggplot() + geom_path(aes(x = K, y = RMSE), color="red") +
geom_path(aes(x = K, y = mean(rmse_vals$V1)), color="green") +
geom_path(aes(x = K, y = mean(rmse_vals$V2)), color="brown") +
geom_path(aes(x = K, y = mean(rmse_vals$V3)), color="purple")+
  theme_bw(base_size = 18)

rmse_k

rmse_k1 = ggplot() + geom_path(aes(x = K, y = RMSE), color="red") +
  geom_path(aes(x = K, y = mean(rmse_vals$V1)), color="green") +
  geom_path(aes(x = K, y = mean(rmse_vals$V2)), color="brown") +
  geom_path(aes(x = K, y = mean(rmse_vals$V3)), color="purple")+
  theme_bw(base_size = 18) +
  xlim(0, 500)

rmse_k1

