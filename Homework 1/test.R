library(tidyverse)

sclass = read.csv('sclass.csv')
sclass <- select(sclass,trim,mileage,price)

# The variables involved

# plot the data
#ggplot(data = sclass) + 
#  geom_point(mapping = aes(x = mileage, y = price), color='darkgrey')
#summary(sclass)

# Focus on 2 trim levels: 350 and 65 AMG
sclass350 = subset(sclass, trim == '350')

sclass65AMG = subset(sclass, trim == '65 AMG')

# create a function to automate everything

function_knn <- function(x){
  # Make a train-test split
  N = nrow(x)
  N_train = floor(0.8*N)
  N_test = N - N_train
  
  #####
  # Train/test split
  #####
  
  # randomly sample a set of data points to include in the training set
  train_ind = sample.int(N, N_train, replace=FALSE)
  
  # Define the training and testing set
  D_train = x[train_ind,]
  D_test = x[-train_ind,]
  
  # optional book-keeping step:
  # reorder the rows of the testing set by the mileage
  D_test = arrange(D_test, mileage)
  head(D_test)
  
  # Now separate the training and testing sets into features (X) and outcome (y)
  X_train = select(D_train, mileage)
  y_train = select(D_train, price)
  X_test = select(D_test, mileage)
  y_test = select(D_test, price)
  
  
  #####
  # Fit a few models
  #####
  
  # linear and quadratic models
  lm1 = lm(price ~ mileage, data=D_train)
  lm2 = lm(price ~ poly(mileage, 2), data=D_train)
  
  # KNN
  knn = knn.reg(train = X_train, test = X_test, y = y_train, k=knn_size)
  
  #####
  # Compare the models by RMSE_out
  #####
  
  # define a helper function for calculating RMSE
  rmse = function(y, ypred) {
    sqrt(mean(data.matrix((y-ypred)^2)))
  }
  
  ypred_lm1 = predict(lm1, X_test)
  ypred_lm2 = predict(lm2, X_test)
  ypred_knn = knn$pred
  
  rmse(y_test, ypred_lm1)
  rmse(y_test, ypred_lm2)
  rmse(y_test, ypred_knn)
  
  
  ####
  # plot the fit
  ####
  
  # attach the predictions to the test data frame
  D_test$ypred_lm2 = ypred_lm2
  D_test$ypred_knn = ypred_knn
  
  p_test = ggplot(data = D_test) + 
    geom_point(mapping = aes(x = mileage, y = price), color='lightgrey') + 
    theme_bw(base_size=18)
  p_test
  
  p_test + geom_path(aes(x = mileage, y = ypred_knn), color='red')+ 
    geom_path(aes(x = mileage, y = ypred_lm2), color='blue')+
    ggtitle(paste("sclass",trim,"when KNN=",knn_size))
}

knn_size <- 10
trim <- 350
function_knn(sclass350)


#function_knn(sclass65AMG)

