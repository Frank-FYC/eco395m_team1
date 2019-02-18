library(tidyverse)
library(FNN)

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

function_knn <- function(x,knn_size,trim){
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
    geom_point(mapping = aes(x = mileage, y = price), color='black') + 
    theme_bw(base_size=18)
  p_test
  
  p_test + geom_path(aes(x = mileage, y = ypred_knn), color='blue')+ 
    geom_path(aes(x = mileage, y = ypred_lm2), color='red')+
    ggtitle(paste("S-Class",trim,"when KNN=",knn_size))
}

# Comparing RMSE to K for 350

rmse_k <- function(x,knn_size){

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
  
  rmse_lm1 <<- append(rmse_lm1,rmse(y_test, ypred_lm1))
  rmse_lm2 <<- append(rmse_lm2,rmse(y_test, ypred_lm2))
  rmse_knn <<- append(rmse_knn,rmse(y_test, ypred_knn))
  knn_value <<- append(knn_value,knn_size)
  knn_max_size <<- nrow(X_train)
}

knn_value <- vector()
rmse_lm1 <- vector()
rmse_lm2 <- vector()
rmse_knn <- vector()

### Ask prof how to do this.
for (i in 1:knn_max_size){
  rmse_k(sclass350,i)
}

# keep things inside the function, also that the fucntion doesn't have a mechanism to save the results.

test <- rmse_k(sclass350,10)
rmse_k(sclass350,20)
rmse_k(sclass350,30)
rmse_k(sclass350,40)
rmse_k(sclass350,50)
rmse_k(sclass350,60)
rmse_k(sclass350,70)
rmse_k(sclass350,80)
rmse_k(sclass350,90)
rmse_k(sclass350,100)
rmse_k(sclass350,110)
rmse_k(sclass350,120)
rmse_k(sclass350,130)
rmse_k(sclass350,140)
rmse_k(sclass350,150)
rmse_k(sclass350,160)
rmse_k(sclass350,170)
rmse_k(sclass350,180)
rmse_k(sclass350,190)
rmse_k(sclass350,200)
rmse_k(sclass350,210)
rmse_k(sclass350,220)
rmse_k(sclass350,230)
rmse_k(sclass350,240)
rmse_k(sclass350,250)
rmse_k(sclass350,260)
rmse_k(sclass350,270)
rmse_k(sclass350,280)
rmse_k(sclass350,290)
rmse_k(sclass350,300)
rmse_k(sclass350,310)
rmse_k(sclass350,320)
rmse_k(sclass350,330)
rmse_k(sclass350,knn_max_size)

rmse_k_df <- data.frame(knn_value,rmse_lm1,rmse_lm2,rmse_knn)

rmse_k_plot_1 <- ggplot(data=rmse_k_df)+
  geom_path(aes(x = knn_value, y = rmse_lm1), color='black')+
  geom_path(aes(x = knn_value, y = rmse_lm2), color='red')+
  geom_path(aes(x = knn_value, y = rmse_knn), color='blue')+
  xlab("KNN Value")+
  ylab("RMSE Value")+
  ggtitle("S-Class 350 RMSE to K")+
  scale_x_reverse()

# Comparing RMSE to K for 65amg
knn_value <- vector()
rmse_lm1 <- vector()
rmse_lm2 <- vector()
rmse_knn <- vector()

rmse_k(sclass65AMG,10)
rmse_k(sclass65AMG,20)
rmse_k(sclass65AMG,30)
rmse_k(sclass65AMG,40)
rmse_k(sclass65AMG,50)
rmse_k(sclass65AMG,60)
rmse_k(sclass65AMG,70)
rmse_k(sclass65AMG,80)
rmse_k(sclass65AMG,90)
rmse_k(sclass65AMG,100)
rmse_k(sclass65AMG,110)
rmse_k(sclass65AMG,120)
rmse_k(sclass65AMG,130)
rmse_k(sclass65AMG,140)
rmse_k(sclass65AMG,150)
rmse_k(sclass65AMG,160)
rmse_k(sclass65AMG,170)
rmse_k(sclass65AMG,180)
rmse_k(sclass65AMG,190)
rmse_k(sclass65AMG,200)
rmse_k(sclass65AMG,210)
rmse_k(sclass65AMG,220)
rmse_k(sclass65AMG,230)
rmse_k(sclass65AMG,knn_max_size)

rmse_k_df <- data.frame(knn_value,rmse_lm1,rmse_lm2,rmse_knn)

rmse_k_plot_2 <- ggplot(data=rmse_k_df)+
  geom_path(aes(x = knn_value, y = rmse_lm1), color='black')+
  geom_path(aes(x = knn_value, y = rmse_lm2), color='red')+
  geom_path(aes(x = knn_value, y = rmse_knn), color='blue')+
  xlab("KNN Value")+
  ylab("RMSE Value")+
  ggtitle("S-Class 65AMG RMSE to K")+
  scale_x_reverse()

min(rmse_k_df$rmse_knn)

# Optimal K

function_knn(sclass350,150,"350")

function_knn(sclass65AMG,60,"65 AMG")
