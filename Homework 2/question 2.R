library(MASS) 	## a library of example datasets
library(tidyverse)
library(nnet)  # for multinom
library(mosaic)

brca <- read.csv("./brca.csv")

do_many = do(100)*{
  
  # split train and test set
  n = nrow(brca) #count all of the rows in the df
  n_train = round(0.8*n) #take 0.8 times n, then round to nearest
  train_ind = sort(sample.int(n, n_train, replace=FALSE)) #randomly sample from n, n_train times, without replacement, then sort
  n_test = n - n_train #number of n test is n minus n_train
  brca_train = brca[train_ind,] #create a dataframe of just the training
  brca_test = brca[-train_ind,] #create a dataframe of the just the test
  
  # train models: recall
  maxit = 10000
  
  ml1 = glm(recall ~ .-cancer,
                 data=brca_train,
                 maxit = maxit)
  ml2 = glm(recall ~ (.-cancer)^2, 
                 data=brca_train, 
                 maxit = maxit)
  
  # train models: cancer
  ml3 = glm(cancer ~ recall,
                 data=brca_train,
                 maxit = maxit)
  ml4 = glm(cancer ~ recall + history,
                 data=brca_train,
                 maxit = maxit)
  ml5 = glm(cancer ~ ., 
                 data=brca_train, 
                 maxit = maxit)
  
  # make predictions (this shit broke cause you changed models)
  predict_test = function(x){
    predict(x, newdata=brca_test, type='probs')
  }
  
  probhat1_test = predict_test(ml1)
  probhat2_test = predict_test(ml2)
  probhat3_test = predict_test(ml3)
  probhat4_test = predict_test(ml4)
  probhat5_test = predict_test(ml5)
  
  # here's a generic function for calculating out-of-sample deviance
  dev_out = function(y, probhat) {
    rc_pairs = cbind(seq_along(y), y)
    -2*sum(log(probhat[rc_pairs]))
  }
  
  # Calculate deviance
  dev_out(brca_test$cancer, probhat1_test)
  dev_out(brca_test$cancer, probhat2_test)
  dev_out(brca_test$cancer, probhat3_test)
  dev_out(brca_test$cancer, probhat4_test)
  dev_out(brca_test$cancer, probhat5_test)
  
  # out of sample classification error rate
  yhat_test = function(x){
    predict(x, newdata=brca_test, type='class')
  }
  
  yhat3_test = yhat_test(ml3)
  yhat4_test = yhat_test(ml4)
  yhat5_test = yhat_test(ml5)
  
  class_err = function(x){
    table(brca_test$cancer,x)
  }
  
  conf3 = class_err(yhat3_test)
  conf4 = class_err(yhat4_test)
  conf5 = class_err(yhat5_test)
  
  sum(diag(conf3))/n_test
  sum(diag(conf4))/n_test
  sum(diag(conf5))/n_test
  
  c(sum(diag(conf3))/n_test,
    sum(diag(conf4))/n_test,
    sum(diag(conf5))/n_test
  )
  
  #cbind(probhat3_test, brca_test$cancer)
  #cbind(probhat4_test, brca_test$cancer)
  #cbind(probhat5_test, brca_test$cancer)
}

do_many
names(do_many) = c("ml3","ml4","ml5")
colMeans(do_many)