library(MASS) 	## a library of example datasets
library(tidyverse)
library(nnet)  # for multinom
library(mosaic)

brca <- read.csv("./brca.csv")

# First question: are some radiologists more clinically conservative than others in recalling patients, holding patient risk factors equal?

# Some advice: imagine two radiologists who see the mammogram of a single patient, who has a specific set of risk factors. If radiologist A has a higher probability of recalling that patient than radiologist B, we'd say that radiologist A is more conservative (because they have a lower threshold for wanting to double-check the patient's results). So if all five radiologists saw the same set of patients, we'd easily find out whether some radiologists are more conservative than others. The problem is that the radiologists don't see the same patients. So we can't just look at raw recall rates-some radiologists might have seen patients whose clinical situation mandated more conservatism in the first place. Can you build a classification model that addresses this problem, i.e. that holds risk factors constant in assessing whether some radiologists are more conservative than others in recalling patients?


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
  
  ml1 = multinom(recall ~ .-cancer,
                 data=brca_train,
                 maxit = maxit)
  ml2 = multinom(recall ~ (.-cancer)^2, 
                 data=brca_train, 
                 maxit = maxit)
  
  # train models: cancer
  ml3 = multinom(cancer ~ recall,
                 data=brca_train,
                 maxit = maxit)
  ml4 = multinom(cancer ~ .,
                 data=brca_train,
                 maxit = maxit)
  ml5 = multinom(cancer ~ (.)^2, 
                 data=brca_train, 
                 maxit = maxit)
  
  # make predictions
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