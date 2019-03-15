library(mosaic)
library(tidyverse)
library(FNN)
library(foreach)
library(nnet)
require(scales)

df = read.csv('./online_news.csv')

df = subset(df, select = -c(url) )



rmse = function(y, yhat) {
  sqrt( mean( (y - yhat)^2 ) )
}

rmse_model = do(10)*{
  df = df[sample(nrow(df), 1000),]
  n = nrow(df)
  n_train = round(0.8*n) #take x times n, then round to nearest
  n_test = n - n_train #number of n test is n minus n_train
  
  train_ind = sort(sample.int(n, n_train, replace=FALSE)) # randomly sample from n, n_train times, without replacement, then sort
  df_train = df[train_ind,]
  df_test = df[-train_ind,]
  y_df_train = df_train$shares
  y_df_test = df_test$shares
  #####
  # train models: recall # num_videos data_channel_is_lifestyle 
  maxit <- 10000
  
  lm1 <<- glm(shares ~ .,
              data=df_train,
              maxit = maxit)
  lm2 <<- glm(shares ~ 
                n_tokens_content 
              + num_imgs 
              + data_channel_is_world 
              + max_negative_polarity,
              data=df_train,
              maxit = maxit)
  lm3 <<- glm(shares ~ 
              . 
              - n_tokens_content 
              - num_imgs 
              - data_channel_is_world
              - max_negative_polarity,
              data=df_train,
              maxit = maxit)
  lm4 <<- glm(shares ~ (.)^2,
              data=df_train,
              maxit = maxit)
  lm5 <<- glm(shares ~ 
                (n_tokens_content 
              + num_imgs 
              + data_channel_is_world 
              + max_negative_polarity)^2,
              data=df_train,
              maxit = maxit)
  lm6 <<- glm(shares ~ 
                (. 
              - n_tokens_content 
              - num_imgs 
              - data_channel_is_world
              - max_negative_polarity)^2,
              data=df_train,
              maxit = maxit)
  
  yhat_test1 = predict(lm1,df_test)
  yhat_test2 = predict(lm2,df_test)
  yhat_test3 = predict(lm3,df_test)
  yhat_test4 = predict(lm4,df_test)
  yhat_test5 = predict(lm5,df_test)
  yhat_test6 = predict(lm6,df_test)
  
  c(rmse(df_test$shares, yhat_test1),
    rmse(df_test$shares, yhat_test2),
    rmse(df_test$shares, yhat_test3),
    rmse(df_test$shares, yhat_test4),
    rmse(df_test$shares, yhat_test5),
    rmse(df_test$shares, yhat_test6)
  )
}

rmse_model
colMeans(rmse_model)

# in-sample and out of sample accuracy?
n = nrow(df)
n_train = round(0.8*n) #take x times n, then round to nearest
n_test = n - n_train #number of n test is n minus n_train

train_ind = sort(sample.int(n, n_train, replace=FALSE)) # randomly sample from n, n_train times, without replacement, then sort
df_train = df[train_ind,]
df_test = df[-train_ind,]

sample.accuracy = function(x){
  yhat_train = ifelse(predict(x) >= 1400, 1, 0)
  y_train = ifelse(df_train$shares >= 1400, 1, 0)
  in.sample_table <<- table(y=y_train, yhat=yhat_train)
  
  probhat_test = predict(x, newdata=df_test)
  yhat_test = ifelse(probhat_test >= 1400, 1, 0)
  y_test = ifelse(df_test$shares >= 1400, 1, 0)
  out.sample_table <<- table(y=y_test, yhat=yhat_test)
}

sample.accuracy(lm1)
in.sample_table
out.sample_table

sample.accuracy(lm2)
in.sample_table
out.sample_table

sample.accuracy(lm3)
in.sample_table
out.sample_table

sample.accuracy(lm4)
in.sample_table
out.sample_table

sample.accuracy(lm5)
in.sample_table
out.sample_table

sample.accuracy(lm6)
in.sample_table
out.sample_table