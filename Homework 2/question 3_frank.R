library(mosaic)
library(tidyverse)
library(FNN)
library(foreach)
library(nnet)
require(scales)

df=read.csv('./online_news.csv')

df = subset(df, select = -c(url) )

df=df[sample(nrow(df), 100),]

rmse = function(y, yhat) {
  sqrt( mean( (y - yhat)^2 ) )
}

rmse_model = do(100)*{

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
                weekday_is_friday 
              + num_videos 
              + data_channel_is_lifestyle 
              + global_rate_negative_words,
              data=df_train,
              maxit = maxit)
  lm3 <<- glm(shares ~ 
              . 
              - weekday_is_friday 
              - num_videos 
              - data_channel_is_lifestyle
              - global_rate_negative_words,
              data=df_train,
              maxit = maxit)
  lm4 <<- glm(shares ~ (.)^2,
              data=df_train,
              maxit = maxit)
  lm5 <<- glm(shares ~ 
                (weekday_is_friday 
              + num_videos 
              + data_channel_is_lifestyle 
              + global_rate_negative_words)^2,
              data=df_train,
              maxit = maxit)
  lm6 <<- glm(shares ~ 
                (. 
              - weekday_is_friday 
              - num_videos 
              - data_channel_is_lifestyle
              - global_rate_negative_words)^2,
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

# in-sample accuracy?
yhat_train = ifelse(predict(lm2) >= 1400, 1, 0)
y_train = ifelse(df_train$shares >= 1400, 1, 0)
table(y=y_train, yhat=yhat_train)

