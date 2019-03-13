library(mosaic)
library(tidyverse)
library(FNN)
library(foreach)
library(nnet)

df=read.csv('./online_news.csv')
online_news=df[sample(nrow(df), 100), ]

# The variables involved
summary(online_news)

# Focus on the following trim level: >=1400
viral = subset(online_news, shares >= 1400)
dim(viral)

#Find a plot that shows an intersting relationship with 'viral'.
plot(shares~n_tokens_title, data=viral)
plot(log(shares)~n_tokens_title, data=viral)
plot(log(shares)~title_sentiment_polarity, data=viral)
plot(log(shares)~title_subjectivity, data=viral)

#I like 'n_tokens_title' the most, so let's use this.

rmse_model = do(2)*{

  # Create a train/test split for y=shares and x=n_tokens_title
  N=nrow(viral)
  N_Train=floor(0.8*N)
  N_Train_Ind=sample.int(N, N_Train, replace=FALSE)
  viral_train = viral[N_Train_Ind,]
  viral_test = viral[-N_Train_Ind,]
  y_train_viral = viral_train$shares
  X_train_viral = data.frame(n_tokens_title = viral_train$n_tokens_title)
  y_test_viral = viral_test$shares
  X_test_viral = data.frame(n_tokens_title = viral_test$n_tokens_title)
  
  rmse = function(y, ypred) {
    sqrt(mean((y-ypred)^2))
  }
  
  k_grid = unique(round(exp(seq(log(N_Train), log(2), length=100))))
  rmse_grid_out = foreach(k = k_grid, .combine='c') %do% {
    knn_model = knn.reg(X_train_viral, X_test_viral, y_train_viral, k = k)
    rmse(y_test_viral, knn_model$pred)
  }
  
  rmse_grid_out = data.frame(K = k_grid, RMSE = rmse_grid_out)
  
  
  revlog_trans <- function(base = exp(1)) {
    require(scales)
    ## Define the desired transformation.
    trans <- function(x){
      -log(x, base)
    }
    ## Define the reverse of the desired transformation
    inv <- function(x){
      base^(-x)
    }
    ## Creates the transformation
    scales::trans_new(paste("revlog-", base, sep = ""),
                      trans,
                      inv,  ## The reverse of the transformation
                      log_breaks(base = base), ## default way to define the scale breaks
                      domain = c(1e-100, Inf) 
    )
  }
  
  ind_best = which.min(rmse_grid_out$RMSE)
  k_best = k_grid[ind_best]
  
  c(k_best)

}

rmse_model
colMeans(rmse_model)

p_out = ggplot(data=rmse_grid_out) + 
  geom_path(aes(x=K, y=RMSE, color='testset'), size=1.5) +
  xlim(0, 100)


which(rmse_grid_out==min(rmse_grid_out),arr.ind=TRUE)
which.min(rmse_grid_out$RMSE)

p_out + geom_vline(xintercept=k_best, color='darkgreen', size=1.5)

# 


