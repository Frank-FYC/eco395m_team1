```{r question 2 code,include=FALSE}
rm(list=ls())

library(MASS) 	## a library of example datasets
library(tidyverse)
library(nnet)  # for multinom
library(mosaic)

options(warn=-1)

brca <- read.csv("./brca.csv")

#####
#First question: are some radiologists more clinically conservative than others in recalling patients, holding patient risk factors equal?

# Second question: when the radiologists at this hospital interpret a mammogram to make a decision on whether to recall the patient, does the data suggest that they should be weighing some clinical risk factors more heavily than they currently are?

rmse = function(y, yhat) {
  sqrt( mean( (y - yhat)^2 ) )
}

df_rad13 = subset(brca,radiologist=="radiologist13")
df_rad34 = subset(brca,radiologist=="radiologist34")
df_rad66 = subset(brca,radiologist=="radiologist66")
df_rad89 = subset(brca,radiologist=="radiologist89")
df_rad95 = subset(brca,radiologist=="radiologist95")

probtest = function(a,b){
  n = nrow(a)
  n_train = round(0.8*n) #take x times n, then round to nearest
  n_test = n - n_train #number of n test is n minus n_train
  
  train_ind = sort(sample.int(n, n_train, replace=FALSE)) # randomly sample from n, n_train times, without replacement, then sort
  brca_train = select(a[train_ind,],-radiologist)
  brca_test = select(a[-train_ind,],-radiologist)
  brca.w_test = select(subset(brca,radiologist != b),-radiologist)
  
  # train models: recall
  maxit = 10000
  
  lm1 = glm(recall ~ .-cancer,
            data=brca_train,
            maxit = maxit)
  lm2 = glm(recall ~ (.-cancer)^2, 
            data=brca_train, 
            maxit = maxit)
  # train models: cancer
  lm3 = glm(cancer ~ recall,
            data=brca_train,
            maxit = maxit)
  lm4 = glm(cancer ~ recall + history,
            data=brca_train,
            maxit = maxit)
  lm5 = glm(cancer ~ ., 
            data=brca_train, 
            maxit = maxit)
  
  # predict on the specific radiologist testing set
  yhat_test1 = predict(lm1,brca_test)
  yhat_test2 = predict(lm2,brca_test) 
  
  yhat_test1.w = predict(lm1,brca.w_test)
  yhat_test2.w = predict(lm2,brca.w_test)
  
  yhat_test3 = predict(lm3,brca_test)
  yhat_test4 = predict(lm4,brca_test)
  yhat_test5 = predict(lm5,brca_test)
  
  yhat_test3.w = predict(lm3,brca.w_test)
  yhat_test4.w = predict(lm4,brca.w_test)
  yhat_test5.w = predict(lm5,brca.w_test)
  
  # predict on the other radiologists testing set
  
  rmse = function(y, yhat) {
    sqrt( mean( (y - yhat)^2 ) )
  }
  
  c(rmse(brca_test$recall, yhat_test1),
    rmse(brca_test$recall, yhat_test2),
    
    rmse(brca.w_test$recall, yhat_test1.w),
    rmse(brca.w_test$recall, yhat_test2.w),
    
    rmse(brca_test$recall, yhat_test3),
    rmse(brca_test$recall, yhat_test4),
    rmse(brca_test$recall, yhat_test5),
    
    rmse(brca.w_test$recall, yhat_test3.w),
    rmse(brca.w_test$recall, yhat_test4.w),
    rmse(brca.w_test$recall, yhat_test5.w)
  )
}

# radiologist13 radiologist34 radiologist66 radiologist89 radiologist95

rad13 = do(100)*{probtest(df_rad13,"radiologist13")}
rad34 = do(100)*{probtest(df_rad34,"radiologist34")}
rad66 = do(100)*{probtest(df_rad66,"radiologist66")}
rad89 = do(100)*{
  tryCatch({
    probtest(df_rad89,"radiologist89")
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}
rad95 = do(100)*{probtest(df_rad95,"radiologist95")}
superrad = do(100)*{probtest(brca,"")}

df = as.data.frame(
  rbind(
    colMeans(rad13),
    colMeans(rad34),
    colMeans(rad66),
    colMeans(rad89),
    colMeans(rad95),
    colMeans(superrad)
  )
)
rownames(df) = c("radiologist13","radiologist34","radiologist66","radiologist89","radiologist95","SuperRad")
colnames(df) = c("lm1","lm2","lm1.w","lm2.w","lm3","lm4","lm5","lm3.w","lm4.w","lm5.w")

df = as.data.frame(t(df))
df$Rad13.compare = (df$radiologist13 - df$SuperRad)
df$Rad34.compare = (df$radiologist34 - df$SuperRad)
df$Rad66.compare = (df$radiologist66 - df$SuperRad)
df$Rad89.compare = (df$radiologist89 - df$SuperRad)
df$Rad95.compare = (df$radiologist95 - df$SuperRad)

# a confusion table to determine the best way to detect cancer

n = nrow(brca)
n_train = round(0.8*n) #take x times n, then round to nearest
n_test = n - n_train #number of n test is n minus n_train

train_ind = sort(sample.int(n, n_train, replace=FALSE)) # randomly sample from n, n_train times, without replacement, then sort
brca_train = brca[train_ind,]
brca_test = brca[-train_ind,]

maxit = 1000

lm3 = lm(cancer ~ recall,
         data=brca_train,
         maxit = maxit)
lm4 = lm(cancer ~ recall + history,
         data=brca_train,
         maxit = maxit)
lm5 = lm(cancer ~ ., 
         data=brca_train, 
         maxit = maxit)

lm3_probhat_test = predict(lm3, newdata=brca_test)
lm4_probhat_test = predict(lm4, newdata=brca_test)
lm5_probhat_test = predict(lm5, newdata=brca_test)

lm3_yhat_test = ifelse(lm3_probhat_test >= 0.1, 1, 0)
lm4_yhat_test = ifelse(lm4_probhat_test >= 0.1, 1, 0)
lm5_yhat_test = ifelse(lm5_probhat_test >= 0.1, 1, 0)

table(y=brca_test$cancer, yhat=lm3_yhat_test)
table(y=brca_test$cancer, yhat=lm4_yhat_test)
table(y=brca_test$cancer, yhat=lm5_yhat_test)

```