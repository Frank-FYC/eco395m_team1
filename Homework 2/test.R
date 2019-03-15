####################################

#do_many = do(100)*{}

# split train and test set
n = nrow(brca) #count all of the rows in the df
n_train = round(0.8*n) #take 0.8 times n, then round to nearest
train_ind = sort(sample.int(n, n_train, replace=FALSE)) #randomly sample from n, n_train times, without replacement, then sort
n_test = n - n_train #number of n test is n minus n_train
brca_train = brca[train_ind,] #create a dataframe of just the training
brca_test = brca[-train_ind,] #create a dataframe of the just the test

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

# make predictions (this shit broke cause you changed models)
predict_test = function(x){
  predict(x, newdata=brca_test, type='probs')
}

probhat1_test = predict_test(lm1)
probhat2_test = predict_test(lm2)
probhat3_test = predict_test(lm3)
probhat4_test = predict_test(lm4)
probhat5_test = predict_test(lm5)

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

yhat3_test = yhat_test(lm3)
yhat4_test = yhat_test(lm4)
yhat5_test = yhat_test(lm5)

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

do_many
names(do_many) = c("lm3","lm4","lm5")
colMeans(do_many)

####################################

probtest = function(x){
  
  df = subset(brca,radiologist=x)
  n = nrow(df)
  n_train = round(0.5*n) #take 0.8 times n, then round to nearest
  train_ind = sort(sample.int(n, n_train, replace=FALSE)) #randomly sample from n, n_train times, without replacement, then sort
  n_test = n - n_train #number of n test is n minus n_train
  brca_train = df[train_ind,] #create a dataframe of just the training
  brca_test = df[-train_ind,] #create a dataframe of the just the test
  
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
  
  # predict on this testing set
  yhat_test1 = predict(lm1,brca_test)
  yhat_test2 = predict(lm2,brca_test)  
  yhat_test3 = predict(lm3,brca_test)
  yhat_test4 = predict(lm4,brca_test)
  yhat_test5 = predict(lm5,brca_test)
  
  rmse = function(y, yhat) {
    sqrt( mean( (y - yhat)^2 ) )
  }
  
  c(rmse(brca_test$recall, yhat_test1),
    rmse(brca_test$recall, yhat_test2),
    rmse(brca_test$recall, yhat_test3),
    rmse(brca_test$recall, yhat_test4),
    rmse(brca_test$recall, yhat_test5)
  )
  
}

probtest(radiologist13)


####################################
a = df_rad13
b = "radiologist13"

n = nrow(df_rad13)
n_train = round(0.5*n) #take x times n, then round to nearest
n_test = n - n_train #number of n test is n minus n_train

train_ind = sort(sample.int(n, n_train, replace=FALSE)) # randomly sample from n, n_train times, without replacement, then sort
brca_train = select(df_rad13[train_ind,],-radiologist)  #create a dataframe of just the training


brca_test = select(df_rad13[-train_ind,],-radiologist) #create a dataframe of the just the test
brca_whole.test = select(subset(brca,radiologist=!radiologist13),-radiologist)

# train models: recall
maxit = 10000

lm1 = glm(recall ~ .-cancer,
          data=brca_whole.test,
          maxit = maxit)
lm2 = glm(recall ~ (.-cancer)^2, 
          data=brca_train, 
          maxit = maxit,
          family=binomial)

# train models: cancer
lm3 = glm(cancer ~ recall,
          data=brca_train,
          maxit = maxit)
lm4 = glm(cancer ~ recall + history,
          data=brca_train,
          maxit = maxit,
          family=binomial)
lm5 = glm(cancer ~ ., 
          data=brca_train, 
          maxit = maxit,
          family=binomial)

# predict on the specific radiologist testing set
yhat_test1 = predict(lm1,brca_test)
yhat_test2 = predict(lm2,brca_test)  
yhat_test3 = predict(lm3,brca_test)
yhat_test4 = predict(lm4,brca_test)
yhat_test5 = predict(lm5,brca_test)

# predict on the other radiologists testing set

rmse = function(y, yhat) {
  sqrt( mean( (y - yhat)^2 ) )
}

c(rmse(brca_test$recall, yhat_test1),
  rmse(brca_test$recall, yhat_test2),
  rmse(brca_test$recall, yhat_test3),
  rmse(brca_test$recall, yhat_test4),
  rmse(brca_test$recall, yhat_test5)
)

#####
library(MASS) 	## a library of example datasets
library(tidyverse)
library(nnet)  # for multinom
library(mosaic)

options(warn=-1)

this.dir <- dirname(parent.frame(2)$ofile)
setwd(this.dir)

brca <- read.csv("./brca.csv")

#####
#First question: are some radiologists more clinically conservative than others in recalling patients, holding patient risk factors equal?

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
  
  lm1.w = glm(recall ~ .-cancer,
              data=brca_train,
              maxit = maxit)
  lm2.w = glm(recall ~ (.-cancer)^2, 
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
  
  lm3.w = glm(cancer ~ recall,
              data=brca_train,
              maxit = maxit)
  lm4.w = glm(cancer ~ recall + history,
              data=brca_train,
              maxit = maxit)
  lm5.w = glm(cancer ~ ., 
              data=brca_train, 
              maxit = maxit)
  
  # predict on the specific radiologist testing set
  yhat_test1 = predict(lm1,brca_test)
  yhat_test2 = predict(lm2,brca_test) 
  
  yhat_test1.w = predict(lm1.w,brca.w_test)
  yhat_test2.w = predict(lm2.w,brca.w_test)
  
  yhat_test3 = predict(lm3,brca_test)
  yhat_test4 = predict(lm4,brca_test)
  yhat_test5 = predict(lm5,brca_test)
  
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
    rmse(brca_test$recall, yhat_test5)
  )
}

# radiologist13 radiologist34 radiologist66 radiologist89 radiologist95

rad1 = do(100)*{probtest(df_rad13,"radiologist13")}
names(rad1) = c("lm1","lm2","lm1.w","lm2.w","lm3","lm4","lm5")
rad2 = do(100)*{probtest(df_rad34,"radiologist34")}
names(rad2) = c("lm1","lm2","lm1.w","lm2.w","lm3","lm4","lm5")
rad3 = do(100)*{probtest(df_rad66,"radiologist66")}
names(rad3) = c("lm1","lm2","lm1.w","lm2.w","lm3","lm4","lm5")
rad4 = do(100)*{
  tryCatch({
    probtest(df_rad89,"radiologist89")
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}
names(rad4) = c("lm1","lm2","lm1.w","lm2.w","lm3","lm4","lm5")
rad5 = do(100)*{probtest(df_rad95,"radiologist95")}
names(rad5) = c("lm1","lm2","lm1.w","lm2.w","lm3","lm4","lm5")

prob_table = rbind(
  colMeans(rad1),
  colMeans(rad2),
  colMeans(rad3),
  colMeans(rad4),
  colMeans(rad5))

rownames(prob_table) = c("radiologist13",
                         "radiologist34",
                         "radiologist66",
                         "radiologist89",
                         "radiologist95"
)

prob_table


#####
# Second question: when the radiologists at this hospital interpret a mammogram to make a decision on whether to recall the patient, does the data suggest that they should be weighing some clinical risk factors more heavily than they currently are?