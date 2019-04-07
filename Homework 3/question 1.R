library(tidyverse)
library(reshape2)
library(expss)
library(mosaic)
library(foreach)
library(gamlr) 

# load the main data
greenbuildings <- read.csv('greenbuildings.csv')

greenbuildings <- na.omit(greenbuildings)# there were na's in the set, so na.omit desn't use them but keeps them in the df.

# subsetting different classes
greenbuildings_a <- subset(greenbuildings,class_a=="1")
greenbuildings_b <- subset(greenbuildings,class_b=="1")
greenbuildings_c <- subset(greenbuildings,class_a=="0"&class_b=="0")

#use the saratoga script to model rent pricing#### 

# baseline medium model
lm_medium <- function(x){
  lm(Rent ~ cluster + size + stories + age + renovated + class_a + class_b + green_rating + amenities + Gas_Costs + Electricity_Costs + cluster_rent, data=x)
  }

# forward selection
lm_forward <- function(x){
  lm0 = lm(Rent ~ 1, data=x)
  lm_forward = step(lm0, direction='forward',scope=~(cluster + size + stories + age + renovated + class_a + class_b + green_rating + amenities + Gas_Costs + Electricity_Costs + cluster_rent)^2) 
}

# backward selection?
lm_backward <- function(x){
  lm(Rent ~ (cluster + size + stories + age + renovated + class_a + class_b + green_rating + amenities + Gas_Costs + Electricity_Costs + cluster_rent)^2, data=x)
}

# stepwise selection
# note that we start with a reasonable guess
lm_step <- function(x){
  step(lm(Rent ~ cluster + size + stories + age + renovated + class_a + class_b + green_rating + amenities + Gas_Costs + Electricity_Costs + cluster_rent, data=x), 
          scope=~(. + cluster + size + stories + age + renovated + class_a + class_b + green_rating + amenities + Gas_Costs + Electricity_Costs + cluster_rent)^2)
}

n.coefficients <- function(x){
  as.numeric(sapply(x, length)[1]) + 1
}

# the scope statement says:
# "consider all pairwise interactions for everything in lm_medium (.),
# along with the other variables explicitly named that weren't in medium"

# what variables are included?
getCall(lm_step)
coef(lm_step)


# goal 1: best model
lm_medium_all <- lm_medium(greenbuildings)
lm_forward_all <- lm_forward(greenbuildings)
lm_backward_all <- drop1(lm_backward(greenbuildings))
lm_step_all <- lm_step(greenbuildings)

getCall(lm_backward_all)

# goal 2: quantify average change
coef(lm_medium_all)["green_rating"]
coef(lm_forward_all)["green_rating"]
coef(lm_backward_all)["green_rating"]
coef(lm_step_all)["green_rating"]

# Goal 3: for class_a and class_b comparison.

lm_medium_a <- lm_medium(greenbuildings_a)
lm_forward_a <- lm_forward(greenbuildings_a)
lm_backward_a <- drop1(lm_backward(greenbuildings_a))
lm_step_a <- lm_step(greenbuildings_a)

lm_medium_b <- lm_medium(greenbuildings_b)
lm_forward_b <- lm_forward(greenbuildings_b)
lm_backward_b <- drop1(lm_backward(greenbuildings_b))
lm_step_b <- lm_step(greenbuildings_b)

coef(lm_medium_a)["green_rating"]
coef(lm_forward_a)["green_rating"]
coef(lm_backward_a)["green_rating"]
coef(lm_step_a)["green_rating"]

coef(lm_medium_b)["green_rating"]
coef(lm_forward_b)["green_rating"]
coef(lm_backward_b)["green_rating"]
coef(lm_step_b)["green_rating"]

#use the gasoline script to model rent pricing, do we see multicolinearity?####

##the following doesn't apply, because the variables are not the same

x = as.matrix(greenbuildings[,-5])
y = as.matrix(greenbuildings[,5])

mu_x = colMeans(x)
nir_variables = seq(1, 22, by=1)
#par(mfrow=c(2,2))
#for(i in sample.int(nrow(x), 4)) {
# plot(nir_variables, x[i,] - mu_x, main=i)
#}

sigma_x = cor(x)
sigma_x[1:10,1:10]

# Let's try dimensionality reduction via PCA
pc_greenbuildings = prcomp(x, scale=TRUE)

# pc_greenbuildings$x has the summary variables
# Regress on the first K
K = 3
rent = pc_greenbuildings$x[,1:K]
pcr1 = lm(y ~ rent)

summary(pcr1)

plot(fitted(pcr1), y)

# Visualize the first few principal components:
# these are the coefficients in the linear combination for each summary
plot(nir_variables, pc_greenbuildings$rotation[,1])
plot(nir_variables, pc_greenbuildings$rotation[,2])
plot(nir_variables, pc_greenbuildings$rotation[,3])

