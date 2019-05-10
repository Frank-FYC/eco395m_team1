library(tidyverse)
library(foreign)
library(dummy)
library(caret)
library(fastDummies)

df <- read.dta("../data/data_Deming_2008_0217.dta")


<<<<<<< HEAD
some code

sgerety4h5h
=======
Age_1stHS88 
Age_1stHS90 
Age_1stHS92 
Age_1stHS94 
Age_1stHS96 
Age_1stHS98 
Age_1stHS100 
Age_1stHS102 
Age_1stHS104

varlist <- c("BirthWeight")

mental.disability <- ifelse(rowSums(select(df,Retard86:Retard100),na.rm = TRUE)==0,0,1)
>>>>>>> 1d40040b77dbaa9e599d1ff0a066e9b3b2ba0891
