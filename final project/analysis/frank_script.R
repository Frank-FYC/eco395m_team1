library(tidyverse)
library(foreign)
library(dummy)
library(caret)
library(fastDummies)

df <- read.dta("../data/data_Deming_2008_0217.dta")


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
