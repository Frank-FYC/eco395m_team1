library(tidyverse)
library(foreign)

df <- read.dta("../data/nlsy_deming.dta")
df2 <- read.dta("../data/data_Deming_2008_0217.dta")

df_headstart <- subset(df,head_start==1)
df_nonheadstart <- subset(df,head_start==0)

birthweight <- subset(df2,BirthWeight=0:100)
