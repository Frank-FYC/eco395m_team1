#call libraries
library(tidyverse)
library(foreign)
library(psycho)
library(reshape2)
library(expss)
library(mosaic)
library(foreach)
library(gamlr)
library(stargazer)

#read data
dt = read.csv('../data/finaldataset.csv')

da <- dt[complete.cases(dt$LogInc_0to3),]
db <- da[complete.cases(da$Father_HH_0to3),]
dc <- db[complete.cases(db$MothED),]
dd <- dc[complete.cases(dc$FirstBorn),]
de <- dd[complete.cases(dd$Age_Moth_Birth),]
dg <- de[complete.cases(de$Repeat),]

dh <- dg[which(dg$LogInc_0to3<=10),]
df <- dh[which(dh$mentaldisability==0),]


#summarize data
summary(df$Repeat)


##########################################################################33

############Effect of HS on Repeat grade

# baseline medium model

lm_one <- glm(repeatgrade ~  headstart, data=df, family = binomial)

lm_few <- glm(repeatgrade ~  headstart+Hispanic+Black+MothED+learndisability, data=df, family = binomial)

lm_medium <- glm(repeatgrade ~  headstart+Hispanic+Black+headstart*Hispanic+headstart*Black+Male+MothED+learndisability, data=df, family = binomial)

stargazer::stargazer(lm_one, lm_few, lm_medium, apply.coef=exp, type="text", title="Odds ratio of Repetition of grade on Headstart participation")

############################################################################

###############Effect of HS on SRH

di <- df[complete.cases(df$PoorHealth),]
dj <- di[complete.cases(di$logBW),]

# baseline medium model
lm2_one <- glm(PoorHealth ~  headstart, data=dj, family = binomial)

lm2_few <- glm(PoorHealth ~  headstart+Hispanic+Black+Male+logBW, data=dj, family = binomial)

lm2_medium <- glm(PoorHealth ~  headstart+Hispanic+Black+headstart*Hispanic+headstart*Black+Male+logBW, data=dj, family = binomial)

stargazer::stargazer(lm2_one, lm2_few, lm2_medium, apply.coef=exp, type="text", title="Odds ratio of Self reported poor health on Headstart participation")

#######################################################################################

dk <- df[complete.cases(df$Idle),]

# baseline medium model
lm3_one <- glm(Idle ~  headstart, data=dk, family = binomial)

lm3_few <- glm(Idle ~  headstart+Hispanic+Black+Male+logBW, data=dk, family = binomial)

lm3_medium <- glm(Idle ~  headstart+Hispanic+Black+headstart*Hispanic+headstart*Black+Male+hsgrad, data=dk, family = binomial)

stargazer::stargazer(lm3_one, lm3_few, lm3_medium, apply.coef=exp, type="text", title="Odds ratio of being out of both college and labor force on Headstart participation")

#######################################################################################