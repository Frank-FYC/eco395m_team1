library(tidyverse)
library(readstata13)
library(clusterSEs)
library(stargazer)
library(effects)
library(knitr)

df <- read.csv("../data/finaldataset.csv")
df2 <- read.dta13("../data/deming_final_data.dta")

# Preprocessing (some more)

df$hs.age <- rowMeans(df[,c("Age_1stHS88","Age_1stHS90","Age_1stHS92","Age_1stHS94","Age_1stHS96","Age_1stHS98",
                         "Age_1stHS100","Age_1stHS102","Age_1stHS98","Age_1stHS104")],
                   na.rm=TRUE)

calc_age <- function(currentyear,childyear){
  currentyear-childyear
}

df$childage1986 <- calc_age(1986,df2$DOB_Yr_Child)
df$childage1988 <- calc_age(1988,df2$DOB_Yr_Child)
df$childage1990 <- calc_age(1990,df2$DOB_Yr_Child)
df$childage1992 <- calc_age(1992,df2$DOB_Yr_Child)
df$childage1994 <- calc_age(1994,df2$DOB_Yr_Child)
df$childage1996 <- calc_age(1996,df2$DOB_Yr_Child)
df$childage1998 <- calc_age(1998,df2$DOB_Yr_Child)
df$childage2000 <- calc_age(2000,df2$DOB_Yr_Child)
df$childage2002 <- calc_age(2002,df2$DOB_Yr_Child)
df$childage2004 <- calc_age(2004,df2$DOB_Yr_Child)

## PIAT Participation

part.number <- rbind(
  11470-sum(is.na(df$compscoreage6)),
  11470-sum(is.na(df$compscoreage7)),
  11470-sum(is.na(df$compscoreage8)),
  11470-sum(is.na(df$compscoreage9)),
  11470-sum(is.na(df$compscoreage10)),
  11470-sum(is.na(df$compscoreage11)),
  11470-sum(is.na(df$compscoreage12)),
  11470-sum(is.na(df$compscoreage13)),
  11470-sum(is.na(df$compscoreage14)),
  11470-sum(is.na(df$compscoreage15)),
  11470-sum(is.na(df$compscoreage16)),
  11470-sum(is.na(df$compscoreage17)),
  11470-sum(is.na(df$compscoreage18))
)

## Age

part.age <- c(
  "6 Years Old",
  "7 Years Old",
  "8 Years Old",
  "9 Years Old",
  "10 Years Old",
  "11 Years Old",
  "12 Years Old",
  "13 Years Old",
  "14 Years Old",
  "15 Years Old",
  "16 Years Old",
  "17 Years Old",
  "18 Years Old"
)

## Counting by age

part_function.total <- function(age){
  part.df <<- subset(df,childage1986==age |childage1988==age |childage1990==age |childage1992==age |childage1994==age |
                       childage1996==age |childage1998==age |childage2000==age |childage2002==age |childage2004==age)
  nrow(part.df)
}

part.total <- rbind(
  part_function.total(6),
  part_function.total(7),
  part_function.total(8),
  part_function.total(9),
  part_function.total(10),
  part_function.total(11),
  part_function.total(12),
  part_function.total(13),
  part_function.total(14),
  part_function.total(15),
  part_function.total(16),
  part_function.total(17),
  part_function.total(18)
)

## Counting by age, and then head start participation

part_function <- function(age){
  part.df <<- subset(df,childage1986==age |childage1988==age |childage1990==age |childage1992==age |childage1994==age |
                      childage1996==age |childage1998==age |childage2000==age |childage2002==age |childage2004==age)$headstart
  table(part.df)
}

part.headstart <- rbind(
  part_function(6),
  part_function(7),
  part_function(8),
  part_function(9),
  part_function(10),
  part_function(11),
  part_function(12),
  part_function(13),
  part_function(14),
  part_function(15),
  part_function(16),
  part_function(17),
  part_function(18)
)

part.grade <- c(
  "Kindergarden",
  "1st Grade",
  "2nd Grade",
  "3rd Grade",
  "4th Grade",
  "5th Grade",
  "6th Grade",
  "7th Grade",
  "8th Grade",
  "9th Grade",
  "10th Grade",
  "11th Grade",
  "12th Grade"
)

part.school <- c(
  "Elementary School",
  "Elementary School",
  "Elementary School",
  "Elementary School",
  "Elementary School",
  "Elementary School",
  "Middle School",
  "Middle School",
  "Middle School",
  "High School",
  "High School",
  "High School",
  "High School"
)

## Creating Tables
  
part.table <- as.data.frame(cbind(part.age,part.grade,part.school,part.total,part.number,part.headstart))

names(part.table) <- c("Age","Grade","School Type","Total","PIAT Participants","Non-HeadStart","HeadStart")

table_part <- kable(part.table,caption="Children Summary")

# Linear Models as the Baseline

## Primary to elementary

df$compscore6to11 <- rowMeans(df[,c("compscoreage6","compscoreage7","compscoreage8","compscoreage9","compscoreage10","compscoreage11")],
                              na.rm=TRUE)

lm6to11 <- glm(
  data=df,
  formula=compscore6to11~
    Hispanic+Black+Male+
    FirstBorn+LogInc_0to3+MothED+Father_HH_0to3+PPVTat3+
    logBW+Repeat+
    PoorHealth+Age_Moth_Birth+
    headstart)

## Middle School

df$compscore12to14 <- rowMeans(df[,c("compscoreage12","compscoreage13","compscoreage14")],na.rm=TRUE)

lm12to14 <- glm(
  data=df,
  formula=compscore12to14~
    Hispanic+Black+Male+
    FirstBorn+LogInc_0to3+MothED+Father_HH_0to3+PPVTat3+
    logBW+Repeat+
    PoorHealth+Age_Moth_Birth+
    headstart)


## High School

df$compscore15to18 <- rowMeans(df[,c("compscoreage15","compscoreage16","compscoreage17","compscoreage18")],na.rm=TRUE)

#### Note that Father_HH_0to3 and PPVTat3 were removed becuase there were no values found at this level
lm15to18 <- glm(
  data=df,
  formula=compscore15to18~
    Hispanic+Black+Male+
    FirstBorn+LogInc_0to3+MothED+
    logBW+Repeat+
    PoorHealth+Age_Moth_Birth+
    headstart)

# Table for linear models

table_lm <- stargazer::stargazer(lm6to11,lm12to14,lm15to18, type = 'text',title="Composition Score based on Age",align=TRUE)


