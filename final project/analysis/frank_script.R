library(tidyverse)
library(foreign)
library(matrixStats)

df <- read.dta("../data/data_Deming_2008_0217.dta")

varlist <- c("BirthWeight")

mentaldisability <- ifelse(
  rowMaxs(
    as.matrix(
      select(df,Retard86:Retard100)
      )
    ,na.rm = TRUE)
  >0,1,0)

somecollege <- ifelse(
  rowMaxs(
    as.matrix(
      select(df,HighGrade_Compl94:HighGrade_Compl104)
      )
    ,na.rm = TRUE)
  >=12,1,0)

hsgrad <- ifelse( 
  rowMaxs(
    as.matrix(
      select(df,HighGrade_Compl94:HighGrade_Compl104)
    )
    ,na.rm = TRUE)
  >=12,1,0)

learndisability <- ifelse( 
  rowMaxs(
    as.matrix(
      select(df,LD86:LD94)
      )
    ,na.rm = TRUE)
  >0,1,0)


repeatgrade <- ifelse( 
  rowMaxs(
    as.matrix(
      select(df,Repeat88:Repeat104)
    )
    ,na.rm = TRUE)
  >0,1,0)

calc_age <- function(currentyear,childyear){
  currentyear-childyear
}

df$childage1986 <- calc_age(1986,df$DOB_Yr_Child)
df$childage1988 <- calc_age(1988,df$DOB_Yr_Child)
df$childage1990 <- calc_age(1990,df$DOB_Yr_Child)
df$childage1992 <- calc_age(1992,df$DOB_Yr_Child)
df$childage1994 <- calc_age(1994,df$DOB_Yr_Child)
df$childage1996 <- calc_age(1996,df$DOB_Yr_Child)
df$childage1998 <- calc_age(1998,df$DOB_Yr_Child)
df$childage2000 <- calc_age(2000,df$DOB_Yr_Child)
df$childage2002 <- calc_age(2002,df$DOB_Yr_Child)
df$childage2004 <- calc_age(2004,df$DOB_Yr_Child)

year <- 1986

compscorefunc <- function(year,column){
  df_comscorefunc <- subset(df,calc_age(year,df$DOB_Yr_Child)==5 | calc_age(year,df$DOB_Yr_Child)==6)
  df_comscorefunc[,ChildID]
  df_comscorefunc[,column]
}
compmath1986 <- compscorefunc(1986,"PIATMT_Raw86")
compreading1986 <- compscorefunc(1986,"PIATRC_Raw86")

compmath.5to6 <- if(df$childage1986==5){
  df$PIATMT_Raw86
} else if(df$childage1988==5|6){
  df$PIATMT_Raw88
}
