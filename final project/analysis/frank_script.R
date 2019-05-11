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

Age_1stHS88 
Age_1stHS90 
Age_1stHS92 
Age_1stHS94 
Age_1stHS96 
Age_1stHS98 
Age_1stHS100 
Age_1stHS102 
Age_1stHS104

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

compscorefunc <- function(year,column1,column2,age1,age2){
  df_comscorefunc <<- as.data.frame(subset(df,calc_age(year,df$DOB_Yr_Child)>=age1 & calc_age(year,df$DOB_Yr_Child)<=age2))
  df_comscorefunc_result <<- cbind(df_comscorefunc[,column1],df_comscorefunc[,column2])
}

df2 <- as.data.frame(subset(df,calc_age(1986,df$DOB_Yr_Child)>=7 & calc_age(1986,df$DOB_Yr_Child)<=10))
df2_result <- cbind(df2[,"ChildID"],df2[,"PIATMT_Raw86"])

compmath_5to6_1986 <- compscorefunc(1986,"ChildID","PIATMT_Raw86","5","6")
compmath_5to6_1988 <- compscorefunc(1988,"ChildID","PIATMT_Raw88","5","6")
compmath_5to6_1990 <- compscorefunc(1990,"ChildID","PIATMT_Raw90","5","6")
compmath_5to6_1992 <- compscorefunc(1992,"ChildID","PIATMT_Raw92","5","6")
compmath_5to6_1994 <- compscorefunc(1994,"ChildID","PIATMT_Raw94","5","6")
compmath_5to6_1996 <- compscorefunc(1996,"ChildID","PIATMT_Raw96","5","6")
compmath_5to6_1998 <- compscorefunc(1998,"ChildID","PIATMT_Raw98","5","6")
compmath_5to6_2000 <- compscorefunc(2000,"ChildID","PIATMT_Raw100","5","6")
compmath_5to6_2002 <- compscorefunc(2002,"ChildID","PIATMT_Raw102","5","6")
compmath_5to6_2004 <- compscorefunc(2004,"ChildID","PIATMT_Raw104","5","6")

compreading_5to6_1986 <- compscorefunc(1986,"ChildID","PIATRC_Raw86","5","6")
compreading_5to6_1988 <- compscorefunc(1988,"ChildID","PIATRC_Raw88","5","6")
compreading_5to6_1990 <- compscorefunc(1990,"ChildID","PIATRC_Raw90","5","6")
compreading_5to6_1992 <- compscorefunc(1992,"ChildID","PIATRC_Raw92","5","6")
compreading_5to6_1994 <- compscorefunc(1994,"ChildID","PIATRC_Raw94","5","6")
compreading_5to6_1996 <- compscorefunc(1996,"ChildID","PIATRC_Raw96","5","6")
compreading_5to6_1998 <- compscorefunc(1998,"ChildID","PIATRC_Raw98","5","6")
compreading_5to6_2000 <- compscorefunc(2000,"ChildID","PIATRC_Raw100","5","6")
compreading_5to6_2002 <- compscorefunc(2002,"ChildID","PIATRC_Raw102","5","6")
compreading_5to6_2004 <- compscorefunc(2004,"ChildID","PIATRC_Raw104","5","6")



compmath_7to10_1986 <- compscorefunc(1986,"ChildID","PIATMT_Raw86","7","10")
compmath_7to10_1988 <- compscorefunc(1988,"ChildID","PIATMT_Raw88","7","10")
compmath_7to10_1990 <- compscorefunc(1990,"ChildID","PIATMT_Raw90","7","10")
compmath_7to10_1992 <- compscorefunc(1992,"ChildID","PIATMT_Raw92","7","10")
compmath_7to10_1994 <- compscorefunc(1994,"ChildID","PIATMT_Raw94","7","10")
compmath_7to10_1996 <- compscorefunc(1996,"ChildID","PIATMT_Raw96","7","10")
compmath_7to10_1998 <- compscorefunc(1998,"ChildID","PIATMT_Raw98","7","10")
compmath_7to10_2000 <- compscorefunc(2000,"ChildID","PIATMT_Raw100","7","10")
compmath_7to10_2002 <- compscorefunc(2002,"ChildID","PIATMT_Raw102","7","10")
compmath_7to10_2004 <- compscorefunc(2004,"ChildID","PIATMT_Raw104","7","10")

compreading_7to10_1986 <- compscorefunc(1986,"ChildID","PIATRC_Raw86","7","10")
compreading_7to10_1988 <- compscorefunc(1988,"ChildID","PIATRC_Raw88","7","10")
compreading_7to10_1990 <- compscorefunc(1990,"ChildID","PIATRC_Raw90","7","10")
compreading_7to10_1992 <- compscorefunc(1992,"ChildID","PIATRC_Raw92","7","10")
compreading_7to10_1994 <- compscorefunc(1994,"ChildID","PIATRC_Raw94","7","10")
compreading_7to10_1996 <- compscorefunc(1996,"ChildID","PIATRC_Raw96","7","10")
compreading_7to10_1998 <- compscorefunc(1998,"ChildID","PIATRC_Raw98","7","10")
compreading_7to10_2000 <- compscorefunc(2000,"ChildID","PIATRC_Raw100","7","10")
compreading_7to10_2002 <- compscorefunc(2002,"ChildID","PIATRC_Raw102","7","10")
compreading_7to10_2004 <- compscorefunc(2004,"ChildID","PIATRC_Raw104","7","10")