#final dataset processing

library(tidyverse)
library(readstata13)
library(matrixStats)

df <- read.dta13("../data/deming_final_data.dta")
finaldataset <- c()

# from the original datasets
finaldataset <- df[,c("ChildID","MotherID","Hispanic","Black","Male","FirstBorn","LogInc_0to3",
                      "MothED","Father_HH_0to3","PPVTat3","logBW","Repeat","HSGrad","HSGrad_GED",
                      "someCollAtt","Idle","PoorHealth","Age_Moth_Birth","Age_1stHS88","Age_1stHS90",
                      "Age_1stHS92","Age_1stHS94","Age_1stHS96","Age_1stHS98","Age_1stHS100","Age_1stHS102",
                      "Age_1stHS104","BirthWeight"
                      )]

# frank's contribution

finaldataset$headstart <- ifelse(
  rowMaxs(
    as.matrix(
      select(df,Ever_HS88:Ever_HS104)
    )
    ,na.rm = TRUE)
  >0,1,0)

finaldataset$mentaldisability <- ifelse(
  rowMaxs(
    as.matrix(
      select(df,Retard86:Retard100)
    )
    ,na.rm = TRUE)
  >0,1,0)

finaldataset$somecollege <- ifelse(
  rowMaxs(
    as.matrix(
      select(df,HighGrade_Compl94:HighGrade_Compl104)
    )
    ,na.rm = TRUE)
  >=12,1,0)

finaldataset$hsgrad <- ifelse( 
  rowMaxs(
    as.matrix(
      select(df,HighGrade_Compl94:HighGrade_Compl104)
    )
    ,na.rm = TRUE)
  >=12,1,0)

finaldataset$learndisability <- ifelse( 
  rowMaxs(
    as.matrix(
      select(df,LD86:LD94)
    )
    ,na.rm = TRUE)
  >0,1,0)


finaldataset$repeatgrade <- ifelse( 
  rowMaxs(
    as.matrix(
      select(df,Repeat88:Repeat104)
    )
    ,na.rm = TRUE)
  >0,1,0)

#calculate age of child at year

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

compscorefunc <- function(age1){
  df_comscorefunc1986 <- subset(df,calc_age(1986,df$DOB_Yr_Child)==age1)
  df_comscorefunc1986_result <- cbind(df_comscorefunc1986[,"ChildID"],df_comscorefunc1986[,"Test_Pct86"])
  df_comscorefunc1988 <- subset(df,calc_age(1988,df$DOB_Yr_Child)==age1)
  df_comscorefunc1988_result <- cbind(df_comscorefunc1988[,"ChildID"],df_comscorefunc1988[,"Test_Pct88"])
  df_comscorefunc1990 <- subset(df,calc_age(1990,df$DOB_Yr_Child)==age1)
  df_comscorefunc1990_result <- cbind(df_comscorefunc1990[,"ChildID"],df_comscorefunc1990[,"Test_Pct90"])
  df_comscorefunc1992 <- subset(df,calc_age(1992,df$DOB_Yr_Child)==age1)
  df_comscorefunc1992_result <- cbind(df_comscorefunc1992[,"ChildID"],df_comscorefunc1992[,"Test_Pct92"])
  df_comscorefunc1994 <- subset(df,calc_age(1994,df$DOB_Yr_Child)==age1)
  df_comscorefunc1994_result <- cbind(df_comscorefunc1994[,"ChildID"],df_comscorefunc1994[,"Test_Pct94"])
  df_comscorefunc1996 <- subset(df,calc_age(1996,df$DOB_Yr_Child)==age1)
  df_comscorefunc1996_result <- cbind(df_comscorefunc1996[,"ChildID"],df_comscorefunc1996[,"Test_Pct96"])
  df_comscorefunc1998 <- subset(df,calc_age(1998,df$DOB_Yr_Child)==age1)
  df_comscorefunc1998_result <- cbind(df_comscorefunc1998[,"ChildID"],df_comscorefunc1998[,"Test_Pct98"])
  df_comscorefunc2000 <- subset(df,calc_age(2000,df$DOB_Yr_Child)==age1)
  df_comscorefunc2000_result <- cbind(df_comscorefunc2000[,"ChildID"],df_comscorefunc2000[,"Test_Pct100"])
  df_comscorefunc2002 <- subset(df,calc_age(2002,df$DOB_Yr_Child)==age1)
  df_comscorefunc2002_result <- cbind(df_comscorefunc2002[,"ChildID"],df_comscorefunc2002[,"Test_Pct102"])
  df_comscorefunc2004 <- subset(df,calc_age(2004,df$DOB_Yr_Child)==age1)
  df_comscorefunc2004_result <- cbind(df_comscorefunc2004[,"ChildID"],df_comscorefunc2004[,"Test_Pct104"])
  rbind(df_comscorefunc1986_result,df_comscorefunc1988_result,df_comscorefunc1990_result,df_comscorefunc1992_result,
        df_comscorefunc1994_result,df_comscorefunc1996_result,df_comscorefunc1998_result,df_comscorefunc2000_result,
        df_comscorefunc2002_result,df_comscorefunc2004_result)
}

compscoreage6 <- compscorefunc("6")
colnames(compscoreage6) <- c("ChildID","compscoreage6")
compscoreage7 <- compscorefunc("7")
colnames(compscoreage7) <- c("ChildID","compscoreage7")
compscoreage8 <- compscorefunc("8")
colnames(compscoreage8) <- c("ChildID","compscoreage8")
compscoreage9 <- compscorefunc("9")
colnames(compscoreage9) <- c("ChildID","compscoreage9")
compscoreage10 <- compscorefunc("10")
colnames(compscoreage10) <- c("ChildID","compscoreage10")
compscoreage11 <- compscorefunc("11")
colnames(compscoreage11) <- c("ChildID","compscoreage11")
compscoreage12 <- compscorefunc("12")
colnames(compscoreage12) <- c("ChildID","compscoreage12")
compscoreage13 <- compscorefunc("13")
colnames(compscoreage13) <- c("ChildID","compscoreage13")
compscoreage14 <- compscorefunc("14")
colnames(compscoreage14) <- c("ChildID","compscoreage14")
compscoreage15 <- compscorefunc("15")
colnames(compscoreage15) <- c("ChildID","compscoreage15")
compscoreage16 <- compscorefunc("16")
colnames(compscoreage16) <- c("ChildID","compscoreage16")
compscoreage17 <- compscorefunc("17")
colnames(compscoreage17) <- c("ChildID","compscoreage17")
compscoreage18 <- compscorefunc("18")
colnames(compscoreage18) <- c("ChildID","compscoreage18")

finaldataset <- merge(finaldataset,compscoreage6,by="ChildID",all=TRUE)
finaldataset <- merge(finaldataset,compscoreage7,by="ChildID",all=TRUE)
finaldataset <- merge(finaldataset,compscoreage8,by="ChildID",all=TRUE)
finaldataset <- merge(finaldataset,compscoreage9,by="ChildID",all=TRUE)
finaldataset <- merge(finaldataset,compscoreage10,by="ChildID",all=TRUE)
finaldataset <- merge(finaldataset,compscoreage11,by="ChildID",all=TRUE)
finaldataset <- merge(finaldataset,compscoreage12,by="ChildID",all=TRUE)
finaldataset <- merge(finaldataset,compscoreage13,by="ChildID",all=TRUE)
finaldataset <- merge(finaldataset,compscoreage14,by="ChildID",all=TRUE)
finaldataset <- merge(finaldataset,compscoreage15,by="ChildID",all=TRUE)
finaldataset <- merge(finaldataset,compscoreage16,by="ChildID",all=TRUE)
finaldataset <- merge(finaldataset,compscoreage17,by="ChildID",all=TRUE)
finaldataset <- merge(finaldataset,compscoreage18,by="ChildID",all=TRUE)

# selecting the final variables for the final dataset

write.csv(finaldataset, file = "../data/finaldataset.csv",row.names=FALSE)
