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

compscorefunc <- function(year,column1,column2,age1){
  df_comscorefunc <- subset(df,calc_age(year,df$DOB_Yr_Child)==age1)
  df_comscorefunc_result <- cbind(df_comscorefunc[,column1],df_comscorefunc[,column2])
}

compscoreage6 <- rbind(
  compscorefunc(1986,"ChildID","Test_Pct86","6"),
  compscorefunc(1988,"ChildID","Test_Pct88","6"),
  compscorefunc(1990,"ChildID","Test_Pct90","6"),
  compscorefunc(1992,"ChildID","Test_Pct92","6"),
  compscorefunc(1994,"ChildID","Test_Pct94","6"),
  compscorefunc(1996,"ChildID","Test_Pct96","6"),
  compscorefunc(1998,"ChildID","Test_Pct98","6"),
  compscorefunc(2002,"ChildID","Test_Pct100","6"),
  compscorefunc(2002,"ChildID","Test_Pct102","6"),
  compscorefunc(2004,"ChildID","Test_Pct104","6")
) %>% as.data.frame() %>% unique()

names(compscoreage6) <- c("ChildID","compscoreage6")

compscoreage11 <- rbind(
  compscorefunc(1986,"ChildID","Test_Pct86","11"),
  compscorefunc(1988,"ChildID","Test_Pct88","11"),
  compscorefunc(1990,"ChildID","Test_Pct90","11"),
  compscorefunc(1992,"ChildID","Test_Pct92","11"),
  compscorefunc(1994,"ChildID","Test_Pct94","11"),
  compscorefunc(1996,"ChildID","Test_Pct96","11"),
  compscorefunc(1998,"ChildID","Test_Pct98","11"),
  compscorefunc(2002,"ChildID","Test_Pct100","11"),
  compscorefunc(2002,"ChildID","Test_Pct102","11"),
  compscorefunc(2004,"ChildID","Test_Pct104","11")
) %>% as.data.frame() %>% unique()

names(compscoreage11) <- c("ChildID","compscoreage11")

compscoreage14 <- rbind(
  compscorefunc(1986,"ChildID","Test_Pct86","14"),
  compscorefunc(1988,"ChildID","Test_Pct88","14"),
  compscorefunc(1990,"ChildID","Test_Pct90","14"),
  compscorefunc(1992,"ChildID","Test_Pct92","14"),
  compscorefunc(1994,"ChildID","Test_Pct94","14"),
  compscorefunc(1996,"ChildID","Test_Pct96","14"),
  compscorefunc(1998,"ChildID","Test_Pct98","14"),
  compscorefunc(2002,"ChildID","Test_Pct100","14"),
  compscorefunc(2002,"ChildID","Test_Pct102","14"),
  compscorefunc(2004,"ChildID","Test_Pct104","14")
) %>% as.data.frame() %>% unique()

names(compscoreage14) <- c("ChildID","compscoreage14")

compscoreage18 <- rbind(
  compscorefunc(1986,"ChildID","Test_Pct86","18"),
  compscorefunc(1988,"ChildID","Test_Pct88","18"),
  compscorefunc(1990,"ChildID","Test_Pct90","18"),
  compscorefunc(1992,"ChildID","Test_Pct92","18"),
  compscorefunc(1994,"ChildID","Test_Pct94","18"),
  compscorefunc(1996,"ChildID","Test_Pct96","18"),
  compscorefunc(1998,"ChildID","Test_Pct98","18"),
  compscorefunc(2002,"ChildID","Test_Pct100","18"),
  compscorefunc(2002,"ChildID","Test_Pct102","18"),
  compscorefunc(2004,"ChildID","Test_Pct104","18")
) %>% as.data.frame() %>% unique()

names(compscoreage18) <- c("ChildID","compscoreage18")

test <- merge(compscoreage6,unique(compscoreage11),by="ChildID")

df2 <- merge(compscoreage6,compscoreage11,by="ChildID",all=TRUE)
df2 <- merge(df2,compscoreage14,by="ChildID",all=TRUE)
df2 <- merge(df2,compscoreage18,by="ChildID",all=TRUE)
df2 <- unique(df2)

# david's contribution

#sibdiff <- select((df, ifelse(df$MotherID ))) # Not working yet

# tessie's contribution

# selecting the final variables for the final dataset

write.csv(finaldataset, file = "../data/finaldataset.csv",row.names=FALSE)
