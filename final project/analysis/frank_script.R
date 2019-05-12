library(tidyverse)
library(readstata13)
library(matrixStats)

df <- read.dta13("../data/final_data.dta")

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

df2 <- as.data.frame(subset(df,calc_age(1986,df$DOB_Yr_Child)>=7 & calc_age(1986,df$DOB_Yr_Child)<=10))
df2_result <- cbind(df2[,"ChildID"],df2[,"PIATMT_Raw86"])

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
)

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
)

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
)

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
)


#math and reading scores of child at year 6

math5to6_1986 <- df[df$childage1986>=5 & df$childage1986<=6,c("ChildID","PIATMT_Raw86")]
names(math5to6_1986)[2] <- "mathscore5to6"
math5to6_1988 <- df[df$childage1988>=5 & df$childage1988<=6,c("ChildID","PIATMT_Raw88")]
names(math5to6_1988)[2] <- "mathscore5to6"
math5to6_1990 <- df[df$childage1990>=5 & df$childage1990<=6,c("ChildID","PIATMT_Raw90")]
names(math5to6_1990)[2] <- "mathscore5to6"
math5to6_1992 <- df[df$childage1992>=5 & df$childage1992<=6,c("ChildID","PIATMT_Raw92")]
names(math5to6_1992)[2] <- "mathscore5to6"
math5to6_1994 <- df[df$childage1994>=5 & df$childage1994<=6,c("ChildID","PIATMT_Raw94")]
names(math5to6_1994)[2] <- "mathscore5to6"
math5to6_1996 <- df[df$childage1996>=5 & df$childage1996<=6,c("ChildID","PIATMT_Raw96")]
names(math5to6_1996)[2] <- "mathscore5to6"
math5to6_1998 <- df[df$childage1998>=5 & df$childage1998<=6,c("ChildID","PIATMT_Raw98")]
names(math5to6_1998)[2] <- "mathscore5to6"
math5to6_2000 <- df[df$childage2000>=5 & df$childage2000<=6,c("ChildID","PIATMT_Raw100")]
names(math5to6_2000)[2] <- "mathscore5to6"
math5to6_2002 <- df[df$childage2002>=5 & df$childage2002<=6,c("ChildID","PIATMT_Raw102")]
names(math5to6_2002)[2] <- "mathscore5to6"
math5to6_2004 <- df[df$childage2004>=5 & df$childage2004<=6,c("ChildID","PIATMT_Raw104")]
names(math5to6_2004)[2] <- "mathscore5to6"

math5to6_combined <- rbind(math5to6_1986,math5to6_1988,math5to6_1990,math5to6_1992,math5to6_1994,
      math5to6_1996,math5to6_1998,math5to6_2000,math5to6_2002,math5to6_2004)
  
reading5to6_1986 <- df[df$childage1986>=5 & df$childage1986<=6,c("ChildID","PIATRC_Raw86")]
names(reading5to6_1986)[2] <- "readingscore5to6"
reading5to6_1988 <- df[df$childage1988>=5 & df$childage1988<=6,c("ChildID","PIATRC_Raw88")]
names(reading5to6_1988)[2] <- "readingscore5to6"
reading5to6_1990 <- df[df$childage1990>=5 & df$childage1990<=6,c("ChildID","PIATRC_Raw90")]
names(reading5to6_1990)[2] <- "readingscore5to6"
reading5to6_1992 <- df[df$childage1992>=5 & df$childage1992<=6,c("ChildID","PIATRC_Raw92")]
names(reading5to6_1992)[2] <- "readingscore5to6"
reading5to6_1994 <- df[df$childage1994>=5 & df$childage1994<=6,c("ChildID","PIATRC_Raw94")]
names(reading5to6_1994)[2] <- "readingscore5to6"
reading5to6_1996 <- df[df$childage1996>=5 & df$childage1996<=6,c("ChildID","PIATRC_Raw96")]
names(reading5to6_1996)[2] <- "readingscore5to6"
reading5to6_1998 <- df[df$childage1998>=5 & df$childage1998<=6,c("ChildID","PIATRC_Raw98")]
names(reading5to6_1998)[2] <- "readingscore5to6"
reading5to6_2000 <- df[df$childage2000>=5 & df$childage2000<=6,c("ChildID","PIATRC_Raw100")]
names(reading5to6_2000)[2] <- "readingscore5to6"
reading5to6_2002 <- df[df$childage2002>=5 & df$childage2002<=6,c("ChildID","PIATRC_Raw102")]
names(reading5to6_2002)[2] <- "readingscore5to6"
reading5to6_2004 <- df[df$childage2004>=5 & df$childage2004<=6,c("ChildID","PIATRC_Raw104")]
names(reading5to6_2004)[2] <- "readingscore5to6"

reading5to6_combined <- rbind(reading5to6_1986,reading5to6_1988,reading5to6_1990,reading5to6_1992,reading5to6_1994,
                           reading5to6_1996,reading5to6_1998,reading5to6_2000,reading5to6_2002,reading5to6_2004)

#math and reading scores of child at year 7to11

math7to11_1986 <- df[df$childage1986==7,c("ChildID","PIATMT_Raw86")]
names(math7to11_1986)[2] <- "mathscore7to11"
math7to11_1988 <- df[df$childage1988==7,c("ChildID","PIATMT_Raw88")]
names(math7to11_1988)[2] <- "mathscore7to11"
math7to11_1990 <- df[df$childage1990==7,c("ChildID","PIATMT_Raw90")]
names(math7to11_1990)[2] <- "mathscore7to11"
math7to11_1992 <- df[df$childage1992==7,c("ChildID","PIATMT_Raw92")]
names(math7to11_1992)[2] <- "mathscore7to11"
math7to11_1994 <- df[df$childage1994==7,c("ChildID","PIATMT_Raw94")]
names(math7to11_1994)[2] <- "mathscore7to11"
math7to11_1996 <- df[df$childage1996==7,c("ChildID","PIATMT_Raw96")]
names(math7to11_1996)[2] <- "mathscore7to11"
math7to11_1998 <- df[df$childage1998==7,c("ChildID","PIATMT_Raw98")]
names(math7to11_1998)[2] <- "mathscore7to11"
math7to11_2000 <- df[df$childage2000==7,c("ChildID","PIATMT_Raw100")]
names(math7to11_2000)[2] <- "mathscore7to11"
math7to11_2002 <- df[df$childage2002==7,c("ChildID","PIATMT_Raw102")]
names(math7to11_2002)[2] <- "mathscore7to11"
math7to11_2004 <- df[df$childage2004==7,c("ChildID","PIATMT_Raw104")]
names(math7to11_2004)[2] <- "mathscore7to11"

math7to11_combined <- rbind(math7to11_1986,math7to11_1988,math7to11_1990,math7to11_1992,math7to11_1994,
                           math7to11_1996,math7to11_1998,math7to11_2000,math7to11_2002,math7to11_2004)

reading7to11_1986 <- df[df$childage1986>=7,c("ChildID","PIATRC_Raw86")]
names(reading7to11_1986)[2] <- "readingscore7to11"
reading7to11_1988 <- df[df$childage1988>=7 & df$childage1988<=11,c("ChildID","PIATRC_Raw88")]
names(reading7to11_1988)[2] <- "readingscore7to11"
reading7to11_1990 <- df[df$childage1990>=7 & df$childage1990<=11,c("ChildID","PIATRC_Raw90")]
names(reading7to11_1990)[2] <- "readingscore7to11"
reading7to11_1992 <- df[df$childage1992>=7 & df$childage1992<=11,c("ChildID","PIATRC_Raw92")]
names(reading7to11_1992)[2] <- "readingscore7to11"
reading7to11_1994 <- df[df$childage1994>=7 & df$childage1994<=11,c("ChildID","PIATRC_Raw94")]
names(reading7to11_1994)[2] <- "readingscore7to11"
reading7to11_1996 <- df[df$childage1996>=7 & df$childage1996<=11,c("ChildID","PIATRC_Raw96")]
names(reading7to11_1996)[2] <- "readingscore7to11"
reading7to11_1998 <- df[df$childage1998>=7 & df$childage1998<=11,c("ChildID","PIATRC_Raw98")]
names(reading7to11_1998)[2] <- "readingscore7to11"
reading7to11_2000 <- df[df$childage2000>=7 & df$childage2000<=11,c("ChildID","PIATRC_Raw100")]
names(reading7to11_2000)[2] <- "readingscore7to11"
reading7to11_2002 <- df[df$childage2002>=7 & df$childage2002<=11,c("ChildID","PIATRC_Raw102")]
names(reading7to11_2002)[2] <- "readingscore7to11"
reading7to11_2004 <- df[df$childage2004>=7 & df$childage2004<=11,c("ChildID","PIATRC_Raw104")]
names(reading7to11_2004)[2] <- "readingscore7to11"

reading7to11_combined <- rbind(reading7to11_1986,reading7to11_1988,reading7to11_1990,reading7to11_1992,reading7to11_1994,
                              reading7to11_1996,reading7to11_1998,reading7to11_2000,reading7to11_2002,reading7to11_2004)
