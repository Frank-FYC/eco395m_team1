final_data <- read_dta("Documents/GitHub/eco395m_team_awesome/final project/data/final_data.dta") ## Demings final dataset

sibdiff <- select((final_data, ifelse(final_data$MotherID ))) # Not working yet
momID <- select(data_Deming_2008_0217, MotherID)
Race <- select(final_data, Hispanic, Black)
final_data$Hispanic <- ifelse(final_data$Race_Child== 1,1,0)
final_data$Black <-ifelse(final_data$Race_Child == 2,1, 0)
Male <-subset(final_data, final_data$Sex_Child == 1)



Deming2008 <- read_dta("Documents/GitHub/eco395m_team_awesome/final project/data/data_Deming_2008_0217.dta") ## Raw NLSY data w/ renamed variables by Deming

nlsy_deming <- read_dta("Documents/GitHub/eco395m_team_awesome/final project/data/nlsy_deming.dta") ## Variable dataset of included regression for Deming paper

Appendix_2008<- read_dta("Documents/GitHub/eco395m_team_awesome/final project/data/Appendix_2008_0217.dta") ## Appendix of full data

final_data <- read_dta("Documents/GitHub/eco395m_team_awesome/final project/data/final_data.dta") ## Demings final dataset

View(Deming_2008)
View(nsly_deming)
View(Appendix_2008)
View(final_data)

## Creating datafram for Sibling diff. variable (difference 
sibdiff <- select((final_data, ifelse(final_data$MotherID)))

## Creating dataframe for Race split into hispanic, black, white
Race <- select(final_data, Hispanic, Black, White)

## Creating dataframe for MotherID (will need to merge with additional column for further analysis)
momID <- select(data_Deming_2008_0217, MotherID)

## For turning factor variable into categorical race variable into 3 different columns
data_Deming_2008_0217$Hispanic <- ifelse(data_Deming_2008_0217$Race_Child== 1,"Hispanic",0)
data_Deming_2008_0217$Black <-ifelse(data_Deming_2008_0217$Race_Child == 2,"Black", 0)
data_Deming_2008_0217$White<-ifelse(data_Deming_2008_0217$Race_Child ==3,"White", 0)

df$Race = factor(data_Deming_2008_0217$Race_Child, levels=1:4, 
                 labels=c("Hispanic","Black","White", "Other")
                 ## Creating Male variable
                 
                 Male <-select(data_Deming_2008_0217, Sex_Child == 1) ## Not sure why this does not work                          
                 