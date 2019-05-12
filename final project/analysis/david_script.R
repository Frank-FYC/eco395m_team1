final_data <- read_dta("Documents/GitHub/eco395m_team_awesome/final project/data/final_data.dta") ## Demings final dataset

sibdiff <- select((final_data, ifelse(final_data$MotherID ))) # Not working yet
momID <- select(data_Deming_2008_0217, MotherID)
Race <- select(final_data, Hispanic, Black)
final_data$Hispanic <- ifelse(final_data$Race_Child== 1,1,0)
final_data$Black <-ifelse(final_data$Race_Child == 2,1, 0)
Male <-subset(final_data, final_data$Sex_Child == 1)

