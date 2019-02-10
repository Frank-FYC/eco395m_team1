library(tidyverse)
library(reshape2)
library(expss)

df <- read.csv('greenbuildings.csv')

# Replicating the Analyst's results
df_rm10 <- df[df$leasing_rate > 10,] #this removes rows where leasing_rate is < 10
df_rm10_green <- df[df$green_rating == 1, ]
df_rm10_ngreen <- df[df$green_rating != 1, ]

rent <- median (df_rm10$Rent) #rent of all buildings
g_rent <- median (df_rm10_green$Rent) #rent of >10 occupancy green buildings
ng_rent <- median (df_rm10_ngreen$Rent) #rent of >10 occupancy non-green buildings
d_rent <- g_rent-ng_rent #difference in rent from green and non-green

n <- 30 #years of operation
o_rate <- 0.90 #occupancy rate
sqft <- 250000 #square footage of building
c_cost <- 100000000 #cost of building
g_cert <- 0.05 #percentage to certify green

#payoff analysis
p1 <- (c_cost*g_cert)/(d_rent*sqft) #at 100% occupancy
p2 <- (c_cost*g_cert)/(d_rent*sqft*o_rate) #at 90% occupancy

t1 <- (n-p1)*d_rent*sqft #total payoff under senario 1
t2 <- (n-p2)*d_rent*sqft*o_rate #total payoff under senario 2

full_rate <- c(100:1)/100 #a range of values from 0-100 for occupancy rates
full_payoff <- (c_cost*g_cert)/(d_rent*sqft*full_rate) #pay off times based on occupancy rates

df_plot <- data.frame(full_payoff,full_rate)

analyst_plot1 <- ggplot(df_plot)+
  geom_point(mapping = aes(x = full_payoff, y = full_rate), color='black')+
  labs(title="Payoff Analysis",x="Years to Payoff",y="Occupancy Rate")

# A rudimentary analysis of Analyst's parameters gives us a basic understanding of how long it would take to pay off the initial investment of building a green building: from a low of 7.6 years at 100% occupancy rate to over 800 years if we have a 1% occupancy rate. However, this is unrealistic, becuase if we were analyze the entire data set, we find that the rate of occupancy for green to non-green buildings are significantly different. But before we continue, let us focus on the actual building operational lifecycle of 30 years.

analyst_plot2 <-ggplot(df_plot)+
  geom_point(mapping = aes(x = full_payoff, y = full_rate), color='black')+
  labs(title="Payoff Analysis",subtitle="30 Year Operational Life",x="Years to Payoff",y="Occupancy Rate")+
  coord_cartesian(xlim=c(0, 30))+
  geom_hline(yintercept=1, linetype="dashed", color = "red")+
  geom_vline(xintercept=7.6, linetype="dashed", color = "red")+
  geom_hline(yintercept=0.25, linetype="dashed", color = "red")+
  geom_vline(xintercept=30, linetype="dashed", color = "red")
  

# Here we see a more accurate estimate of the payoff timetable for the building if we built it green. If we have 100% occupancy, we expect to pay it off within 7.6 years, however if we see 25% occupacy rates, we would break-even at the end of the building's estimated operational lifecycle, and any lower than 25% we would never break-even.

# Regardless of the analysis there are a number of points of fault within the Analyst's analysis. 

# One: by eliminating buildings where its occupancy rates are below 10%, we exclude important data that would give us insight as to how the market is doing, namely the fact that we don't expect occupancy rates to remain constant nor at max-capacity at the onsent of the building's commission. Becuase the data does not give us longitudial information regarding a given building's occupancy rate over the time of its lifetime, we do not have an idea of how a building going from non-green to green affects its rental value. 

# Two: by utilizing the median rental value of the subsets: green and non-green, we skew the data because we have more green versus non-green buildings:

point2 <- table(df$green_rating)

# Where "0" represent the number of non-green buildings and "1" represent the number of green buildings

# Three: As for the decision to utilize the median instead of the mean value of rent, a cursory assessment of different values provide different results of a green vs. non-green building.

hist(df_rm10$Rent)

# A histogram of the Analyst's data set provides a view of how the rental prices are positively skewed. There are more buildings with rental prices in the $0-50 range than >$50. Using the median value prices would have a result less than the mean value.

table <- matrix(c(
  median(df_rm10$Rent),
  mean(df_rm10$Rent)
    ),ncol=2,byrow=TRUE)

colnames(table) <- c("Median","Mean")
rownames(table) <- c("Whole Dataset")

table <- as.table(table)
table


# However, if we were to compare the price differences within the green versus non-green building subsets, we see that the effect of green buildings are less than anticipated. Intead of a $2.6 difference using median-values, it will only be $1.7. This will greatly extend our anticipated break-even point.

table <- matrix(c(
  median(df_rm10$Rent),
  mean(df_rm10$Rent),
  median(df_rm10_green$Rent),
  mean(df_rm10_green$Rent),
  median(df_rm10_ngreen$Rent),
  mean(df_rm10_ngreen$Rent)
),ncol=2,byrow=TRUE)

colnames(table) <- c("Median","Mean")
rownames(table) <- c("Whole Dataset","Green","Non-Green")

table <- as.table(table)
table

## If we were to conduct the analysis with the full set of data and the mean-value, we see a different picture. 

df <- read.csv('greenbuildings.csv')
df_green <- df[df$green_rating == 1, ]
df_ngreen <- df[df$green_rating != 1, ]

rent <- mean (df$Rent) #rent of all buildings
g_rent <- mean (df_green$Rent) #rent of occupancy green buildings
ng_rent <- mean (df_ngreen$Rent) #rent of occupancy non-green buildings
d_rent <- g_rent-ng_rent #difference in rent from green and non-green

n <- 30 #years of operation
o_rate <- 0.90 #occupancy rate
sqft <- 250000 #square footage of building
c_cost <- 100000000 #cost of building
g_cert <- 0.05 #percentage to certify green

#payoff analysis
p1 <- (c_cost*g_cert)/(d_rent*sqft) #at 100% occupancy
p2 <- (c_cost*g_cert)/(d_rent*sqft*o_rate) #at 90% occupancy

t1 <- (n-p1)*d_rent*sqft #total payoff under senario 1
t2 <- (n-p2)*d_rent*sqft*o_rate #total payoff under senario 2

full_rate <- c(100:1)/100 #a range of values from 0-100 for occupancy rates
full_payoff <- (c_cost*g_cert)/(d_rent*sqft*full_rate) #pay off times based on occupancy rates

df_plot <- data.frame(full_payoff,full_rate)

# Begin Analysis of the better data

analyst_plot2 <-ggplot(df_plot)+
  geom_point(mapping = aes(x = full_payoff, y = full_rate), color='black')+
  labs(title="Payoff Analysis",subtitle="30 Year Operational Life",x="Years to Payoff",y="Occupancy Rate")+
  coord_cartesian(xlim=c(0, 30))+
  geom_hline(yintercept=1, linetype="dashed", color = "red")+
  geom_vline(xintercept=p1, linetype="dashed", color = "red")+
  geom_hline(yintercept=0.37, linetype="dashed", color = "red")+
  geom_vline(xintercept=30, linetype="dashed", color = "red")


# Here we see a more accurate estimate of the payoff timetable for the building if we built it green using mean-values. If we have 100% occupancy, we expect to pay it off within 11.43 years, however if we see 37% occupacy rates, we would break-even at the end of the building's estimated operational lifecycle, and any lower than 37% we would never break-even.

# In conclusion, we see that a more accurate assessment of the viability of building green is represented by using the full data set with mean-values instead of a truncated data set with median-values.