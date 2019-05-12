library(tidyverse)
library(foreign)


t.df = read.dta('../data/data_Deming_2008_0217.dta')

View(t.df)

t.df$firstborn <- ifelse(t.df$BirthOrder==1, 1, 0)

lninc <- function(x){
  log(x)
}

t.df$lninc78 <- lninc(t.df$NetFamInc78)
t.df$lninc78 <- lninc(t.df$NetFamInc78)
t.df$lninc78 <- lninc(t.df$NetFamInc78)
t.df$lninc78 <- lninc(t.df$NetFamInc78)
t.df$lninc78 <- lninc(t.df$NetFamInc78)
t.df$lninc78 <- lninc(t.df$NetFamInc78)
t.df$lninc78 <- lninc(t.df$NetFamInc78)
t.df$lninc78 <- lninc(t.df$NetFamInc78)
t.df$lninc78 <- lninc(t.df$NetFamInc78)
t.df$lninc78 <- lninc(t.df$NetFamInc78)
t.df$lninc78 <- lninc(t.df$NetFamInc78)
t.df$lninc78 <- lninc(t.df$NetFamInc78)
t.df$lninc78 <- lninc(t.df$NetFamInc78)
t.df$lninc78 <- lninc(t.df$NetFamInc78)
t.df$lninc78 <- lninc(t.df$NetFamInc78)
t.df$lninc78 <- lninc(t.df$NetFamInc78)
t.df$lninc78 <- lninc(t.df$NetFamInc78)
t.df$lninc78 <- lninc(t.df$NetFamInc78)
t.df$lninc78 <- lninc(t.df$NetFamInc78)
t.df$lninc78 <- lninc(t.df$NetFamInc78)
t.df$lninc78 <- lninc(t.df$NetFamInc78)
t.df$lninc78 <- lninc(t.df$NetFamInc78)
t.df$lninc78 <- lninc(t.df$NetFamInc78)
t.df$lninc78 <- lninc(t.df$NetFamInc78)