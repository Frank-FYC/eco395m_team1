library(tidyverse)

df <- read.csv("../data/finaldataset.csv")

lm1 <- lm(compscoreage6~Hispanic+Black+Male+FirstBorn+MothED+headstart+mentaldisability,df
          )
