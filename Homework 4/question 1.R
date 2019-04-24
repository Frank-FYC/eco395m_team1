library(ISLR)
library(tidyverse)

wine <- read.csv("wine.csv", header = TRUE)

# PCA analysis####
pr_wine = prcomp(wine, scale=TRUE)
 
 
#Cluster Analysis####
library(ggplot2)
library(LICORS)  # for kmeans++
library(foreach)
library(mosaic)
