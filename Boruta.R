rm(list=ls())
setwd("")
library(Boruta) 
library(glmnet)  
library(tidyverse)
library(psych)  
data <- read.table("fs.csv", row.names = 1, header=T, sep=",",check.names = FALSE)
set.seed(123456)  #
y <- data %>% dplyr::select(RR) %>% 
  scale(center = TRUE, scale = FALSE) %>% as.matrix()
#########################
X <- data %>% dplyr::select(-RR) %>% as.matrix()
fit <- Boruta(y ~ X, data = data,pValue=0.01, mcAdj=T,  maxRuns=500, getImp =getImpRfZ)
print(fit)                       




