##############Functions to Use
setwd("C:/Users/Maxim/Documents/University/MBA/FXRetail")
source("PPP-Functions.R")
source("C:/R/MAXIM/Tools/FunctionList.R")
source("PPP-LogitLag-Functions.R")

#######################################################

library(foreign)
library(readstata13)
library(MASS)
library(Hmisc)
library(reshape)
library(reshape2)
library(rpart)
library(caret)
library(glmnet)
library(randomForest)
library(e1071)
library(kernlab)
library(neuralnet)
library(mgcv)
library(stringr)
library(timeDate)
library(plyr)

all.data <- Read.All.Data() #get all data
#####Specify Country to Use

all.country.list <- c("CHI","BRA","AUS","UK","SOA","GER","JAP")


icount.list <- c("CHI","BRA","AUS","UK","SOA","GER","JAP")

trade.weights <- Get.Trade.Weights(all.data, icount="USA",source.to.use = "USCensus",method.to.use=720) #method.to.use="natural"
head(trade.weights)

