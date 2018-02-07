## Estimating change in Retail Value based on CPI ratio of UK/US and PPP ratio of UK/US
setwd("C:/Users/Maxim/Documents/University/MBA/FXRetail")
source("C:/R/MAXIM/Tools/FunctionList.R")
source("Code//PPP-Functions.R")
source("Code//PPP-ResponseFunctions.R")

#######################################################
library(RColorBrewer)
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
library(boot)
library(fanplot)
library(plyr)
library(stringr)
library(fanplot)
library(zoo)

all.data <- Read.All.Data() #get all data
#####Specify Country to Use
icount <- "UK"

########################################
#Model Settings
months.to.analyse <- 12 #how many months lag to consider
use.trade.weights <- FALSE # TRUE - weigh PPP by the trade weights
detrend.data <- FALSE
normalize.data <- TRUE; norm.factor <- 0.1 #when renormalizing the data to be between 0 and 1, how much much additional "padding" to give on edges 
factor.to.test.list <- c("Retail.Log","Retail.vs.USA.1",paste0("Retail.Log.",seq(1,6)))
#Which combinations of AR/USDX/In-Sample Out Sample do you want to test
include.AR.list <- include.USDX.list <- in.sample.list <- c(TRUE,FALSE)
if(icount == "USA"){use.trade.weights <- TRUE} # TRUE}
################################################

country.retail.types <- Get.Country.Retail.Types(all.data,icount);
#Pick factor with Seasonal Adjustment
retail.type.to.use <- country.retail.types[str_detect(country.retail.types,"SA") | str_detect(country.retail.types,"sa")]
include.Dummy <- FALSE; #include Dummy Monthly Variables only if no Seasonal Adjustment already
if(length(retail.type.to.use)>1){retail.type.to.use <-retail.type.to.use[1]}else{include.Dummy <- TRUE; retail.type.to.use <- country.retail.types[1]}

#Get Data from Country and US
country.data <- Get.Country.Data(all.data,icount, retail.type = retail.type.to.use)
usa.country.data <- Get.Country.Data(all.data,"USA",retail.type="w.motor.sa")

##Now Get Retail Data and PPP Merged
i.retail <- as.data.frame(country.data[2]); 
usa.retail <- as.data.frame(usa.country.data[2])

head(i.retail)

##CHART ALL TRADE VOLUME
chart.all.series <- TRUE
if(chart.all.series){
  Chart.All.Series(all.data,country.data, i.retail, icount)
}
#################################

i.ppp <- as.data.frame(country.data[1])
retail.ppp <- Merge.Retail.And.EPPP(i.retail, i.ppp, icount=icount, detrend.data = detrend.data)
usa.retail.ppp <- Merge.Retail.And.EPPP(usa.retail, i.ppp, icount=icount, detrend.data= detrend.data)

cpi.all <- as.data.frame(all.data[5]); cpi.all$Date <- as.Date(cpi.all$Date)
cpi.uk <- cpi.all[cpi.all$Country == "UK" & cpi.all$Type == "CPIH.2015",]; 
cpi.uk$Diff <- c(NA,diff(log(cpi.uk$Value)))
cpi.uk <- cpi.uk[,c("Date","Value","Diff")]; colnames(cpi.uk)[2:3] <- c("CPI.UK","CPI.UK.Log")

cpi.us <- cpi.all[cpi.all$Country == "USA" & cpi.all$Type == "CPI.Urban.84",];
cpi.us$Diff <- c(NA,diff(log(cpi.us$Value)))
cpi.us <- cpi.us[,c("Date","Value","Diff")]; colnames(cpi.us)[2:3] <- c("CPI.USA","CPI.USA.Log")

#merge CPIs with USA
usa.retail.cpi <- merge(usa.retail.ppp, cpi.us, by="Date")
usa.retail.cpi <- merge(usa.retail.cpi, cpi.uk, by="Date")
head(usa.retail.cpi)

###### chart Retail Sales vs PPP
chart.retail.vs.ppp <- TRUE
if(chart.retail.vs.ppp){
  Chart.Retail.vs.PPP(i.retail, country.data, icount, retail.ppp)}

#Restrict number of dates (UK PPP not full before 2012)
if(icount=="UK"){retail.ppp <- retail.ppp[retail.ppp$Date >= as.Date("2012-01-01"),]}

####
#Change USA data to End of MOnth
library(timeDate)
if(icount != "USA"){usa.retail.cpi$Date <- as.Date(timeLastDayInMonth(usa.retail.cpi$Date))}

####Compare Lags vs USA going out 6 months out
country.plus.usa.retail.ppp <- merge(retail.ppp, usa.retail.cpi, by="Date")

reset_par()
par(mfrow=c(2,2))
plot(country.plus.usa.retail.ppp$Date,country.plus.usa.retail.ppp$Retail.x,main="UK Retail Sales",type = 'l')
plot(country.plus.usa.retail.ppp$Date,country.plus.usa.retail.ppp$Retail.y,main="US Retail Sales",type = 'l')
#CPI
plot(country.plus.usa.retail.ppp$Date,country.plus.usa.retail.ppp$CPI.USA,main="UK CPI",type = 'l')
plot(country.plus.usa.retail.ppp$Date,country.plus.usa.retail.ppp$CPI.UK,main="US CPI",type = 'l')

####CALCULATE THE DIFFERENCES

#0 Lag Diff
country.plus.usa.retail.ppp$temp <- country.plus.usa.retail.ppp[,paste0("Retail.Log.x")] - country.plus.usa.retail.ppp[,paste0("Retail.Log.y")]
colnames(country.plus.usa.retail.ppp)[ncol(country.plus.usa.retail.ppp)] <- paste0("Retail.vs.USA")
for(i.lag in 1:6){
  country.plus.usa.retail.ppp$temp <- country.plus.usa.retail.ppp[,paste0("Retail.Log.",i.lag,".x")] - country.plus.usa.retail.ppp[,paste0("Retail.Log.",i.lag,".y")]
  colnames(country.plus.usa.retail.ppp)[ncol(country.plus.usa.retail.ppp)] <- paste0("Retail.vs.USA.",i.lag)
}
colnames(country.plus.usa.retail.ppp)[which(colnames(country.plus.usa.retail.ppp)=="Retail.y")] <- "USA.Retail"
#drop irrelevant US columns
library(stringr)
paste0(colnames(country.plus.usa.retail.ppp),"-",str_detect(colnames(country.plus.usa.retail.ppp),".y"))
country.plus.usa.retail.ppp <- country.plus.usa.retail.ppp[,!str_detect(colnames(country.plus.usa.retail.ppp),".y")]
head(country.plus.usa.retail.ppp)


country.plus.usa.retail.ppp$CPI.Dif <- country.plus.usa.retail.ppp$CPI.UK.Log-country.plus.usa.retail.ppp$CPI.USA.Log


#Get Seasonal Dummies
country.plus.usa.ppp <- if(include.Dummy){create.monthly.dummies(country.plus.usa.retail.ppp)}else{country.plus.usa.retail.ppp}
colnames(country.plus.usa.ppp) <- gsub("ts.retail.","",colnames(country.plus.usa.ppp))
colnames(country.plus.usa.ppp) <- gsub(".x","",colnames(country.plus.usa.ppp))
head(country.plus.usa.ppp)

  
#### CHART TIME SERIES FOR MODEL
par(mfrow=c(2,4))
plot(country.plus.usa.ppp$Date, country.plus.usa.ppp$PPP, main="PPP",type='l')
plot(country.plus.usa.ppp$Date, country.plus.usa.ppp$PPP.Log, main="PPP.Dif",type='l')
abline(h=0,col="red")
plot(country.plus.usa.ppp$Date, country.plus.usa.ppp$Eppp, main="EPPP",type='l')
plot(country.plus.usa.ppp$Date, country.plus.usa.ppp$Eppp.Log, main="EPPP.Dif",type='l')
abline(h=0,col="red")
plot(country.plus.usa.ppp$Date, log(country.plus.usa.ppp$USA.Retail), main="USA Retail Log",type='l')
plot(country.plus.usa.ppp$Date, c(NA, diff(log(country.plus.usa.ppp$USA.Retail))), main="USA Retail Log Dif",type='l')
abline(h=0,col="red")
plot(country.plus.usa.ppp$Date, log(country.plus.usa.ppp$Retail), main="UK Retail Log",type='l')
plot(country.plus.usa.ppp$Date, country.plus.usa.ppp$Retail.Log, main="UK Retail Log Dif",type='l')
abline(h=0,col="red")

#Chart Retail Value vs CPI Value
par(mfrow=c(1,3))
plot(country.plus.usa.ppp$Date, country.plus.usa.ppp$Retail.vs.USA, main="Retail vs USA Retail",type='l')
abline(h=0,col="red")
plot(country.plus.usa.ppp$Date, country.plus.usa.ppp$CPI.Dif, main="UK CPI vs US CPI",type='l')
abline(h=0,col="red")
plot(country.plus.usa.ppp$CPI.Dif,country.plus.usa.ppp$Retail.vs.USA, main = "CPI Diff vs Retail Diff")



#####
  
base.model <- lm(Retail.vs.USA ~ CPI.Dif + PPP.Log, data = country.plus.usa.ppp)
summary(base.model)
anova(base.model)

base.ppp.model <- lm(Retail.vs.USA ~ CPI.Dif + Eppp.Log, data = country.plus.usa.ppp)
summary(base.ppp.model)
anova(base.ppp.model)






























#Get calculation of how the Eppp and Retail Sales have shifted by 1 to 12 months in time
#retail.ppp <- Factors.Calculate.Lags(retail.ppp,months.to.analyse,c("PPP.Log","Retail.Log","Usdx.Log"))
retail.ppp <- Factors.Calculate.Lags(as.data.frame(country.plus.usa.ppp),months.to.analyse,c("Retail.Log","PPP.Log","Retail.vs.USA.1","Us.Log","Eppp.Log")) #optionally include USDX log
#plot(retail.ppp$Date, retail.ppp$PPP, type="l",col="blue",main="PPP for UK", xlab = "Date",ylab="PPP value");grid(NULL,NULL,col="grey")

#Take out Dates < 2016-06 (i.e. Brexit)
take.out.brexit <- FALSE
if(take.out.brexit){
  retail.ppp <- retail.ppp[retail.ppp$Date < as.Date("2016-06-01"),]
}

#Specify Factor to Test - Retail.Log for Domestic of Retail.vs.USA.Dif
factor.to.test <- "Retail.Log"