## Estimating change in Retail Value UK (Output Gap) vs PPP
setwd("C:/Users/Maxim/Documents/University/MBA/FXRetail")
source("C:/R/MAXIM/Tools/FunctionList.R")
source("Code//PPP-Functions.R")
source("Code//PPP-ResponseFunctions.R")

#######################################################
library(mFilter)

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

filter.lambda = 14400 ## based on Suggested Level

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


hp.filt <- hpfilter(retail.ppp$Retail, freq= filter.lambda)
hp.log.filt <- hpfilter(log(retail.ppp$Retail), freq= filter.lambda)
reset_par(); par(mfrow=c(1,2))
plot(retail.ppp$Retail,type='l', main="UK Retail Sales and Output Gap"); lines(hp.filt$trend)
plot(log(retail.ppp$Retail),type='l', main="LOG UK Retail Sales and Output Gap"); lines(hp.log.filt$trend)
retail.ppp$Retail.Gap <- hp.log.filt$cycle


###### chart Retail Sales vs PPP
chart.retail.vs.ppp <- TRUE
if(chart.retail.vs.ppp){
  Chart.Retail.vs.PPP(i.retail, country.data, icount, retail.ppp)}

#Restrict number of dates (UK PPP not full before 2012)
if(icount=="UK"){retail.ppp <- retail.ppp[retail.ppp$Date >= as.Date("2012-01-01"),]}

####
#Change USA data to End of MOnth
library(timeDate)
if(icount != "USA"){usa.retail.ppp$Date <- as.Date(timeLastDayInMonth(usa.retail.ppp$Date))}

####Compare Lags vs USA going out 6 months out
country.plus.usa.retail.ppp <- merge(retail.ppp, usa.retail.ppp, by="Date")

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


#Get Seasonal Dummies
country.plus.usa.ppp <- if(include.Dummy){create.monthly.dummies(country.plus.usa.retail.ppp)}else{country.plus.usa.retail.ppp}
colnames(country.plus.usa.ppp) <- gsub("ts.retail.","",colnames(country.plus.usa.ppp))
colnames(country.plus.usa.ppp) <- gsub(".x","",colnames(country.plus.usa.ppp))
head(country.plus.usa.ppp)


reset_par()
par(mfrow=c(1,3))
plot(country.plus.usa.ppp$Date,country.plus.usa.ppp$Retail.Gap, main="Retail Gap",type='l'); abline(h=0,col="red")
plot(country.plus.usa.ppp$Date,country.plus.usa.ppp$PPP, main="PPP",type='l');
plot(country.plus.usa.ppp$PPP,country.plus.usa.ppp$Retail.Gap,main="PPP vs Gap")

#Calculate Lags
retail.ppp <- Factors.Calculate.Lags(as.data.frame(country.plus.usa.ppp),months.to.analyse,c("Retail.Gap","PPP","Retail.Log","PPP.Log","Retail.vs.USA.1","Us.Log","Eppp.Log")) #optionally include USDX log


#####
ppp.f <- as.formula(paste0("Retail.Gap ~ PPP",paste0(" + PPP.Lag.",seq(1,12),collapse="")))
base.model <- lm(ppp.f, data = retail.ppp)
summary(base.model)
anova(base.model)

lag.f <- as.formula(paste0("Retail.Gap ~ PPP",paste0(" + PPP.Lag.",seq(1,6),collapse=""),paste0(" + Retail.Gap.Lag.",seq(1,6),collapse="")))
lag.model <- lm(lag.f, data = retail.ppp)
summary(lag.model)
anova(lag.model)



matplot(retail.ppp[,c("PPP","Eppp","Us")],type='l'); legend("topleft", legend=c("PPP","Eppp","Us"),cex=1.5, col=c(1,2,3),bty='n',pch=16)


























#Get calculation of how the Eppp and Retail Sales have shifted by 1 to 12 months in time
#retail.ppp <- Factors.Calculate.Lags(retail.ppp,months.to.analyse,c("PPP.Log","Retail.Log","Usdx.Log"))
retail.ppp <- Factors.Calculate.Lags(as.data.frame(country.plus.usa.ppp),months.to.analyse,c("PPP","Retail.Log","PPP.Log","Retail.vs.USA.1","Us.Log","Eppp.Log")) #optionally include USDX log
#plot(retail.ppp$Date, retail.ppp$PPP, type="l",col="blue",main="PPP for UK", xlab = "Date",ylab="PPP value");grid(NULL,NULL,col="grey")

#Take out Dates < 2016-06 (i.e. Brexit)
take.out.brexit <- FALSE
if(take.out.brexit){
  retail.ppp <- retail.ppp[retail.ppp$Date < as.Date("2016-06-01"),]
}

#Specify Factor to Test - Retail.Log for Domestic of Retail.vs.USA.Dif
factor.to.test <- "Retail.Log"