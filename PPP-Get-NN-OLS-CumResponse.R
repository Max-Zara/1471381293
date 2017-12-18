##############Functions to Use
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

icount <- "UK"

months.to.analyse <- 12 #how many months lag to consider
use.trade.weights <- FALSE # TRUE - weigh PPP by the trade weights
detrend.data <- FALSE
normalize.data <- FALSE; norm.factor <- 0.1 #when renormalizing the data to be between 0 and 1, how much much additional "padding" to give on edges 
i.factor.to.test <- "Retail.Log"
include.USDX <- TRUE; include.AR <- TRUE
include.Dummy <- FALSE

factor.to.test.list <- c("Retail.Log",paste0("Retail.Log.",seq(1,6)))
#Which combinations of AR/USDX/In-Sample Out Sample do you want to test
#include.AR.list <- include.USDX.list <- in.sample.list <- c(TRUE,FALSE)

country.retail.types <- Get.Country.Retail.Types(all.data,icount);
#Pick factor with Seasonal Adjustment
retail.type.to.use <- country.retail.types[str_detect(country.retail.types,"SA") | str_detect(country.retail.types,"sa")]
include.Dummy <- FALSE; #include Dummy Monthly Variables only if no Seasonal Adjustment already
if(length(retail.type.to.use)>1){retail.type.to.use <-retail.type.to.use[1]}else{include.Dummy <- TRUE; retail.type.to.use <- country.retail.types[1]}

#Get Data from Country and US
country.data <- Get.Country.Data(all.data,icount, retail.type = retail.type.to.use)

##Now Get Retail Data and PPP Merged
i.retail <- as.data.frame(country.data[2])
i.ppp <- as.data.frame(country.data[1])
retail.ppp <- Merge.Retail.And.EPPP(i.retail, i.ppp, icount=icount, detrend.data = detrend.data)

chart.retail.vs.ppp <- TRUE
if(chart.retail.vs.ppp){
  Chart.Retail.vs.PPP(i.retail, country.data, icount, retail.ppp)}

#if(icount=="UK"){retail.ppp <- retail.ppp[retail.ppp$Date >= as.Date("2012-01-01"),]}

#Change USA data to End of MOnth so can match with the time series
library(timeDate)
if(icount != "USA"){usa.retail.ppp$Date <- as.Date(timeLastDayInMonth(usa.retail.ppp$Date))}


#Create Dummies
retail.ppp.wdum <- if(include.Dummy){
retail.ppp.wdum <- create.monthly.dummies(retail.ppp)}else{retail.ppp}
colnames(retail.ppp.wdum) <- gsub("ts.retail.","",colnames(retail.ppp.wdum))
colnames(retail.ppp.wdum) <- gsub(".x","",colnames(retail.ppp.wdum))
head(retail.ppp.wdum)
retail.ppp.wlags <- Factors.Calculate.Lags(as.data.frame(retail.ppp.wdum),months.to.analyse,c(i.factor.to.test,"PPP.Log","Us.Log","Eppp.Log")) #optionally include USDX log

take.out.brexit <- FALSE
if(take.out.brexit){
  retail.ppp.wlags <- retail.ppp.wlags[retail.ppp.wlags$Date < as.Date("2016-06-01"),]
}

retail.ppp.to.use <- retail.ppp.saved <- retail.ppp.wlags

###Renormalize Data if Required
if(normalize.data){
  factor.y <- normalize(retail.ppp.wlags[,i.factor.to.test],norm.factor)
  retail.ppp.saved[,i.factor.to.test] <- normalize(retail.ppp.to.use[,i.factor.to.test],norm.factor)
}else{
  factor.y <- retail.ppp.wlags[,i.factor.to.test]
}

#Calculate Factor Density for Estimating Responses
factor.density <- hist(factor.y,plot=FALSE)
##Extreme Densities
response.density <- quantile(factor.y,probs = c(.05,.95))#c(.01,.02,.05,.1,.3,.7,.9,.95,.98,.99))
#Add the +1SD/-1SD
response.density <- c(response.density, 'min.1SD'= mean(factor.y)-sd(factor.y), 'plus.1SD' = mean(factor.y)+sd(factor.y))
original.response.density <- quantile(retail.ppp.wlags[,i.factor.to.test],probs = c(0.05,0.95))
#Add the +1SD/-1SD
original.response.density <- c(original.response.density, 'min.1SD'= mean(retail.ppp[,i.factor.to.test])-sd(retail.ppp[,i.factor.to.test]),
                               'plus.1SD' = mean(retail.ppp[,i.factor.to.test])+sd(retail.ppp[,i.factor.to.test]))

##Break out into Within Sample and Out of Sample
i.sample <- FALSE
retail.analysis <- retail.ppp.saved[retail.ppp.saved$Date < as.Date("2015-07-30"),]
retail.predict <- retail.ppp.saved[retail.ppp.saved$Date >= as.Date("2016-01-01"),]

#summary(lm(Retail.Log ~ .,data = retail.analysis[,c(cols.to.use,"Retail.Log")]))

###CALCULATE NONLINEAR RESPONSE
lin.response.frames <- Calculate.Model.Responses(Model.Form = "OLS",retail.analysis,
                                              retail.predict,i.sample,months.to.analyse,
                                              include.USDX,include.AR,i.factor.to.test,
                                              response.density, include.Dummy, original.response.density)

lin.results <- c(Factor = i.factor.to.test, Lags=months.to.analyse, InSample = i.sample, include.AR = include.AR, 
                    include.USDX = include.USDX, include.Dummy=include.Dummy, lin.response.frames)

nonlin.response.frames <- Calculate.Model.Responses(Model.Form = "NN",retail.analysis,
                                                 retail.predict,i.sample,months.to.analyse,
                                                 include.USDX,include.AR,i.factor.to.test,
                                                 response.density, include.Dummy, original.response.density)

nonlin.results <- c(Factor = i.factor.to.test, Lags=months.to.analyse, InSample = i.sample, include.AR = include.AR, 
                 include.USDX = include.USDX, include.Dummy=include.Dummy, nonlin.response.frames)


##CHART THE RESPONSE
col.UK <- colorRampPalette(c("tomato", "gray90"))

chart.AR <- TRUE
chart.Factor <- "Retail.Log"
chart.USDX <- TRUE
chart.inSample <- FALSE
additional.heading = paste0(" (Linear Estimation) OLS ")

#png(paste0("Images\\NonLinear\\",icount,"\\1SD-5-95-Fan(NegSD)\\",icount,"-NN-",
#           "Factor.",chart.Factor,"-AR.",chart.AR,"-USDX.",chart.USDX,"-InSample.",chart.inSample,".png"),width=1000,height=600)

optional.resize <- if(normalize.data){(max(retail.ppp.wlags[,chart.Factor])-min(retail.ppp.wlags[,chart.Factor]))*(1+2*norm.factor)}else{1}

Chart.NonLin.Responses(lin.results[[7]], additional.heading, optional.color = brewer.pal(10,"RdGy"), 
                       optional.nmonths = months.to.analyse, response.density = response.density, optional.resize = optional.resize
                       ,use.means = FALSE #chart deterministic (use.means = TRUE) or probabilistic (use.means = FALSE)
                       , factor.col = 4 # typically 1 = 5%, 2 = 95%, 3 = -1SD, 4 = +1SD
                       , chart.single = FALSE, chart.cumulative = TRUE
                       , optional.percentiles = seq(0.1,0.9,0.1)
                       ,add.from.row = 0)


additional.heading = paste0(" (Non-Linear Estimation) NN ")

Chart.NonLin.Responses(nonlin.results[[7]], additional.heading, optional.color = brewer.pal(10,"RdGy"), 
                       optional.nmonths = months.to.analyse, response.density = response.density, optional.resize = optional.resize
                       ,use.means = TRUE #chart deterministic (use.means = TRUE) or probabilistic (use.means = FALSE)
                       , factor.col = 4 # typically 1 = 5%, 2 = 95%, 3 = -1SD, 4 = +1SD
                       , chart.single = FALSE, chart.cumulative = TRUE
                       , optional.percentiles = seq(0.1,0.9,0.1)
                       ,add.from.row = 0)

lines(lin.results[[7]])
dev.off()
