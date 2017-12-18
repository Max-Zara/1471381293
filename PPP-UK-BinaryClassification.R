##############Functions to Use
setwd("C:/Users/Maxim/Documents/University/MBA/FXRetail")
source("C:/R/MAXIM/Tools/FunctionList.R")
source("Code//PPP-Functions.R")
source("Code//PPP-ResponseFunctions.R")
source("Code//PPP-LogitLag-Functions.R")

########################################
#Functions

Chart.Prediction.Function <- function(test.data, model.predictions, model.name, chart.detrended = FALSE){
  plot(test.data$Date,model.predictions,type = 'l',xlab="Date",ylab="Prob. Of Positive Move",xaxt='n',ylim = c(0,1)
       ,main = paste0(model.name," Predictions of RSI Change MOM"))
  axis.Date(1,test.data$Date,test.data$Date, format = "%m-%Y")
  abline(v=as.Date("2016-06-01"),col="red",lwd=2,lty=2)
  abline(h=0.5,col="blue")
  
  if(chart.detrended){
    
    detr.max <- max(test.data$Retail.Log)
    detr.min <- min(test.data$Retail.Log)
    detr.min.max <- max(abs(detr.min),detr.max)
    
    lines(test.data$Date,(test.data$Retail.Log-sign(detr.min)*detr.min.max)/(2*detr.min.max)
          ,lty=1,col="red")
    axis(4,at = seq(0,1,length.out=11),labels = round(seq(sign(detr.min)*detr.min.max,detr.min.max,length.out=11),3))
  }else{
  lines(test.data$Date,(test.data$Retail-110)/5.5*0.5,lty=1,col="red")
  axis(4,at = seq(0,1,length.out=12),labels = 110:121)}
  
  legend("bottomright",legend = c("Model Prediction","RSI Index"),col = c("black","red")
         , bg="transparent",bty='n',cex=1.5, pch=16)
}

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

n.boot <- 1000
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
retail.ppp <- Merge.Retail.And.EPPP(i.retail, i.ppp, icount=icount, detrend.data = detrend.data, optional.lags = 1)

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

#Create Binary Function on whether +ve or -ve
retail.ppp.wlags$Binary <- ifelse(retail.ppp.wlags[,i.factor.to.test] >=0, 1,0)

retail.ppp.to.use <- retail.ppp.saved <- retail.ppp.wlags

##Break out into Within Sample and Out of Sample
i.sample <- FALSE
retail.analysis <- retail.ppp.saved[retail.ppp.saved$Date < as.Date("2015-07-30"),]
retail.predict <- retail.ppp.saved[retail.ppp.saved$Date >= as.Date("2016-01-01"),]

#logit.results.frames <- Calculate.Logit.Accuracy(retail.analysis,retail.predict,i.sample,i.lag,include.USDX,include.AR)

cols.to.use <- Get.Cols.To.Use(6,include.USDX, include.AR, include.Dummy)

#Model Structure
#Probit
probit.model <- glm(Binary ~., family = binomial(link='probit'), data = retail.analysis[,c("Binary",cols.to.use)])

#Logit Lasso
lasso.model <- cv.glmnet(x=as.matrix(retail.analysis[complete.cases(retail.analysis),cols.to.use])
                         ,y=as.matrix(as.factor(retail.analysis[complete.cases(retail.analysis),]$Binary))
                         , family = "binomial", alpha = 1)
coef(lasso.model)

##Random Forest Model
forest.model <- randomForest(as.factor(Binary) ~.,data=retail.analysis[complete.cases(retail.analysis),c("Binary",cols.to.use)])

##NNet Model
f <- as.formula(paste0("Binary~",paste(cols.to.use, collapse = " + ")))
nn.model = neuralnet(f,data=retail.analysis[complete.cases(retail.analysis),c("Binary",cols.to.use)]
                     , hidden = mean(c(round(27/3,0),3)),linear.output = FALSE, act.fct = "tanh")


#Random Forest

#NN

#Predictions
probit.predictions <- predict(probit.model,retail.predict[,c("Binary",cols.to.use)], type= "response")
predictions.binary <- ifelse(probit.predictions > 0.5, 1,0);
lasso.predictions <- predict(lasso.model, as.matrix(retail.predict[,cols.to.use]), s= "lambda.min",type="response")
forest.predictions = predict(forest.model, retail.predict[,cols.to.use],type="response") #or class
nnet.predictions <- t(compute(nn.model, retail.predict[,c(cols.to.use)])$net.result)

#Chart the Data
reset_par()
Chart.Prediction.Function(retail.predict,probit.predictions,paste0("Probit",if(detrend.data){" (Detrended)"}),chart.detrended = detrend.data)
Chart.Prediction.Function(retail.predict,lasso.predictions,paste0("LASSO",if(detrend.data){" (Detrended)"}), chart.detrended = detrend.data)
Chart.Prediction.Function(retail.predict,as.numeric(forest.predictions)-1,paste0("Random Forest",if(detrend.data){" (Detrended)"}),chart.detrended = detrend.data)
Chart.Prediction.Function(retail.predict,as.numeric(nnet.predictions),paste0("Neural Net",if(detrend.data){" (Detrended)"}),chart.detrended = detrend.data)


###Bootstrapped classifcation

set.seed(1)

uk.boot.results <- double()

for(i.boot in 1:n.boot){
  print(paste0("Working on Boot: ",i.boot, " Out of ",n.boot))
  temp.resample <- sample(1:nrow(retail.analysis),nrow(retail.analysis),replace = TRUE)
  retail.boot <- retail.analysis[temp.resample,]
  
  probit.model <- glm(Binary ~., family = binomial(link='probit'), data = retail.boot[,c("Binary",cols.to.use)])
  probit.predictions <- predict(probit.model,retail.predict[,c("Binary",cols.to.use)], type= "response")
  probit.predictions.binary <- ifelse(probit.predictions > 0.5, 1,0);
  probit.confusion.matrix <- confusionMatrix(table(factor(probit.predictions.binary, levels= c(0,1)),retail.predict$Binary))
  
  #Logit Lasso
  ridge.model <- cv.glmnet(x=as.matrix(retail.boot[complete.cases(retail.boot),cols.to.use])
                           ,y=as.matrix(as.factor(retail.boot[complete.cases(retail.boot),]$Binary))
                           , family = "binomial", alpha = 0)
  ridge.predictions <- predict(ridge.model, as.matrix(retail.predict[,cols.to.use]), s= "lambda.min",type="response")
  ridge.predictions.binary <- ifelse(ridge.predictions > 0.5, 1,0);
  ridge.confusion.matrix <- confusionMatrix(table(factor(ridge.predictions.binary, levels= c(0,1)),retail.predict$Binary))
  
  #coef(ridge.model)
  
  ##Random Forest Model
  forest.model <- randomForest(as.factor(Binary) ~.,data=retail.boot[complete.cases(retail.boot),c("Binary",cols.to.use)])
  forest.predictions = predict(forest.model, retail.predict[,cols.to.use],type="response") #or class
  forest.confusion.matrix <- confusionMatrix(table(factor(forest.predictions, levels= c(0,1)),retail.predict$Binary))
  
  ##NNet Model
  f <- as.formula(paste0("Binary~",paste(cols.to.use, collapse = " + ")))
  nn.model = neuralnet(f,data=retail.boot[complete.cases(retail.boot),c("Binary",cols.to.use)]
                       , hidden = mean(c(round(27/3,0),3)),linear.output = FALSE, act.fct = "tanh")
  nnet.predictions <- t(compute(nn.model, retail.predict[,c(cols.to.use)])$net.result)
  nnet.predictions.binary <- ifelse(nnet.predictions > 0.5, 1,0);
  nnet.confusion.matrix <- confusionMatrix(table(factor(nnet.predictions.binary, levels= c(0,1)),retail.predict$Binary))
  
  temp.result <- data.frame(BootNo= i.boot, Detrended = detrend.data, 
                            Probit.Accuracy = as.numeric(probit.confusion.matrix$overall["Accuracy"]), Probit.Kappa = as.numeric(probit.confusion.matrix$overall["Kappa"]),
                            Ridge.Accuracy = as.numeric(ridge.confusion.matrix$overall["Accuracy"]), Ridge.Kappa = as.numeric(ridge.confusion.matrix$overall["Kappa"]),
                            Forest.Accuracy = as.numeric(forest.confusion.matrix$overall["Accuracy"]), Forest.Kappa = as.numeric(forest.confusion.matrix$overall["Kappa"]),
                            NN.Accuracy = as.numeric(nnet.confusion.matrix$overall["Accuracy"]), NN.Kappa = as.numeric(nnet.confusion.matrix$overall["Kappa"]))
  uk.boot.results <- rbind(uk.boot.results, temp.result)
}

write.csv(uk.boot.results, file=paste0("UK-Boot-Results-Detrended-",detrend.data,".CSV"))

detrend.data <- TRUE
uk.boot.results <- read.csv(paste0("UK-Boot-Results-Detrended-",detrend.data,".CSV"))


reset_par(); par(mfrow=c(2,2))
i.factor <- "Kappa";# i.factor <- "Accuracy"
for(i.model in c("Probit","Ridge","Forest","NN")){

hist((uk.boot.results[,paste0(i.model,".",i.factor)]),
       main = paste0(i.model,if(detrend.data){" (Detrended)"}," ",i.factor," after 1000 Straps"),xlab=i.factor,lwd=2,col="darkgrey") 
#plot(density(uk.boot.results[,paste0(i.model,".",i.factor)]),lwd = 2,
#     main = paste0(i.model,if(detrend.data){" (Detrended)"}," ",i.factor," after 1000 Straps"))

if(i.factor == "Kappa"){  
rect(0.2,-1,0.4,2000,col=rgb(0,1,0,0.05), border = NULL, lwd = 0)
rect(0.4,-1,0.6,2000,col=rgb(0,1,0,0.1), border = NULL, lwd = 0)
rect(0.6,-1,0.8,2000,col=rgb(0,1,0,0.15), border = NULL, lwd = 0)
rect(0.8,-1,1.0,2000,col=rgb(0,1,0,0.2), border = NULL, lwd = 0)
rect(-300,-1,0,2000,col=rgb(1,0,0,0.1), border = NULL, lwd = 0)
}else if(i.factor == "Accuracy"){
  rect(0.5,0,0.6,2000,col=rgb(0,1,0,0.05), border = NULL, lwd = 0)
  rect(0.6,0,0.7,2000,col=rgb(0,1,0,0.1), border = NULL, lwd = 0)
  rect(0.7,0,0.8,2000,col=rgb(0,1,0,0.15), border = NULL, lwd = 0)
  rect(0.8,0,1.0,2000,col=rgb(0,1,0,0.2), border = NULL, lwd = 0)
  rect(-300,0,0.5,2000,col=rgb(1,0,0,0.1), border = NULL, lwd = 0)
}
}

