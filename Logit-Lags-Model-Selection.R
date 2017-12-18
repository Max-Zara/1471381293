##############Functions to Use
setwd("C:/Users/Maxim/Documents/University/MBA/FXRetail")
source("Code//PPP-Functions.R")
source("C:/R/MAXIM/Tools/FunctionList.R")
source("Code//PPP-LogitLag-Functions.R")

#######################################################

library(foreign)
library(readstata13)
library(MASS)
library(Hmisc)
library(RColorBrewer)
library(reshape)
library(reshape2)
library(rpart)
library(caret)
library(glmnet)
library(randomForest)
library(e1071)
library(kernlab)
library(neuralnet)
library(nnet)
library(mgcv)
library(stringr)
library(timeDate)
library(plyr)
library(zoo)

all.data <- Read.All.Data() #get all data
#####Specify Country to Use

icount.list <- c("SOA","GER","JAP", "CHI","AUS","BRA","USA")##  ,"UK")


for(icount in icount.list){
#icount <- "UK" # options are UK, SOA, JAP, GER,CHI, BRA,AUS
print(paste0("Working on :",icount))
  
months.to.analyse <- 12
use.trade.weights <- FALSE #TRUE
detrend.data <- TRUE
if(icount == "USA"){use.trade.weights <- TRUE}

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

######
#See what data series looks like
plot(i.retail$Date,i.retail$Value,type='l')

##If want PPP weighted by Trade Volume
#USA
if(use.trade.weights){
  i.ppp <- as.data.frame(all.data[1])
  if(icount=="USA"){
    trade.weights <- Get.Trade.Weights(all.data, icount="USA",source.to.use = "USCensus",method.to.use=720)} #method.to.use="natural"
  #UK
  if(icount=="UK"){
    trade.weights <- Get.Trade.Weights(all.data, icount, source.to.use = "BBG", method.to.use = "natural",to.Chart=FALSE)}
  tail(trade.weights)
  retail.ppp <- Merge.Retail.And.EPPP(i.retail, i.ppp, use.trade.weights=TRUE, trade.weights, icount)
  usa.retail.ppp <- Merge.Retail.And.EPPP(usa.retail, i.ppp, use.trade.weights=TRUE, trade.weights, icount)
}else{
  i.ppp <- as.data.frame(country.data[1])
  retail.ppp <- Merge.Retail.And.EPPP(i.retail, i.ppp, icount=icount, detrend.data= detrend.data)
  usa.retail.ppp <- Merge.Retail.And.EPPP(usa.retail, i.ppp, icount=icount, detrend.data = detrend.data)  
}

###chart how 1 month change is different to 6 months
par(mfrow=c(1,2))
plot(retail.ppp$Retail,main=paste0(icount,"Retail Sales"),type='l')
iChart.colors <- brewer.pal(6,"Paired")
matplot(retail.ppp[,paste0("Retail.Log.",seq(1,6))],col=iChart.colors,type='l',lty=1,main=paste0(icount,":How Retail Sales Look with Diff Lags"))
legend("topright",legend = paste0("Diff.",seq(1,6)), col = iChart.colors
       , bg="transparent",bty='n',cex=1.5, pch=16)
abline(h=0)

####
#Change USA data to End of MOnth
if(icount != "USA"){usa.retail.ppp$Date <- as.Date(timeLastDayInMonth(usa.retail.ppp$Date))}
country.plus.usa.retail.ppp <- merge(retail.ppp, usa.retail.ppp, by="Date")
for(i.lag in 1:6){
  country.plus.usa.retail.ppp$temp <- country.plus.usa.retail.ppp[,paste0("Retail.Log.",i.lag,".x")] - country.plus.usa.retail.ppp[,paste0("Retail.Log.",i.lag,".y")]
  colnames(country.plus.usa.retail.ppp)[ncol(country.plus.usa.retail.ppp)] <- paste0("Retail.vs.USA.",i.lag)
}
colnames(country.plus.usa.retail.ppp)[which(colnames(country.plus.usa.retail.ppp)=="Retail.y")] <- "USA.Retail"
#drop irrelevant US columns
paste0(colnames(country.plus.usa.retail.ppp),"-",str_detect(colnames(country.plus.usa.retail.ppp),".y"))
country.plus.usa.retail.ppp <- country.plus.usa.retail.ppp[,!str_detect(colnames(country.plus.usa.retail.ppp),".y")]
head(country.plus.usa.retail.ppp) 

#Get Seasonal Dummies
if(include.Dummy){
country.plus.usa.ppp <- create.monthly.dummies(country.plus.usa.retail.ppp)}
colnames(country.plus.usa.ppp) <- gsub("ts.retail.","",colnames(country.plus.usa.ppp))
colnames(country.plus.usa.ppp) <- gsub(".x","",colnames(country.plus.usa.ppp))
head(country.plus.usa.ppp)

#Get calculation of how the Eppp and Retail Sales have shifted by 1 to 12 months in time
#retail.ppp <- Factors.Calculate.Lags(retail.ppp,months.to.analyse,c("PPP.Log","Retail.Log","Usdx.Log"))
retail.ppp <- Factors.Calculate.Lags(as.data.frame(country.plus.usa.ppp),months.to.analyse,c("Retail.Log","PPP.Log","Retail.vs.USA.1","Us.Log","Eppp.Log")) #optionally include USDX log
#plot(retail.ppp$Date, retail.ppp$PPP, type="l",col="blue",main="PPP for UK", xlab = "Date",ylab="PPP value");grid(NULL,NULL,col="grey")

#Take out Dates < 2016-06 (i.e. Brexit)
take.out.brexit <- FALSE
if(take.out.brexit){
  retail.ppp <- retail.ppp[retail.ppp$Date < as.Date("2016-06-01"),]
}

###Create Directional PPP Returns - binary 1/0 outcomes

#Specify Factor to Test - Retail.Log for Domestic of Retail.vs.USA.Dif
factor.to.test <- "Retail.Log"
#factor.to.test <- "Retail.vs.USA.Dif"



#Full DataFrame of Results
classification.results <- data.frame(Retail.Base = character(), Model.Form = character(), Include.USDX = logical(), Include.AR = logical(),
                                         Model.Type = character(), Model.Lags = double(), Model.Accuracy = double(), Model.Kappa = double()
                                         , Model.Accuracy.Low = double(), Model.Accuracy.High = double()
                                         ,Cumulative.PPP.Coef = double(), Within.Sample = logical(), Average.PPP.Coef= double())
runModel <- TRUE
if(runModel==FALSE){
  #print("Not Running Model")
  classification.results <- read.csv(paste0("Results\\",icount,"-MultiLag-LinearNonLinearClassificationResults.csv"))
}else{
  repeat{
  n.factor.to.test <- 8; n.sample <- n.AR <- n.Type <- n.USDX <- 2; n.lags <- (months.to.analyse+1)
  n.models <- 8
  expected.count <- n.factor.to.test * n.sample * n.lags* n.AR * n.USDX * n.Type * n.models
  for(i.factor.to.test in c("Retail.Log","Retail.vs.USA.1",paste0("Retail.Log.",seq(1,6)))){
    
    #Calculate the Binary Pos/Neg/Bipolar values according to whether testing only that countries Retail MOM or comparison vs USA
    retail.ppp$Retail.Pos <- ifelse(retail.ppp[,i.factor.to.test] >=0, 1,0)
    retail.ppp$Retail.Neg <- ifelse(retail.ppp[,i.factor.to.test] <=0, 1,0)
    
    retail.ppp$Retail.Bipolar <- retail.ppp$Retail.Pos
    retail.ppp$Retail.Bipolar <- ifelse(retail.ppp$Retail.Neg ==1, 0,retail.ppp$Retail.Bipolar)
    retail.ppp$Retail.Bipolar <- ifelse(retail.ppp[,i.factor.to.test] ==0, 0.5,retail.ppp$Retail.Bipolar)
    
    retail.ppp.saved <- retail.ppp
      
    for(i.sample  in c(TRUE,FALSE))
    {
      if(i.sample){ #Looking at prediction within sample
        print(paste0(icount," Doing In Sample"))
        retail.analysis <- retail.ppp.saved
        retail.predict <- retail.ppp.saved
      }else{
        print(paste0(icount," Doing Out Of Sample"))
        set.seed(10)
        if(icount == "UK"){
          retail.analysis <- retail.ppp.saved[retail.ppp.saved$Date < as.Date("2015-07-30"),]
          retail.predict <- retail.ppp.saved[retail.ppp.saved$Date >= as.Date("2016-01-01"),]
        }else{
          retail.analysis <- as.data.frame(split.data.ordered(retail.ppp.saved,0.7)[1])
          retail.predict <- as.data.frame(split.data.ordered(retail.ppp.saved,0.7)[2]);
          colnames(retail.analysis) <- gsub("train.","",colnames(retail.analysis));
          colnames(retail.predict) <- gsub("test.","",colnames(retail.predict))
        }
      }
      
      ##Now Loop Through Lags to calculate how performance changes with inclusion
      for(i.lag in 0:12){
        for(include.USDX in c(TRUE,FALSE)){
          for(include.AR in c(TRUE,FALSE)){
            print(paste0("Factor:",i.factor.to.test," Sample:",i.sample," Lag:",i.lag," USDX:",include.USDX," AR:",include.AR))
            
            #Check if done already
            i.val <- classification.results[classification.results$Retail.Base == i.factor.to.test & 
                                              classification.results$Within.Sample == i.sample & classification.results$Model.Lags == i.lag 
                                            & classification.results$Include.USDX == include.USDX & classification.results$Include.AR == include.AR,]
            #Check if done already
            if(nrow(i.val)==0){
              try({
              logit.results.frames <- Calculate.Logit.Accuracy(retail.analysis,retail.predict,i.sample,i.lag,include.USDX,include.AR)
              
              for(i.frame in 1:length(logit.results.frames)){
                logit.results.frames[[i.frame]]$Retail.Base <- i.factor.to.test
                logit.results.frames[[i.frame]]$Model.Lags <- i.lag
                logit.results.frames[[i.frame]]$Within.Sample <- i.sample
                classification.results <- rbind.fill(classification.results, logit.results.frames[[i.frame]])
              }
              })
            }else{"Done Already"}
            
          }}}
    }}
    
    if(nrow(classification.results) >= expected.count*0.90){break;}
    }
  }
#Clear out duplicates
classification.results <- classification.results[!duplicated(classification.results),]

#Save the Results
write.csv(classification.results, file=paste0("Results\\",icount,"-OrderedSplitBrexitOOS-LinearNonLinearClassificationResults.csv"))
models.run <- unique(classification.results$Model.Form)
}



###### CHARTING SECTION
#Chart Results of All Models
for(icount in icount.list){
  print(paste0("Printing Charts for: ",icount))
  classification.results <- read.csv(paste0("Results\\",icount,"-OrderedSplitBrexitOOS-LinearNonLinearClassificationResults.csv"))
  subfolder.to.use <- "\\Detrended-Dummy-Brexit"
  
  for(i.factor.to.test in c("Retail.Log","Retail.vs.USA.1",paste0("Retail.Log.",seq(1,6)))){
reset_par(); par(mfrow=c(1,1))
Retail.Base <- i.factor.to.test #"Retail.Log.6" #or "Retail.vs.USA.1" "Retail.Log"
iChart <- classification.results[classification.results$Model.Form=="Logit" 
                             & classification.results$Retail.Base == Retail.Base ,]
iChart <- iChart[iChart$Model.Type == "Pos" ,]
iChart <- iChart[iChart$Include.USDX == TRUE,]
iChart <- iChart[iChart$Include.AR == FALSE,]
iChart.WS <- iChart[iChart$Within.Sample == TRUE,]; iChart.OS <- iChart[iChart$Within.Sample == FALSE,]

library(RColorBrewer)
iChart.colors <- brewer.pal(length(models.run),"Paired")
#Restore the Par Settings

dir.create(file.path("Images\\PosNegModels\\",icount,subfolder.to.use,"\\"))

png(paste0("Images\\PosNegModels\\",icount,subfolder.to.use,"\\",icount,"-",i.factor.to.test,"-AccuracyAllModels.png"), width = 1000, height= 600)
plot(x=iChart.WS$Model.Lags,y=iChart.WS$Model.Accuracy,main=paste0("Accuracy Comparison of Models: ",Retail.Base),xlab="No. of Lgas",ylab="Accuracy",
     type='l',col=iChart.colors[1],ylim=c(0,1),lwd=2)
lines(x=iChart.OS$Model.Lags,y=iChart.OS$Model.Accuracy,type='l',col=iChart.colors[1],lwd=2,lty="dashed")
abline(h=0.5)
grid(NULL,NULL,col="darkgrey")
legendlist <- c("Logit-WS","Logit-OS"); col.list <- rep(iChart.colors[1],2)
for(i.model in 2:length(models.run)){
  temp.list <- classification.results[classification.results$Model.Form==models.run[i.model]
                                      & classification.results$Retail.Base == Retail.Base ,]
  temp.list <- temp.list[temp.list$Model.Type == "Pos" ,];  temp.list <- temp.list[temp.list$Include.USDX == TRUE,]; 
  temp.list <- temp.list[temp.list$Include.AR == TRUE,]
  temp.list.WS <- temp.list[temp.list$Within.Sample ==TRUE,]; temp.list.OS <- temp.list[temp.list$Within.Sample ==FALSE,]
  
  lines(x= temp.list.WS$Model.Lags, y= temp.list.WS$Model.Accuracy,col=iChart.colors[i.model],lwd=2)
  lines(x= temp.list.OS$Model.Lags, y= temp.list.OS$Model.Accuracy,col=iChart.colors[i.model],lwd=2,lty="dashed")
  
  legendlist <- c(legendlist,paste0(models.run[i.model],"-WS"),paste0(models.run[i.model],"-OS"))
  col.list <- c(col.list,rep(iChart.colors[i.model],2))
}
legend("bottomleft",legend=legendlist,col=col.list,lwd=2,bg = "transparent",bty='n',cex=1.25,lty=rep(c(1,2),8))
dev.off()
}


####Now Get Kappas for Each Retail Lag 
par(mfrow=c(3 ,2))
png(paste0("Images\\PosNegModels\\",icount,subfolder.to.use,"\\",icount,"-MultiDif-KappaCoef-LogitModels.png"), width = 600, height = 1000)
par(mfrow=c(3 ,2))

for(i.lag in 1:6){
  i.chart <- "Pos"
  logit.lag.results <- classification.results[classification.results$Retail.Base==paste0("Retail.Log.",i.lag)
                                                       & classification.results$Model.Form=="Logit" 
                                                       & classification.results$Include.AR==FALSE 
                                                       & classification.results$Include.USDX==FALSE ,]
  plot(jitter(logit.lag.results[(logit.lag.results$Model.Type == i.chart & 
                                            logit.lag.results$Within.Sample==TRUE),]$Model.Kappa), 
       main = paste0(icount,":Kappa of ",i.chart," Logit Models Dif:",i.lag," AR:False USDX:False"),
       xlab = "No. of Lags",ylab = "Accuracy", type = 'l', lty = 1,col = "palegreen3", lwd = 2, ylim = c(-0.2,0.7))
  abline(h=0, lwd=2)
  lines(jitter(logit.lag.results[(logit.lag.results$Model.Type == i.chart
                                           & logit.lag.results$Within.Sample==FALSE),]$Model.Kappa),
        type = 'l', col = "green", lwd = 2, lty = 2)
  #VS USA
  grid(NULL, NULL, lwd = 2, col = "grey")
  legend("bottomright",legend = c("C-In.Sample","C-Out.Sample")
         , col = c("palegreen3","green"), pch=16, bg= "transparent", bty='n')
  rect(0,0.2,14,0.4,col=rgb(0,0,1,0.05), border = NULL, lwd = 0)
  rect(0,0.4,14,0.6,col=rgb(0,0,1,0.1), border = NULL, lwd = 0)
  rect(0,0.6,14,0.8,col=rgb(0,0,1,0.15), border = NULL, lwd = 0)
}
dev.off()

  
###Now Chart the Model Accuracy vs Lag, Kappa and Coefficients
domestic.logit.lag.results <- classification.results[classification.results$Retail.Base=="Retail.Log"
                                                     & classification.results$Model.Form=="Logit" 
                                                     & classification.results$Include.AR==FALSE 
                                                     & classification.results$Include.USDX==FALSE ,]
vs.USA.logit.lag.results <- classification.results[classification.results$Retail.Base=="Retail.vs.USA.1"
                                                     & classification.results$Model.Form=="Logit" 
                                                     & classification.results$Include.AR==FALSE 
                                                     & classification.results$Include.USDX==FALSE ,]
if(icount == "USA"){vs.USA.logit.lag.results <- domestic.logit.lag.results}

png(paste0("Images\\PosNegModels\\",icount,subfolder.to.use,"\\",icount,"-AccuracyKappaCoef-LogitModels.png"), width = 600, height = 1000)
par(mfrow=c(3 ,2))
#Chart the Accuracy
for(i.chart in c("Pos","Neg")){
plot(jitter(domestic.logit.lag.results[(domestic.logit.lag.results$Model.Type == i.chart
                        & domestic.logit.lag.results$Within.Sample==TRUE),]$Model.Accuracy), 
     main = paste0(icount,":Accuracy of ",i.chart," Logit Models AR:False USDX:False"),
     xlab = "No. of Lags",ylab = "Accuracy", type = 'l', lty = 1,col = "palegreen3", lwd = 2, ylim = c(0.4,1))
abline(h=0.5, lwd=2)
lines(jitter(domestic.logit.lag.results[(domestic.logit.lag.results$Model.Type == i.chart 
                         & domestic.logit.lag.results$Within.Sample==FALSE),]$Model.Accuracy),
      type = 'l', col = "green", lwd = 2, lty = 2)
grid(NULL, NULL, lwd = 2, col = "grey")
#USA Comparison
lines(jitter(vs.USA.logit.lag.results[(vs.USA.logit.lag.results$Model.Type == i.chart 
                         & vs.USA.logit.lag.results$Within.Sample==TRUE),]$Model.Accuracy),
      type = 'l', col = "orangered3", lwd = 1, lty = 1)
lines(jitter(vs.USA.logit.lag.results[(vs.USA.logit.lag.results$Model.Type == i.chart 
                                & vs.USA.logit.lag.results$Within.Sample==FALSE),]$Model.Accuracy),
      type = 'l', col = "red", lwd = 1, lty = 2)
legend("bottomright",legend = c("C-In.Sample","C-Out.Sample","vsUS-In.Sample","vsUS-Out.Sample")
       , col = c("palegreen3","green","orangered3","red"), pch=16, bg= "transparent", bty='n')}

#Chart the Kappa
for(i.chart in c("Pos","Neg")){
  plot(jitter(domestic.logit.lag.results[(domestic.logit.lag.results$Model.Type == i.chart & 
                                     domestic.logit.lag.results$Within.Sample==TRUE),]$Model.Kappa), 
       main = paste0(icount,":Kappa of ",i.chart," Logit Models"),
       xlab = "No. of Lags",ylab = "Accuracy", type = 'l', lty = 1,col = "palegreen3", lwd = 2, ylim = c(-0.2,0.7))
  abline(h=0, lwd=2)
  lines(jitter(domestic.logit.lag.results[(domestic.logit.lag.results$Model.Type == i.chart
                                    & domestic.logit.lag.results$Within.Sample==FALSE),]$Model.Kappa),
        type = 'l', col = "green", lwd = 2, lty = 2)
  #VS USA
  lines(jitter(vs.USA.logit.lag.results[(vs.USA.logit.lag.results$Model.Type == i.chart 
                                  & vs.USA.logit.lag.results$Within.Sample==TRUE),]$Model.Kappa),
        type = 'l', col = "orangered3", lwd = 1, lty = 1)
  lines(jitter(vs.USA.logit.lag.results[(vs.USA.logit.lag.results$Model.Type == i.chart 
                                  & vs.USA.logit.lag.results$Within.Sample==FALSE),]$Model.Kappa),
        type = 'l', col = "red", lwd = 1, lty = 2)
  grid(NULL, NULL, lwd = 2, col = "grey")
  legend("bottomright",legend = c("C-In.Sample","C-Out.Sample","vsUS-In.Sample","vsUS-Out.Sample")
         , col = c("palegreen3","green","orangered3","red"), pch=16, bg= "transparent", bty='n')
  rect(0,0.2,14,0.4,col=rgb(0,0,1,0.05), border = NULL, lwd = 0)
  rect(0,0.4,14,0.6,col=rgb(0,0,1,0.1), border = NULL, lwd = 0)
  rect(0,0.6,14,0.8,col=rgb(0,0,1,0.15), border = NULL, lwd = 0)}

#Chart the Coefficients
for(i.chart in c("Pos","Neg")){
  plot(abs(domestic.logit.lag.results[(domestic.logit.lag.results$Model.Type == i.chart
                                       & domestic.logit.lag.results$Within.Sample==TRUE),]$Average.PPP.Coef), 
       main = paste0(icount,": Logit Coefficient of ",i.chart," Logit Models"),
       xlab = "No. of Lags",ylab = "Accuracy", type = 'l', lty = 1,col = "palegreen3", lwd = 2, ylim = c(0,35))
  abline(h=0, lwd=2)
  lines(abs(domestic.logit.lag.results[(domestic.logit.lag.results$Model.Type == i.chart 
                                        & domestic.logit.lag.results$Within.Sample==FALSE),]$Average.PPP.Coef),
        type = 'l', col = "green", lwd = 2, lty = 2)
  #vs USA
  lines(abs(vs.USA.logit.lag.results[(vs.USA.logit.lag.results$Model.Type == i.chart 
                                      & vs.USA.logit.lag.results$Within.Sample==TRUE),]$Average.PPP.Coef),
        type = 'l', col = "orangered3", lwd = 2, lty = 1)
  lines(abs(vs.USA.logit.lag.results[(vs.USA.logit.lag.results$Model.Type == i.chart 
                                      & vs.USA.logit.lag.results$Within.Sample==FALSE),]$Average.PPP.Coef),
        type = 'l', col = "red", lwd = 2, lty = 2)
  grid(NULL, NULL, lwd = 2, col = "grey")
  legend("bottomright",legend = c("C-In.Sample","C-Out.Sample","vsUS-In.Sample","vsUS-Out.Sample")
         , col = c("palegreen3","green","orangered3","red"), pch=16, cex = 1, bg= "transparent", bty='n')
}
dev.off()
}

####PLOT COMPARISON OF ACCURACY AMONG LOGIT MODELS - Number of Lags vs Accuracy
par(mfrow=c(1,1))
png(paste0("Images\\PosNegModels\\",icount,subfolder.to.use,"\\",icount,"-Accuracy-LogitModels.png"))
plot(domestic.logit.lag.results[(domestic.logit.lag.results$Model.Type == "Pos" 
                                 & domestic.logit.lag.results$Within.Sample==TRUE),]$Model.Accuracy, 
     main = paste0(icount,": Logit Comparison of Accuracy Among Logit Models"),
     xlab = "No. of Lags",ylab = "Accuracy", type = 'l', lty = 4,col = "palegreen3", lwd = 3, ylim = c(0.4,0.9))
lines(domestic.logit.lag.results[(domestic.logit.lag.results$Model.Type == "Pos" 
                                & domestic.logit.lag.results$Within.Sample==FALSE),]$Model.Accuracy,
      type = 'l', col = "green", lwd = 3, lty = 2)
abline(h=0.5, lwd=2)
grid(NULL, NULL, lwd = 2, col = "grey")

lines(jitter(vs.USA.logit.lag.results[(vs.USA.logit.lag.results$Model.Type == "Pos" 
                                & vs.USA.logit.lag.results$Within.Sample==TRUE),]$Model.Accuracy),
      type = 'l', col = "orangered3", lwd = 2, lty = 4)
lines(jitter(vs.USA.logit.lag.results[(vs.USA.logit.lag.results$Model.Type == "Pos" 
                                & vs.USA.logit.lag.results$Within.Sample==FALSE),]$Model.Accuracy),
      type = 'l', col = "red", lwd = 2, lty = 2)

legend("bottomright",legend = c("Domestic Model wSample","Domestic Model OutSample","vs USA Model wSample","vs USA Model OutSampel"), 
       col = c("palegreen3","green","orangered3","red")
       , bg="transparent",bty='n',cex=1, pch=16)
dev.off()

#lines(logit.lag.results[(logit.lag.results$Model.Type == "Neg" & logit.lag.results$Within.Sample==TRUE),]$Model.Accuracy,
#      type = 'l', col = "darkred", lwd = 2, lty = 1)
#lines(logit.lag.results[(logit.lag.results$Model.Type == "Bipolar" & logit.lag.results$Within.Sample==TRUE),]$Model.Accuracy,
#      type = 'l', col = "darkblue", lwd = 2, lty = 1)
#legend("bottomright",legend = c("Positive Model","Negative Model","Bipolar Model"), col = c("darkgreen","darkred","darkblue"), lwd = 2)

##plot how the results look going forward for the one country
par(mfrow=c(2,2))
models.run <- unique(classification.results$Model.Form) ;unique(temp.results$Retail.Base)
temp.results <- classification.results
temp.results <- temp.results[temp.results$Include.AR==TRUE & temp.results$Include.USDX == TRUE & 
                               temp.results$Within.Sample == FALSE & temp.results$Retail.Base != "Retail.vs.USA.Dif"
                             & temp.results$Retail.Base != "Retail.Log",]
head(temp.results)
temp.Kappa.Max <- cast(temp.results, Retail.Base ~ Model.Form,value = "Model.Kappa", fun.aggregate = max)
temp.Accuracy.Max <- cast(temp.results, Retail.Base ~ Model.Form,value = "Model.Accuracy", fun.aggregate = max)
temp.Kappa.Mean <- cast(temp.results, Retail.Base ~ Model.Form,value = "Model.Kappa", fun.aggregate = mean)
temp.Accuracy.Mean <- cast(temp.results, Retail.Base ~ Model.Form,value = "Model.Accuracy", fun.aggregate = mean)

iChart.colors <- brewer.pal(ncol(temp.kappa)-1,"Paired")

reset_par()
png(paste0("Images\\PosNegModels\\",icount,subfolder.to.use,"\\FwdComparison-OOS.png"),width=1000,height=1000)
par(mfrow=c(2,2))
for(i.max.mean in c("Max","Mean")){
for(i.var in c("Accuracy","Kappa")){
  df.use <- get(paste0("temp.",i.var,".",i.max.mean))
  
  matplot(df.use,type='l',col=iChart.colors,lty=1,lwd=2,main=paste0(icount," OOS-",i.var," Stat:",i.max.mean),ylab=i.var,xlab="Periods forecasting ahead")
  grid(NULL, NULL, col= "grey")
  if(i.var == "Accuracy"){abline(h=0.5)}else{
    rect(0,0.2,8,0.4,col=rgb(0,0,1,0.05), border = NULL, lwd = 0)
    rect(0,0.4,8,0.6,col=rgb(0,0,1,0.1), border = NULL, lwd = 0)
    rect(0,0.6,8,0.8,col=rgb(0,0,1,0.15), border = NULL, lwd = 0)
    rect(0,0.8,8,1.0,col=rgb(0,0,1,0.2), border = NULL, lwd = 0)}
  legend("bottomleft",legend = colnames(df.use)[-1], 
         col = iChart.colors
         , bg="transparent",bty='n',cex=1.5, pch=16)}}
dev.off()




##Plot going forward horizon Across Countries
for(icount in icount.list){
  print(icount)
  temp.results <- read.csv(paste0("Results\\",icount,"-Detrended-LinearNonLinearClassificationResults.csv"))
  models.run <- unique(temp.results$Model.Form) ;unique(temp.results$Retail.Base)
  
  temp.results <- temp.results[temp.results$Include.AR==TRUE & temp.results$Include.USDX == TRUE & 
                                 temp.results$Within.Sample == FALSE & temp.results$Retail.Base != "Retail.vs.USA.Dif"
                               & temp.results$Retail.Base != "Retail.Log",]
  head(temp.results)
  temp.kappa <- cast(temp.results, Retail.Base ~ Model.Form,value = "Model.Kappa", fun.aggregate = max)
  temp.accuracy <- cast(temp.results, Retail.Base ~ Model.Form,value = "Model.Accuracy", fun.aggregate = max)
  
  iChart.colors <- brewer.pal(ncol(temp.kappa)-1,"Paired")
  reset_par(); 
  png(paste0("Images\\PosNegModels\\",icount,"\\Detrended\\FwdComparison-OOS-Max.png"),width = 1000, height= 600)
  par(mfrow=c(1,2))
  for(i.var in c("Accuracy","Kappa")){
    if(i.var == "Accuracy"){df.use <- temp.accuracy}else{df.use <- temp.kappa}
    matplot(df.use,type='l',col=iChart.colors,lty=1,lwd=2,main=paste0(icount," OOS-",i.var),ylab=i.var,xlab="Periods forecasting ahead")
    grid(NULL, NULL, col= "grey")
    if(i.var == "Accuracy"){abline(h=0.5)}else{
      rect(0,0.2,8,0.4,col=rgb(0,0,1,0.05), border = NULL, lwd = 0)
      rect(0,0.4,8,0.6,col=rgb(0,0,1,0.1), border = NULL, lwd = 0)
      rect(0,0.6,8,0.8,col=rgb(0,0,1,0.15), border = NULL, lwd = 0)
      rect(0,0.8,8,1.0,col=rgb(0,0,1,0.2), border = NULL, lwd = 0)}
    legend("bottomleft",legend = colnames(df.use)[-1], 
           col = iChart.colors
           , bg="transparent",bty='n',cex=1.5, pch=16)}
  dev.off()
}




###COMPARE ALL COUNTRIES RESULTS

library(reshape2)


icount.list <- c("CHI","BRA","AUS","UK","SOA","GER","JAP")

for(i.factor.to.test in c("Retail.Log","Retail.vs.USA.Dif",paste0("Retail.Log.",seq(1,6)))){
  retail.count.comparison <- data.frame()
  
  for(icount in icount.list){
  print(icount)
  temp.results <- read.csv(paste0("Results\\",icount,"-1to6Lags-LinearNonLinearClassificationResults.csv"))
  #OLD LINE - temp.results <- read.csv(paste0("Results\\",icount,"-LinearNonLinearClassificationResults.csv"))
  head(temp.results); unique(temp.results$Include.AR,temp.results$Include.USDX)
  #compare with USDX and AR
  retail.temp.frame <- data.frame(Country = icount, cast(temp.results[temp.results$Include.AR == TRUE & temp.results$Include.USDX == TRUE
                    & temp.results$Model.Type=="Pos" & temp.results$Within.Sample == FALSE
                    & temp.results$Retail.Base == i.factor.to.test,],Model.Form ~ Model.Lags , value = "Model.Kappa", FUN = mean  )  )
  
  if(ncol(retail.temp.frame) == ncol(retail.count.comparison) | length(retail.count.comparison)==0){
  retail.count.comparison <- rbind(retail.count.comparison, retail.temp.frame)  }
  #vsUSA.temp.frame <- data.frame(Country = icount, cast(temp.results[temp.results$Include.AR == TRUE & temp.results$Include.USDX == TRUE
  #                                                                    & temp.results$Model.Type=="Pos" & temp.results$Within.Sample == FALSE
  #                                                                    & temp.results$Retail.Base == "Retail.vs.USA.Dif",],Model.Form ~ Model.Lags , value = "Model.Kappa", FUN = mean  )  )
  #vsUSA.count.comparison <- rbind(vsUSA.count.comparison, vsUSA.temp.frame)  
  }
  head(retail.count.comparison); #head(vsUSA.count.comparison);
  
  #Chart The Best Model Accuracy Out of Sample
  iChart.colors <- brewer.pal(length(icount.list),"Paired")
  #Restore the Par Settings
  
  reset_par(); par(mfrow=c(1,1))
  png(paste0("Images\\PosNegModels\\Kappa-CountryComparison-",i.factor.to.test,"-OOS.png"))
  
  df.use <- retail.count.comparison
  #if(i.Factor=="Retail.Log"){df.use <- retail.count.comparison}else{df.use <- vsUSA.count.comparison}
  temp.X <- apply(df.use[df.use$Country=="CHI",],2,max, na.rm = T)
  plot(x = as.numeric(temp.X[-c(1,2)]), ylim = c(0,1), main = paste0(i.factor.to.test,": OOS Best Kappa Comparison"),
       col = iChart.colors[1], type = "l", lwd = 3, ylab = "Best Accuracy", xlab = "# of Lags", lty = 1)
  for(i.count in 2:7){
    if(nrow(df.use[df.use$Country==icount.list[i.count],])>0){
    temp.X <- apply(df.use[df.use$Country==icount.list[i.count],],2,max, na.rm = T)
    lines(x=as.numeric(temp.X[-c(1,2)]), lwd = 3,type='l', col = iChart.colors[i.count], lty = 1)
    }
  }
  grid(NULL, NULL, col= "grey")
  abline(h=0.5)
  legend("bottomleft",legend = icount.list, 
         col = iChart.colors
         , bg="transparent",bty='n',cex=1.5, pch=16)
  dev.off()
}



  
  
  #OLD LINE - temp.results <- read.csv(paste0("Results\\",icount,"-LinearNonLinearClassificationResults.csv"))
  
#Restore the Par Settings


  
  



##### OLD - LOGIT COMPARISON

###Compare USDX and Non-USDX Fit
par(mfrow=c(1,2))
for(iVar in c("Domestic","Country vs USA")){
  
  if(iVar == "Domestic"){
    table1 <- domestic.logit.lag.results
    table2 <- classification.results[classification.results$Retail.Base=="Retail.Log"
                                     & classification.results$Model.Form=="Logit" 
                                     & classification.results$Include.AR==FALSE 
                                     & classification.results$Include.USDX==TRUE ,]
  }else{
    table1 <- vs.USA.logit.lag.results
    table2 <- classification.results[classification.results$Retail.Base=="Retail.vs.USA.Dif"
                                     & classification.results$Model.Form=="Logit" 
                                     & classification.results$Include.AR==FALSE 
                                     & classification.results$Include.USDX==TRUE ,]
  }
    
  plot(table1[(table1$Model.Type == "Pos"
                                 & table1$Within.Sample==TRUE),]$Model.Accuracy, 
       main = paste0("Logit ", iVar," With/Without USDX"),
       xlab = "No. of Lags",ylab = "Accuracy", type = 'l', lty = 1,col = "palegreen3", lwd = 2, ylim = c(0.4,1))
  abline(h=0.5, lwd=2)
  lines(table1[(table1$Model.Type == "Pos" & table1$Within.Sample==FALSE),]$Model.Accuracy,
        type = 'l', col = "green", lwd = 2, lty = 2)
  lines(table2[(table2$Model.Type == "Pos"  & table2$Within.Sample==TRUE),]$Model.Accuracy,
        type = 'l', col = "darkred", lwd = 2, lty = 1)
  lines(table2[(table2$Model.Type == "Pos" & table2$Within.Sample==FALSE),]$Model.Accuracy,
        type = 'l', col = "red", lwd = 2, lty = 2)
  legend("bottomright",
         legend = c("USDX Model Within Sample","USDX Model OutOfSample","PPP-Only-Model Within Sample","PPP-Only-Model OutOfSample")
         , col = c("darkred","red","palegreen3","green"), pch=16, bg="transparent",bty="n")
  grid(NULL, NULL, lwd = 2, col = "grey")
}




##########################SCRAP
#Plot ROC Curve
library(ROCR)
prediction.pos <- prediction(pos.predictions.binary, retail.predict$Retail.Log.Pos)
prediction.neg <- prediction(neg.predictions.binary, retail.predict$Retail.Log.Neg)

perform.pos <- performance(prediction.pos, measure = "tpr", x.measure = "fpr")
perform.neg <- performance(prediction.neg, measure = "tpr",x.measure = "fpr")
plot(perform.pos); plot(perform.neg)

plot(pos.predictions, retail.ppp[,"Retail.Log.Pos"])












