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
#####Specify Country to Use

icount.list <- c("JAP","CHI","BRA","AUS","UK","SOA","GER","USA")
for(icount in icount.list){

#icount <- "UK" # options are UK, SOA, JAP, GER,CHI, BRA,AUS

months.to.analyse <- 12 #how many months lag to consider
use.trade.weights <- FALSE # TRUE - weigh PPP by the trade weights
detrend.data <- TRUE
normalize.data <- TRUE; norm.factor <- 0.1 #when renormalizing the data to be between 0 and 1, how much much additional "padding" to give on edges 
factor.to.test.list <- c("Retail.Log","Retail.vs.USA.1",paste0("Retail.Log.",seq(1,6)))
#Which combinations of AR/USDX/In-Sample Out Sample do you want to test
include.AR.list <- include.USDX.list <- in.sample.list <- c(TRUE,FALSE)
if(icount == "USA"){use.trade.weights <- TRUE} # TRUE}


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

##CHART ALL TRADE VOLUME
###Get the original Non-TW values
  i.ppp.nontw <- as.data.frame(country.data[1])
  retail.ppp.nontw <- Merge.Retail.And.EPPP(i.retail, i.ppp.nontw, icount=icount)
  
  all.ppp <- as.data.frame(all.data[1])
  pivot.all.ppp <- cast(all.ppp[all.ppp$Measure.Names=="Eppp",],Date ~ Country,value="Value",fun.aggregate = mean)
  
  x2 <- sapply(seq_len(ncol(pivot.all.ppp)), function(x) pivot.all.ppp[,x] [min(which(!is.na(pivot.all.ppp[,x])))])
  x3 <- sapply(seq_len(ncol(pivot.all.ppp)), function(x) pivot.all.ppp[,1][which(pivot.all.ppp[,x]==x2[x])])  
  x3 <- as.Date(x3,origin ="1970-01-01")
  names(x3) <- colnames(pivot.all.ppp)
  
  head(pivot.all.ppp)
  reset_par()
  matplot(pivot.all.ppp,type='l')
#################################

##If want PPP weighted by Trade Volume
#USA
if(use.trade.weights){
  i.ppp <- as.data.frame(all.data[1]); #PPP for all countries
  if(icount=="USA"){
  trade.weights <- Get.Trade.Weights(all.data, icount="USA",source.to.use = "USCensus",method.to.use=720)} #method.to.use="natural"
  #UK
  if(icount=="UK"){
  trade.weights <- Get.Trade.Weights(all.data, icount, source.to.use = "BBG", method.to.use = "natural",to.Chart=FALSE)}
  tail(trade.weights)
  retail.ppp.tw <- retail.ppp <- Merge.Retail.And.EPPP(i.retail, i.ppp, use.trade.weights=TRUE, trade.weights, icount)
  usa.retail.ppp <- Merge.Retail.And.EPPP(usa.retail, i.ppp, use.trade.weights=TRUE, trade.weights, icount)
}else{
  i.ppp <- as.data.frame(country.data[1])
  retail.ppp <- Merge.Retail.And.EPPP(i.retail, i.ppp, icount=icount, detrend.data = detrend.data)
  usa.retail.ppp <- Merge.Retail.And.EPPP(usa.retail, i.ppp, icount=icount, detrend.data= detrend.data)  
}

######
#See what data series looks like
reset_par(); par(mfrow=c(1,2))
plot(retail.ppp$Date,retail.ppp$Retail.Log,type='l',col="red",main=paste0("Retail Sales: ",icount))
plot(retail.ppp$Date,retail.ppp$PPP,type='l',col="darkred",main=paste0("PPP: ",icount),ylim=range(c(retail.ppp$PPP,retail.ppp.nontw$PPP)))
lines(retail.ppp.nontw$Date,retail.ppp.nontw$PPP,col="red")
legend("topleft",legend=c("Trade-Weighted","USD-PPP"),col=c("darkred","red"),pch=0.8)
##can plot what the PPP trade-weighted vs non-trade weighted looks like

#Restrict number of dates (UK PPP not full before 2012)
if(icount=="UK"){retail.ppp <- retail.ppp[retail.ppp$Date >= as.Date("2012-01-01"),]}

####
#Change USA data to End of MOnth
library(timeDate)
if(icount != "USA"){usa.retail.ppp$Date <- as.Date(timeLastDayInMonth(usa.retail.ppp$Date))}

country.plus.usa.retail.ppp <- merge(retail.ppp, usa.retail.ppp, by="Date")
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
country.plus.usa.ppp <- create.monthly.dummies(country.plus.usa.retail.ppp)
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

#Specify Factor to Test - Retail.Log for Domestic of Retail.vs.USA.Dif
factor.to.test <- "Retail.Log"
#factor.to.test <- "Retail.vs.USA.Dif"

#Full DataFrame of Results

classification.results <- list(); run.classification <- FALSE;
response.results <- list(); run.response <- TRUE;
#Can load results
#classification.results <- load("ClassificationResults.Rda")

#Save the Table Before Sampling
retail.ppp.to.use <- retail.ppp.saved <- retail.ppp

repeat{

  n.factor.to.test <- length(factor.to.test.list); 
  n.sample <- length(in.sample.list); n.AR <- length(include.AR.list); n.USDX <- length(include.USDX.list); n.lags <- (months.to.analyse+1)
  n.models <- 8
  class.expected.count <- n.factor.to.test * n.sample * n.lags* n.AR * n.USDX * n.models
  response.expected.count <- n.factor.to.test * n.sample *  n.AR * n.USDX 
  
  for(i.factor.to.test in factor.to.test.list){
    
    #Renormalize the Factor
    if(normalize.data){
      factor.y <- normalize(retail.ppp[,i.factor.to.test],norm.factor)
    }else{
      factor.y <- retail.ppp[,i.factor.to.test]
    }
    
    factor.density <- hist(factor.y,plot=FALSE)
    
    ##Extreme Densities
    response.density <- quantile(factor.y,probs = c(.05,.95))#c(.01,.02,.05,.1,.3,.7,.9,.95,.98,.99))
    #Add the +1SD/-1SD
    response.density <- c(response.density, 'min.1SD'= mean(factor.y)-sd(factor.y), 'plus.1SD' = mean(factor.y)+sd(factor.y))
    original.response.density <- quantile(retail.ppp[,i.factor.to.test],probs = c(0.05,0.95))
    #Add the +1SD/-1SD
    original.response.density <- c(original.response.density, 'min.1SD'= mean(retail.ppp[,i.factor.to.test])-sd(retail.ppp[,i.factor.to.test]),
                                   'plus.1SD' = mean(retail.ppp[,i.factor.to.test])+sd(retail.ppp[,i.factor.to.test]))
    
    
    for(i.sample  in in.sample.list)
    {
      retail.ppp.saved[,i.factor.to.test] <- normalize(retail.ppp.to.use[,i.factor.to.test],norm.factor)
      if(i.sample){ #Looking at prediction within sample
        print("Doing In Sample")
        retail.analysis <- retail.ppp.saved
        retail.predict <- retail.ppp.saved
      }else{
        print("Doing Out Of Sample")
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
      for(include.USDX in include.USDX.list){
        for(include.AR in include.AR.list){
          print(paste0("Country: ",icount," Factor:",i.factor.to.test," Sample:",i.sample," USDX:",include.USDX," AR:",include.AR, " Dummies:",include.Dummy))
          
          try({if(run.classification){
            
            if(length(classification.results)>0){tmp.class <- as.data.frame(classification.results[,1:(ncol(classification.results)-1)])
            tmp.check <- tmp.class[tmp.class$Factor == i.factor.to.test & 
                                     tmp.class$include.AR == include.AR & 
                                     tmp.class$include.USDX == include.USDX & 
                                     tmp.class$include.Dummy == include.Dummy]}else{tmp.check <- data.frame()}
            
            if(nrow(tmp.check)==0){
              results.frames <- Calculate.Response.Functions.And.Accuracy(retail.analysis,
                                                                                retail.predict,i.sample,months.to.analyse,
                                                                                include.USDX,include.AR,i.factor.to.test,
                                                                                factor.density, include.Dummy)
              #Consider Charting
              #Chart.Fan.Models(results.frames[[2]],paste0(icount," LASSO "))
              tmp.results <- c(Factor = i.factor.to.test, Lags=months.to.analyse, InSample = i.sample, include.AR = include.AR, 
                               include.USDX = include.USDX, include.Dummy=include.Dummy, results.frames)
              classification.results <- rbind(classification.results, tmp.results)
            }
          }
          if(run.response){
            
            if(length(response.results)>0){tmp.response <- as.data.frame(response.results[,1:(ncol(response.results)-1)])
            tmp.check <- tmp.response[tmp.response$Factor == i.factor.to.test & 
                                        tmp.response$include.AR == include.AR & 
                                        tmp.response$include.USDX == include.USDX & 
                                        tmp.response$InSample == i.sample,]}else{tmp.check<-data.frame()}
            if(nrow(tmp.check)==0){
            
              response.frames <- Calculate.NonLinear.Responses(retail.analysis,
                                                               retail.predict,i.sample,months.to.analyse,
                                                               include.USDX,include.AR,i.factor.to.test,
                                                               response.density, include.Dummy, original.response.density)
              
              tmp.results <- c(Factor = i.factor.to.test, Lags=months.to.analyse, InSample = i.sample, include.AR = include.AR, 
                               include.USDX = include.USDX, include.Dummy=include.Dummy, response.frames)
              response.results <- rbind(response.results, tmp.results)
            }
          }
          })
    }}}}
  
  if(run.response)
    if(nrow(response.results) > 0.9*response.expected.count)
        break;
  
  if(run.classification)
    if(nrow(classification.results) > 0.9*class.expected.count)
      break;
}

if(run.classification){
save(classification.results, file=paste0("Results\\",icount,"-AbsoluteRescaled-MultiLag-ClassificationResults.Rda"))}
if(run.response){
save(response.results, file=paste0("Results\\",icount,"-UKOnly-AbsoluteRescaled-MultiLag-ResponseResults.Rda"))}


#load(paste0("Results\\",icount,"-Boot-ClassificationResults.Rda"))
#names(classification.results) <- c("Factor","Lags","InSample","include.AR","include.USDX","OLS","LASSO","Ridge","Tree","Forest","SVM","NN","GAM")








##Extract the Results you wish and save them

col.USA <- col.UK <- colorRampPalette(c("tomato", "gray90"))
col.BRA <- colorRampPalette(c("forestgreen", "gray90"))
col.SOA <- colorRampPalette(c("darkgoldenrod", "gray90"))
col.JAP <- colorRampPalette(c("darkred", "gray90"))
col.AUS <- colorRampPalette(c("darkcyan", "gray90"))
col.GER <- colorRampPalette(c("khaki", "gray90"))


chart.AR <- TRUE
chart.Factor <- "Retail.Log"
chart.USDX <- TRUE
chart.inSample <- FALSE
for(chart.AR in include.AR.list){
  for(chart.Factor in factor.to.test.list){
    for(chart.USDX in include.USDX.list){
      for(chart.inSample in in.sample.list){
        
        if(run.classification){
          tmp1 <- classification.results[classification.results[,"include.AR"]==chart.AR & classification.results[,"Factor"]== chart.Factor &
                                           classification.results[,"include.USDX"]==chart.USDX & classification.results[,"InSample"]==chart.inSample ]
          
          
          
          model.list <- c("OLS","LASSO","Ridge","Tree","Forest","SVM","NN","GAM")
          for(i.temp in 1:8){
            print(paste0(i.temp, " AR:", chart.AR, " Factor:", chart.Factor," USDX:",chart.USDX, " Sample:", chart.inSample))
            temp.model.name <- model.list[i.temp]
            temp.models <- tmp1[i.temp+6][[1]]
            if(length(temp.models)==11){ #print only if not error
              png(paste0("Images\\Response",icount,"\\",temp.model.name,"\\",icount,"-",temp.model.name,"-",
                                  "Factor.",chart.Factor,"-AR.",chart.AR,"-USDX.",chart.USDX,"-InSample.",chart.inSample,".png"))
              
              temp.color <- get(paste0("col.",icount))
              Chart.Fan.Models(temp.models,paste0(temp.model.name," "),optional.color=temp.color, optional.nmonths=months.to.analyse)
              dev.off()
            }
          }
        }
        if(run.response){
          tmp1 <- response.results[response.results[,"include.AR"]==chart.AR & response.results[,"Factor"]== chart.Factor &
                                     response.results[,"include.USDX"]==chart.USDX & response.results[,"InSample"]==chart.inSample ]
          additional.heading = paste0(" AR:", chart.AR, " Factor:", chart.Factor," USDX:",chart.USDX, " Sample:", chart.inSample)
          
          if(length(tmp1[[7]])>1){
          png(paste0("Images\\NonLinear\\",icount,"\\1SD-5-95\\",icount,"-NN-",
                     "Factor.",chart.Factor,"-AR.",chart.AR,"-USDX.",chart.USDX,"-InSample.",chart.inSample,".png"),width=1000,height=600)
          
          if(normalize.data){optional.resize <- (max(retail.ppp[,chart.Factor])-min(retail.ppp[,chart.Factor]))*(1+2*norm.factor)}
            
          Chart.NonLin.Responses(tmp1[[7]], additional.heading, optional.color = brewer.pal(10,"RdGy"), 
                                 optional.nmonths = months.to.analyse, response.density = response.density, optional.resize = optional.resize)
          dev.off()
          }
        }
}}}}
#Loop bracket for all countries
}

reset_par();plot(response.density,type='b',xaxt='n'); axis(1, at = 1:10,labels=names(response.density));abline(h=0)


#####Chart all models together in one chart
results.count.list <- c("JAP","GER")

library(RColorBrewer)
model.cols <- brewer.pal(8,"Paired")
model.list <- c("OLS","LASSO","Ridge","Tree","Forest","SVM","NN","GAM")

for(icount in results.count.list){
  
  load(paste0("Results\\",icount,"-Boot-ClassificationResults.Rda"))
  
  for(chart.AR in c(TRUE,FALSE)){
    for(chart.Factor in c("Retail.Log","Retail.vs.USA.Dif")){
      for(chart.USDX in c(TRUE,FALSE)){
        for(chart.inSample in c(TRUE,FALSE)){
  
          tmp1 <- classification.results[classification.results[,"include.AR"]==chart.AR & classification.results[,"Factor"]== chart.Factor &
                                           classification.results[,"include.USDX"]==chart.USDX & classification.results[,"InSample"]==chart.inSample ]
          #Plot OLS First
          png(paste0("Images\\ResponseSummaries\\",icount,"\\",icount,"-",
                     "Factor.",chart.Factor,"-AR.",chart.AR,"-USDX.",chart.USDX,"-InSample.",chart.inSample,".png"))
          par(mfrow=c(1,2))
          for(i.cumulative in c(FALSE, TRUE)){
          heading.to.use <- paste(icount,if(i.cumulative){"Cumul."}else{"Monthly"}," AR:",chart.AR,"USDX:",chart.USDX,"InSample:",chart.inSample)
            for(i.temp in 1:8){
            temp.model.name <- model.list[i.temp]
            temp.models <- tmp1[i.temp+5][[1]]
            
            if(i.temp==1){ add.tf = FALSE}else{add.tf=TRUE}
            Add.Line.For.Model(temp.models, model.cols[i.temp], add=add.tf,cum = i.cumulative, heading.to.use)
            }
          abline(h=0)
          grid(NULL,NULL,col="grey",lwd=2)
          legend("bottomleft",legend=model.list,col=model.cols,pch=16,bg="transparent",bty='n')
          }
          dev.off()
        }}}}
  }
  