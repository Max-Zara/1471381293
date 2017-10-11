##############Functions to Use
setwd("C:/Users/Maxim/Documents/University/MBA/FXRetail")
source("PPP-Functions.R")

#######################################################

all.data <- Read.All.Data() #get all data
#####Specify Country to Use
icount <- "UK"
useUSD <- TRUE
if(useUSD){ cross.currency <- "N/A" }else{ cross.currency <- "USDEUR"}
months.to.analyse <- 12
factor.to.use <- "Usdx.Log" #choice of "Eppp.Log","Usdx.Log","FX.Log"

#Can Specify Specific Retail Type to Use (Seasonally adjusted or not?)
country.data <- Get.Country.Data(all.data,icount,cross.currency, retail.type = "NA")

i.retail <- as.data.frame(country.data[2]); i.ppp <- as.data.frame(country.data[1])
i.fx <- as.data.frame(country.data[3])
######
#See what data series looks like
par(mfrow=c(1,2)); plot(i.retail$Date,i.retail$Value); plot(i.ppp[i.ppp$Measure.Names=="Eppp",]$Date, i.ppp[i.ppp$Measure.Names=="Eppp",]$Value)

retail.ppp <- Merge.Retail.And.EPPP.And.FX(i.retail, i.ppp, i.fx)
library(PerformanceAnalytics)
chart.Correlation(retail.ppp[,c("Retail.Log","Usdx.Log","Eppp.Log")], pch=21)

acf(retail.ppp[,"Retail.Log"])
acf(na.omit(retail.ppp[,"Eppp.Log"]))
acf(na.omit(retail.ppp[,"FX.Log"]))

#Get calculation of how the Eppp and Retail Sales have shifted by 1 to 12 months in time
retail.ppp <- Retail.And.Factor.Calculate.MA(retail.ppp,months.to.analyse,factor.to.use)
chart.Correlation(retail.ppp[,c("Retail.MA.6",paste0(rep(paste0(factor.to.use,".MA."),8),seq(1,8,1)))], pch=21)

writeCSV <- FALSE
if(writeCSV==TRUE)
write.csv(retail.ppp,"MergedFile-RetailPPP.csv")

#Take out Dates < 2016-06 (i.e. Brexit)
take.out.brexit <- FALSE
if(take.out.brexit){
  retail.ppp <- retail.ppp[retail.ppp$Date < as.Date("2016-06-01"),]
}

#Now Regress the MAs to get the fit results
MA.regression.results <- Retail.Get.MA.Results(data.to.use = retail.ppp,factor.to.use = factor.to.use, 
                                                       months.to.analyse = c(1:months.to.analyse))

#Chart the Beta and Coefficient
par(mfrow=c(2,2))
boxplot(MA.regression.results$Factor.Coef, main=paste0("Distribution of Beta of ",factor.to.use," on Retail Sales MA"))
plot(density(MA.regression.results$Factor.Coef), main= paste0("Density of Beta of ",factor.to.use," on Retail Sales MA"))
boxplot(MA.regression.results$Factor.R2 ~ MA.regression.results$Factor.Lag, main = paste0("R2 vs Number of Months Lagged for ",factor.to.use))
boxplot(MA.regression.results$Factor.R2 ~MA.regression.results$Retail.Lag, main = "R2 vs Number of Months Lagged for Retail.Lag")

library(reshape2)
MA.regr.reshape <- dcast(MA.regression.results, Retail.Lag ~ Factor.Lag, value.var = "Factor.R2")
head(MA.regr.reshape)

#Determine which is the best lag
MA.regr.reshape$BestLag <- NA
MA.regr.reshape$DifLag <- NA
for(ilag in 1:months.to.analyse){
  temp.max.r2 <- max(MA.regr.reshape[1:months.to.analyse,ilag+1])
  MA.regr.reshape[ilag,]$BestLag <- which(MA.regr.reshape[1:months.to.analyse,ilag+1]==temp.max.r2)
  MA.regr.reshape[ilag,]$DifLag <- which(MA.regr.reshape[1:months.to.analyse,ilag+1]==temp.max.r2) - ilag
}

mycols <- topo.colors(100,0.5)
#Plot the density
par(mfrow=c(1,1))
image(1:months.to.analyse,1:months.to.analyse,as.matrix(MA.regr.reshape[,2:months.to.analyse+1]),col=mycols,
      main=paste0("Moving Average R2 Performance - ",factor.to.use," & Retail"),xlab="Retail.Log", ylab=paste0(factor.to.use,".Log"))
contour(1:months.to.analyse,1:months.to.analyse,as.matrix(MA.regr.reshape[,2:(months.to.analyse+1)]),add=T)
lines(MA.regr.reshape$BestLag,1:months.to.analyse,type="l",col="red",lwd=2)

barplot(MA.regr.reshape$DifLag, xlab = "Retail MA Period Considered", ylab="Additional Periods", main="Lags to consider relative to Retail MA")
grid(NULL, NULL, lwd=1,col="grey")

########Construct the Impulse Response Function for Retail MA

IRF.Results <- list(Retail.MA = double(), Retail.R2 = double(), Cumulative.Coefs = double(), Coefs = double())
IRF.AIC.Results <- data.frame(Retail.MA = double(), Retail.R2 = double(), Best.AIC.Num.Lags = double(), Best.AIC.R2 = double(), Best.BIC.Num.Lags = double(), Best.BIC.R2 = double())

for(imonth.retail in 1:months.to.analyse){
  #Get the Retail for that Period
  ifactor.ma <- retail.ppp[,paste0(factor.to.use,".MA.1")]    
  
  ifactor.mas <- NA
  for(i.period in 1:months.to.analyse){    ifactor.temp <- c(rep(NA,i.period-1),ifactor.ma[1:(length(ifactor.ma)-i.period+1)])
    ifactor.mas <- cbind(ifactor.mas,ifactor.temp)   }
  ifactor.mas <- ifactor.mas[,-1]
  colnames(ifactor.mas) <- paste0(rep(factor.to.use,".Delta.M",months.to.analyse),seq(0,months.to.analyse-1,1))
  head(ifactor.mas)
  
  iretail.ma <- retail.ppp[,c("Date",paste0("Retail.MA.",imonth.retail))]    
  head(iretail.ma)
  
  #Now bind table for total
  total.table <- cbind(Retail.Delta = iretail.ma[,2], ifactor.mas)
  rownames(total.table) <- as.Date(iretail.ma$Date)
  
  temp.AIC.list <- NA; temp.BIC.list <- NA;
  for(i.coefs in 1:months.to.analyse){
    temp.fit <- lm(Retail.Delta ~.,data = as.data.frame(total.table[,1:(i.coefs+1)]))
    temp.AIC <- AIC(temp.fit); temp.BIC <- BIC(temp.fit)
    temp.AIC.list <- c(temp.AIC.list,temp.AIC); temp.BIC.list <- c(temp.BIC.list,temp.BIC)
  }
  
  best.bic.lags <- which(temp.BIC.list ==min(na.omit(temp.BIC.list)))-1
  best.aic.lags <- which(temp.AIC.list ==min(na.omit(temp.AIC.list)))-1
  
  #Find optimum number of lags
  #par(mfrow=c(1,1))
  #plot(temp.AIC.list[-1], type = 'l',ylim=c(min(na.omit(temp.AIC.list),na.omit(temp.BIC.list)),
  #                                          max(na.omit(temp.AIC.list),na.omit(temp.BIC.list)))); lines(temp.BIC.list[-1],type='l',col="red")
  
  temp.fit <- lm(Retail.Delta ~.,data = as.data.frame(total.table))
  summary(temp.fit)
  
  temp.fit.r2 <- as.numeric(summary(temp.fit)["adj.r.squared"])
  temp.coefs <- temp.fit$coefficients
  
  temp.AIC.fit <- lm(Retail.Delta ~.,data = as.data.frame(total.table[,1:(best.aic.lags+1)]))
  best.aic.r2 <- summary(temp.AIC.fit)["adj.r.squared"]
  temp.BIC.fit <- lm(Retail.Delta ~.,data = as.data.frame(total.table[,1:(best.bic.lags+1)]))
  best.bic.r2 <- summary(temp.BIC.fit)["adj.r.squared"]

  #Calculate what would happen with AIC choice of parameters
  temp.AIC.Results <- data.frame(Retail.MA = imonth.retail, Retail.R2 = temp.fit.r2,Best.AIC.Num.Lags = best.aic.lags, 
                                 Best.AIC.R2 = best.aic.r2 , Best.BIC.Num.Lags = best.bic.lags,Best.BIC.R2 = best.bic.r2)
  IRF.AIC.Results <- rbind(IRF.AIC.Results, temp.AIC.Results)
  
  temp.list <- list(Retail.MA = imonth.retail, Retail.R2 = temp.fit.r2, 
                    Cumulative.Coefs = cumsum(as.numeric(temp.coefs[2:(months.to.analyse+1)])), 
                    Coefs = as.numeric(temp.coefs[2:(months.to.analyse+1)]))
  IRF.Results <- rbind(IRF.Results, temp.list)  
}

#Reformat the IRF to be a flat table for charts
Temp.IRF.Results.Flat <- matrix(unlist(IRF.Results), nrow = months.to.analyse)[,c(-1,-2)];
Temp.Cum <- t(Temp.IRF.Results.Flat[,1:months.to.analyse])
Temp.Coef <- t(Temp.IRF.Results.Flat[,(months.to.analyse+1):(months.to.analyse*2)])
IRF.Results.Flat <- cbind(matrix(unlist(IRF.Results),nrow=months.to.analyse)[,1:2],Temp.Cum,Temp.Coef)
colnames(IRF.Results.Flat) <- c("Retail.MA","R2",paste0(rep("Cum.Coef.",months.to.analyse),
                                                        seq(0:(months.to.analyse-1))),paste0(rep("Coef.",months.to.analyse),
                                                                                            seq(0:(months.to.analyse-1))))
head(IRF.Results.Flat)
#Write out Results
write.csv(IRF.Results.Flat, file="IRF.Results.Retail.MA.csv")
write.csv(IRF.AIC.Results, file="IRF.AIC.Results.csv")

#Plot the Cumulative Response functions
par(mfrow=c(4,4))
for(i.temp.period in 5:12){#Displaying  only 8 periods - more doesn't fit in PLOT()
  
  cum.coefs <- IRF.Results.Flat[i.temp.period,3:(months.to.analyse+2)]
  ind.coefs <- IRF.Results.Flat[i.temp.period,(months.to.analyse+3):(months.to.analyse*2+2)]
  temp.r2 <- IRF.Results.Flat[i.temp.period,2]
  
  barplot(cum.coefs,names = 1:months.to.analyse,main=paste0(i.temp.period," months MA Cum.Coef R2: ",round(temp.r2,2)),  col="red",ylab="Cum.Coef",xlab="Period")
  barplot(ind.coefs,names = 1:months.to.analyse,main=paste0(i.temp.period," months Indiv. Coef R2:",round(temp.r2,2)), col="blue")
}


#Now plot what AIC and BIC results tell you
par(mfrow=c(1,2))
barplot(IRF.AIC.Results$Best.AIC.Num.Lags, names = 1:months.to.analyse,main="AIC:#lags to include vs # months MA of Retail");
abline(0,1)
barplot(IRF.AIC.Results$Best.BIC.Num.Lags, names = 1:months.to.analyse,main="BIC:#lags to include vs # months MA of Retail")
abline(0,1)

#Plot the R2 and how they change
par(mfrow=c(1,2))
plot(IRF.AIC.Results$Retail.R2,main="R2 vs #lags",type = "l",lwd=2, ylim = c(-0.1,0.5), xlab = "Months Lags", ylab="R2")
grid(NULL,NULL,lwd=2, col="grey")
abline(h=0)
lines(IRF.AIC.Results$adj.r.squared, col="red",lwd=2)
lines(IRF.AIC.Results$adj.r.squared.1, col="blue",lwd=2)
legend("topleft",c("Non-Adjusted","AIC","BIC"),col=c("black","red","blue"),lwd=2)
#diff(IRF.AIC.Results$Retail.R2)

#And get differences
plot(diff(IRF.AIC.Results$Retail.R2),main="R2 vs #lags",type = "l",lwd=2, ylim = c(-0.3,0.3), xlab="Months Lags",ylab="Change in R2")
grid(NULL,NULL,lwd=2, col="grey")
lines(diff(IRF.AIC.Results$adj.r.squared), col="red",lwd=2)
lines(diff(IRF.AIC.Results$adj.r.squared.1), col="blue",lwd=2)
abline(h=0)
legend("topleft",c("Non-Adjusted","AIC","BIC"),col=c("black","red","blue"),lwd=2)


par(mfrow=c(1,2))
boxplot(IRF.Results.Flat[,3:(months.to.analyse+2)]/IRF.Results.Flat[,1] ~IRF.Results.Flat[,1], main = "Cumulative Coefficient per Period")
boxplot(IRF.Results.Flat[,(months.to.analyse+3):(months.to.analyse*2+2)]/IRF.Results.Flat[,1] ~IRF.Results.Flat[,1], main = "Coefficient vs Number of Lags")



#######