##############Functions to Use
setwd("C:/Users/Maxim/Documents/University/MBA/FXRetail")
source("PPP-Functions-Error.R")

#######################################################

all.data <- Read.All.Data() #get all data
#####Specify Country to Use
icount <- "JAP"   #choice of AUS BRA CHI GER JAP SOA UK
country.data <- Get.Country.Data(all.data,icount)

i.retail <- as.data.frame(country.data[2]); i.ppp <- as.data.frame(country.data[1])
######
#See what data series looks like
plot(i.retail$Date,i.retail$Value)

retail.ppp <- Merge.Retail.And.EPPP(i.retail, i.ppp)

#Get calculation of how the Eppp and Retail Sales have shifted by 1 to 12 months in time
retail.ppp <- PPP.and.Retail.Calculate.MA(retail.ppp,12)

#Take out Dates < 2016-06 (i.e. Brexit)
take.out.brexit <- TRUE
if(take.out.brexit){
  retail.ppp <- retail.ppp[retail.ppp$Date < as.Date("2016-06-01"),]
}

MA.regression.results <- data.frame(Eppp.Lag = double(), Retail.Lag = double(), Intercept.Coef = double(), Eppp.Coef = double(), Eppp.R2 = double())

#Now Regress the MAs
for(imonth.retail in 1:12){
  for(imonth.eppp in 1:12){
    
    ippp.ma <- retail.ppp[,paste0("Eppp.MA.",imonth.eppp)]    
    iretail.ma <- retail.ppp[,paste0("Retail.MA.",imonth.retail)]    
    
    ma.fit <- lm(iretail.ma ~ ippp.ma)
    
    result.fit <- data.frame(Eppp.Lag = imonth.eppp, Retail.Lag = imonth.retail, Intercept.Coef = as.numeric(ma.fit$coefficients[1]),
                             Eppp.Coef = as.numeric(ma.fit$coefficients[2]), Eppp.R2 = as.numeric(summary(ma.fit)["adj.r.squared"]))
    
    MA.regression.results <- rbind(MA.regression.results, result.fit)
  }
}
#Chart the Beta and Coefficient
par(mfrow=c(2,2))
boxplot(MA.regression.results$Eppp.Coef, main="Distribution of Beta of EPPP on Retail Sales MA")
plot(density(MA.regression.results$Eppp.Coef), main= "Density of Beta of EPPP on Retail Sales MA")
boxplot(MA.regression.results$Eppp.R2 ~ MA.regression.results$Eppp.Lag, main = "R2 vs Number of Months Lagged for EPPP")
boxplot(MA.regression.results$Eppp.R2 ~MA.regression.results$Retail.Lag, main = "R2 vs Number of Months Lagged for Retail.Lag")

library(reshape2)
MA.regr.reshape <- dcast(MA.regression.results, Retail.Lag ~ Eppp.Lag, value.var = "Eppp.R2")
head(MA.regr.reshape)

#Determine which is the best lag
MA.regr.reshape$BestLag <- NA
MA.regr.reshape$DifLag <- NA
for(ilag in 1:12){
  temp.max.r2 <- max(MA.regr.reshape[1:12,ilag+1])
  MA.regr.reshape[ilag,]$BestLag <- which(MA.regr.reshape[1:12,ilag+1]==temp.max.r2)
  MA.regr.reshape[ilag,]$DifLag <- which(MA.regr.reshape[1:12,ilag+1]==temp.max.r2) - ilag
}

mycols <- topo.colors(100,0.5)
#Plot the density
par(mfrow=c(1,1))
image(1:12,1:12,as.matrix(MA.regr.reshape[,2:13]),col=mycols,main="Moving Average R2 Performance - EPPP & Retail",xlab="Retail.Log", ylab="Eppp.Log")
contour(1:12,1:12,as.matrix(MA.regr.reshape[,2:13]),add=T)
lines(MA.regr.reshape$BestLag,1:12,type="l",col="red",lwd=2)

barplot(MA.regr.reshape$DifLag, xlab = "Retail MA Period Considered", ylab="Additional EPPP Periods", main="Lags to consider relative to Retail MA")

########Construct the Impulse Response Function for Retail MA

IRF.Results <- list(Retail.MA = double(), Retail.R2 = double(), Cumulative.Coefs = double(), Coefs = double())
IRF.AIC.Results <- data.frame(Retail.MA = double(), Retail.R2 = double(), Best.AIC.Num.Lags = double(), Best.AIC.R2 = double(), Best.BIC.Num.Lags = double(), Best.BIC.R2 = double())

for(imonth.retail in 1:12){
  #Get the Retail for that Period
  ippp.ma <- retail.ppp[,paste0("Eppp.MA.1")]    
  
  ippp.mas <- NA
  for(i.period in 1:12){    ippp.temp <- c(rep(NA,i.period-1),ippp.ma[1:(length(ippp.ma)-i.period+1)])
    ippp.mas <- cbind(ippp.mas,ippp.temp)   }
  ippp.mas <- ippp.mas[,-1]
  colnames(ippp.mas) <- paste0(rep("Eppp.Delta.M",12),seq(0,11,1))
  head(ippp.mas)
  
  iretail.ma <- retail.ppp[,c("Date",paste0("Retail.MA.",imonth.retail))]    
  head(iretail.ma)
  
  #Now bind table for total
  total.table <- cbind(Retail.Delta = iretail.ma[,2], ippp.mas)
  rownames(total.table) <- as.Date(iretail.ma$Date)
  
  temp.AIC.list <- NA; temp.BIC.list <- NA;
  for(i.coefs in 1:12){
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
  
  temp.list <- list(Retail.MA = imonth.retail, Retail.R2 = temp.fit.r2, Cumulative.Coefs = cumsum(as.numeric(temp.coefs[2:13])), Coefs = as.numeric(temp.coefs[2:13]))
  IRF.Results <- rbind(IRF.Results, temp.list)  
}

#Reformat the IRF to be a flat table for charts
Temp.IRF.Results.Flat <- matrix(unlist(IRF.Results), nrow = 12)[,c(-1,-2)];
Temp.Cum <- t(Temp.IRF.Results.Flat[,1:12])
Temp.Coef <- t(Temp.IRF.Results.Flat[,13:24])
IRF.Results.Flat <- cbind(matrix(unlist(IRF.Results),nrow=12)[,1:2],Temp.Cum,Temp.Coef)
colnames(IRF.Results.Flat) <- c("Retail.MA","R2",paste0(rep("Cum.Coef.",12),seq(0:11)),paste0(rep("Coef.",12),seq(0:11)))
head(IRF.Results.Flat)
#Write out Results
write.csv(IRF.Results.Flat, file="IRF.Results.Retail.MA.csv")
write.csv(IRF.AIC.Results, file="IRF.AIC.Results.csv")

#Plot the Cumulative Response functions
par(mfrow=c(4,4))
for(i.temp.period in 5:12){
  
  cum.coefs <- IRF.Results.Flat[i.temp.period,3:14]/i.temp.period
  ind.coefs <- IRF.Results.Flat[i.temp.period,15:26]/i.temp.period
  temp.r2 <- IRF.Results.Flat[i.temp.period,2]
  
  barplot(cum.coefs,names = 1:12,main=paste0(i.temp.period," months MA Cum.Coef R2: ",round(temp.r2,2)),  col="red",ylab="Cum.Coef",xlab="Period")
  barplot(ind.coefs,names = 1:12,main=paste0(i.temp.period," months Indiv. Coef R2:",round(temp.r2,2)), col="blue")
}


#Now plot what AIC and BIC results tell you
par(mfrow=c(1,2))
barplot(IRF.AIC.Results$Best.AIC.Num.Lags, names = 1:12,main="AIC:#lags to include vs # months MA of Retail");
abline(0,1)
barplot(IRF.AIC.Results$Best.BIC.Num.Lags, names = 1:12,main="BIC:#lags to include vs # months MA of Retail")
abline(0,1)

#Plot the R2 and how they change
par(mfrow=c(1,2))
plot(IRF.AIC.Results$Retail.R2,main="R2 vs #lags",type = "l",lwd=2)
lines(IRF.AIC.Results$adj.r.squared, col="red",lwd=2)
lines(IRF.AIC.Results$adj.r.squared.1, col="blue",lwd=2)
legend("topleft",c("Non-Adjusted","AIC","BIC"),col=c("black","red","blue"),lwd=2)
cumdif(IRF.AIC.Results$Retail.R2)

#And get differences
plot(diff(IRF.AIC.Results$Retail.R2),main="R2 vs #lags",type = "l",lwd=2, ylim = c(-0.1,0.4))
lines(diff(IRF.AIC.Results$adj.r.squared), col="red",lwd=2)
lines(diff(IRF.AIC.Results$adj.r.squared.1), col="blue",lwd=2)
abline(h=0)
legend("topleft",c("Non-Adjusted","AIC","BIC"),col=c("black","red","blue"),lwd=2)


par(mfrow=c(1,2))
boxplot(IRF.Results.Flat[,3:14]/IRF.Results.Flat[,1] ~IRF.Results.Flat[,1], main = "Cumulative Coefficient per Period")
boxplot(IRF.Results.Flat[,15:26]/IRF.Results.Flat[,1] ~IRF.Results.Flat[,1], main = "Coefficient vs Number of Lags")



#######