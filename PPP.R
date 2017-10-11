##############Functions to Use
setwd("C:/Users/Maxim/Documents/University/MBA/FXRetail")
source("PPP-Functions.R")

#######################################################

all.data <- Read.All.Data() #get all data
#####Specify Country to Use
icount <- "UK"
country.data <- Get.Country.Data(all.data,icount)

i.retail <- as.data.frame(country.data[2]); i.ppp <- as.data.frame(country.data[1])
######
#See what data series looks like
plot(i.retail$Date,i.retail$Value)

retail.ppp <- Merge.Retail.And.EPPP(i.retail, i.ppp)

#Get calculation of how the Eppp and Retail Sales have shifted by 1 to 12 months in time
retail.ppp <- PPP.and.Retail.Calculate.MA(retail.ppp,12)

MA.regression.results <- data.frame(Eppp.Lag = double(), Retail.Lag = double(), Intercept.Coef = double(), Eppp.Coef = double(), Eppp.R2 = double())

#Now Regress the MAs
for(imonth.retail in 1:12){
  for(imonth.eppp in 1:12){
   
    ippp.ma <- retail.ppp[,paste0("Eppp.MA.",imonth.retail)]    
    iretail.ma <- retail.ppp[,paste0("Retail.MA.",imonth.eppp)]    
    
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
mycols <- topo.colors(100,0.5)
#Plot the density
par(mfrow=c(1,1))
image(1:12,1:12,as.matrix(MA.regr.reshape[,2:13]),col=mycols,main="Moving Average R2 Performance - EPPP & Retail",xlab="Retail.Log", ylab="Eppp.Log")
contour(1:12,1:12,as.matrix(MA.regr.reshape[,2:13]),add=T)
lines(MA.regr.reshape$BestLag,1:12,type="l",col="red",lwd=2)

#Determine which is the best lag
MA.regr.reshape$BestLag <- NA
MA.regr.reshape$DifLag <- NA
for(ilag in 1:12){
  temp.max.r2 <- max(MA.regr.reshape[1:12,ilag+1])
  MA.regr.reshape[ilag,]$BestLag <- which(MA.regr.reshape[1:12,ilag+1]==temp.max.r2)
  MA.regr.reshape[ilag,]$DifLag <- which(MA.regr.reshape[1:12,ilag+1]==temp.max.r2) - ilag
}
barplot(MA.regr.reshape$DifLag, xlab = "Retail MA Period Considered", ylab="Additional EPPP Periods", main="Lags to consider relative to Retail MA")





#Chart what the Retail Distribution looks like
par(mfrow=c(3,3))
plot(i.retail.wlog$Date,i.retail.wlog$Value, main = paste0(icount,": Retail Monthly "),
     xlab = "Date",ylab="Retail Index", col="blue",type="l"); 

plot(i.retail.wlog$Date,i.retail.wlog$Retail.Log, main = paste0(icount,": Retail Monthly Log(Returns)"),
     xlab = "Date",ylab="Log-Return"); abline(h=0)
acf(na.omit(i.retail.wlog$Retail.Log), main = paste0("ACF of ",icount," Retail Monthly Log(Returns)"))

###Now PPP
library(reshape2)
i.ppp.recast <- recast(i.ppp, Date ~ Measure.Names, id.var = c("Date","Measure.Names"), measure.var = c("Value"), fun.aggregate=mean)
tail(i.ppp.recast)
i.ppp.recast$FX <- i.ppp.recast$Usdx / i.ppp.recast$Eppp

i.ppp.recast <- cbind(i.ppp.recast, FX.Log = c(NA, diff(log(i.ppp.recast$FX)))
                      , Usdx.Log = c(NA,diff(log(i.ppp.recast$Usdx))), Eppp.Log = c(NA,diff(log(i.ppp.recast$Eppp))))

tail(i.ppp.recast)

###Try PPP VAR
#Alt-L to fold #Alt-L to unfodl
var.ppp <- i.ppp.recast[colSums(!is.na(i.ppp.recast)) > 0]
var.ppp <- var.ppp[complete.cases(var.ppp),]
var.ppp$YearMonth <- NULL
var.ppp <- as.ts(var.ppp[,c("Eppp.Log","Usdx.Log")])
tail(var.ppp)

acf(var.ppp)
library(vars)
library(VAR.etp)
VARselect(var.ppp, lag.max=100, type= "both")$selection
#Normal VAR
VAR.ppp <- VAR(var.ppp, p=8, type = "none")
summary(VAR.ppp)
summary(VAR.ppp$varresult$Eppp.Log)["adj.r.squared"]
irf(VAR.ppp)
serial.test(VAR.ppp,lags.pt=10,type = "PT.asymptotic")

#Create the lags for the Bootstrapping
var.ppp.boot <- VAR.boot.create(var.ppp, 8,"Eppp","Usdx")
head(var.ppp.boot)
#Calculate the Bootstrap R2
results <- boot(data = var.ppp.boot, statistic = VAR.boot.estimate, R=200, stype = "i")
plot(results, index = 1)

#Plot the VAR Function
summary(VAR.ppp)
fcast.ppp <- forecast(VAR.ppp)
plot(fcast.ppp, xlim = c(2200,2500), xaxt='n', main = "VAR(8) Daily USDX PPP Model")
axis(1,at=2200:2500,labels=i.ppp.recast[2200:2500,]$Date)



############################## DAILY LOG RETURNS
#Daily Log-Returns
plot(i.ppp.recast$Date,i.ppp.recast$Eppp, main = paste0(icount,": EPPP Daily"), col="blue",type="l")

plot(i.ppp.recast$Date,i.ppp.recast$Eppp.Log, main = paste0(icount,": EPPP Daily Log(Returns)")
     ,xlab="Date",ylab="Log-Return"); abline(h=0)
acf(na.omit(i.ppp.recast$Eppp.Log), main = paste0("ACF of ",icount," EPPP Daily Log(Returns)"))

############################## MONTHLY CALCULATIONS
#Get Monthly Data
i.ppp.recast$YearMonth <- format(i.ppp.recast$Date,"%Y-%m") #Determine Year and Month of the Daily PPP Data
i.ppp.month <- melt(i.ppp.recast, id.vars = "YearMonth", fun=mean, measure.vars = c("Eppp","Usdx","FX","FX.Log","Usdx.Log","Eppp.Log"))
i.ppp.month <- recast(i.ppp.month, YearMonth ~variable, id.var=c("YearMonth","variable"), measure.var = "value", fun.aggregate=mean)

#Plot Monthy Returns of EPPP
plot(i.ppp.month$Eppp, type = "l",col="blue", main=paste0("Monthly EPPP of ",icount), xaxt='n')
axis(1,at = index(i.ppp.month$Eppp),labels=i.ppp.month$YearMonth)

plot( i.ppp.month$Eppp, type = "l", col="blue", main=paste0("Monthly Log Return(EPPP) of ",icount), xaxt='n'); abline(h=1);
axis(1,at = index(i.ppp.month$Eppp),labels=i.ppp.month$YearMonth)
acf(na.omit(i.ppp.month$Eppp), main = paste0("ACF of ",icount, " Monthly EPP Log(Returns)"))
  
##Merge the Monthly Data with Retail Data
i.retail.wlog$YearMonth <- format(i.retail.wlog$Date,"%Y-%m")


###GET MONTHLY COMBINATION
i.comb <- merge(i.retail.wlog, i.ppp.month,by="YearMonth")
i.comb$Eppp.MOM <- c(NA,diff(log(i.comb$Eppp)))

head(i.comb)
#Clean it so we have complete cases
i.comb <- i.comb[colSums(!is.na(i.comb)) > 0]
i.comb <- i.comb[complete.cases(i.comb),]

par(mfrow=c(1,1))
#Chart of just the time series
plot( i.comb$Retail.Log, type = "l", col="blue", main=paste0("Monthly Log Return(Retail) of ",icount), xaxt='n'); abline(h=0);
lines(i.comb$Eppp.MOM, type = 'l', col="red")
axis(1,at = index(i.comb$Retail.Log),labels=i.comb$YearMonth)
legend("bottomleft",c("Retail Log-Return","ePPP Log-Return"),lty=1, col = c("blue","red"))

library(PerformanceAnalytics)
chart.Correlation(i.comb[c("Eppp.MOM","Retail.Log")],pch=21)

ccf(i.comb$Retail.Log, i.comb$Eppp.MOM, main = "CCF of Retail & Eppp MoM ",icount)

##GET DAILY COMBINATION
i.comm.daily <- merge(i.ppp.recast,i.retail.wlog,by="YearMonth")
tail(i.comm.daily)


plot(i.comm.daily$Date.x,i.comm.daily$Eppp,main="Epp Daily")
lines(i.comm.daily$Date.x,i.comm.daily$Value/100)

plot(i.comm.daily$Date.y,i.comm.daily$Value, type = "l", col="red")
lines(i.retail.wlog$Date,i.retail.wlog$Value, type = "l", col="blue")

#Plot the Chart of FX vs True PPP
plot(i.ppp.recast$Date, i.ppp.recast$FX, main=paste0("PPP/USDx of",icount) 
     , ylim = c(min(na.omit(i.ppp$Value)),max(na.omit(i.ppp$Value))),
     type = "l",col="blue")
lines(i.ppp.recast$Date,i.ppp.recast$Usdx, col = "red")
legend("bottomleft",c("FX","True PPP"),lty=1, col = c("red","blue"))


######## vAR MODEL FOR RETAIL

ppp.month <- as.ts(data.frame(Retail.Log =  i.comb$Retail.Log, Eppp.MOM = i.comb$Eppp.MOM))
acf(ppp.month)

#Select how many lags you want to use
VARselect(as.matrix(ppp.month), lag.max=10, type= "const")$selection
VAR.ppp <- VAR(as.matrix(ppp.month), p=1, type = "const")
serial.test(VAR.ppp,lags.pt=10,type = "PT.asymptotic")
summary(VAR.ppp)
fcast.ppp <- forecast(VAR.ppp)
plot(fcast.ppp)

#create table for boot
ppp.boot <- VAR.boot.create(ppp.month,1,"Retail.Log","Eppp.MOM")
head(ppp.boot)
ppp.boot.result <- boot(data = ppp.boot, statistic = VAR.boot.estimate, R=200, stype = "i")
plot(ppp.boot.result, index = 2)
irf(VAR.ppp)

##NEURAL NETWORK
ind = sample(2,nrow(ppp.boot), replace = TRUE, prob=c(0.7,0.3))
ppp.train = ppp.boot[ind==1,]
ppp.test = ppp.boot[ind==2,]
library(neuralnet)

#picking number of hidden layers based on Ns (observations)/(alpha * (No of Input + Number of Output))= 53/(5*(9+1))
nn.model  <- neuralnet(Retail.Log.0 ~ Retail.Log.1 + Retail.Log.2 + Retail.Log.3 + Retail.Log.4
                       + Eppp.MOM.0 + Eppp.MOM.1+Eppp.MOM.2+Eppp.MOM.3+Eppp.MOM.4,
                            data = ppp.train, hidden = c(5,3))
plot(nn.model)
nn.results <- compute(nn.model, ppp.test[2:10])
nn.predict <- nn.results$net.result
plot(nn.predict, ppp.test$Retail.Log.0)
MAE(nn.predict, ppp.test$Retail.Log.0)
cor(nn.predict, ppp.test$Retail.Log.0)
#Plot the fit
plot(nn.predict, ppp.test$Retail.Log.0,col="blue",pch=18,cex=0.7); abline(0,1,lwd=2)
r2.nn <- rSquared(nn.predict, ppp.test$Retail.Log.0 - nn.predict)

##GAM
library(mgcv)
gam.fit <- gam(Retail.Log.0 ~ Eppp.MOM.0+Retail.Log.1 + Retail.Log.2 + Retail.Log.3 + Eppp.MOM.1 + Eppp.MOM.2 + Eppp.MOM.3,data = ppp.boot)
gam.lag1 <- gam(Retail.Log.0 ~ Eppp.MOM.1, data = ppp.boot)
plot(gam.lag1)

vis.gam(gam.fit)

plot(gam.fit)
#Diagnosing a GAM model
#generate diagnostic plot using gam.check of fitted model
par(mfrow=c(2,2))
gam.check(gam.fit)

#RIDGE REGRESSION/LASSO

#Specify how many lags you want to use
ppp.boot <- VAR.boot.create(ppp.month,4,"Retail.Log","Eppp.MOM")
tail(ppp.boot)
library(glmnet)
#Get the LASSO FIT taking out the first value of RETAIL LOG and EPPP
lin <- glmnet(as.matrix(ppp.boot[,c(-1,-2)]),as.matrix(ppp.boot[,2]))
par(mfrow=c(1,1))
plot(lin, main = paste("Coefficients LASSO for:",icount));   vnames = colnames(ppp.boot[,-c(1,2)])
#Coefficients of the fit
vnat = coef(lin); vnat = vnat[-1,ncol(vnat)] #remove the intercept and get coefficients at end of path
axis(4,at=vnat, line = -.5, label = vnames, tick = T, cex.axis = 0.6, lty = 2)

#Get the X-Validated value with lowest lambda
cv.lin <- cv.glmnet(as.matrix(ppp.boot[,c(3,4)]), as.matrix(ppp.boot[,2]), nfolds = 10)

#Get the fitted values
LASSO.FIT = as.numeric(predict(cv.lin,as.matrix(ppp.boot[,c(3,4)]),s="lambda.min"))
plot(cv.lin); cv.lin$lambda.min
lin.prediction <- predict(cv.lin, as.matrix(ppp.boot[,c(3,4)]), s= "lambda.min")

#Out of Sample R2
require(miscTools)
#r2 <- rSquared(ytest, resid = ytest-yhat)
r2.lasso <- rSquared(ppp.boot[,2], resid = ppp.boot[,2]-lin.prediction)

#SVM


##Regression Trees
library(RWeka)
fit.tree <- M5P(Retail.Log.0 ~ Retail.Log.1 + Eppp.MOM.0 + Eppp.MOM.1,data=ppp.boot)
fit.tree; summary(fit.tree); plot(fit.tree)
predict.tree <- predict(fit.tree, ppp.boot)
r2.tree <- rSquared(ppp.boot$Retail.Log.0,resid= ppp.boot$Retail.Log.0- predict.tree)

##

###

##### Different VAR Boot Method
#bootstrapped VAR coefficients <- different method
VAR.ppp.boot <- VAR.Boot(var.ppp, p=8, nb=200,type = "const")
VAR.ppp.boot$coef
#Get Impulse Response function
VAR.ppp.est <- VAR.est(var.ppp, p = 8, type = "const")$coef
VAR.Fore(var.ppp,VAR.ppp.est, p = 8,h=10, type = "const")
