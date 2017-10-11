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

MA.regression.results <- data.frame(Eppp.Lag = double(), Retail.Lag = double(),
                                    Eppp.Additional.Lag = double(), Intercept.Coef = double(), 
                                    Eppp.Coef = double(), Eppp.R2 = double())


for(imonth.retail in 1:12){
  for(imonth.eppp in 1:12){
    
    retail.ppp.temp <- PPP.and.Retail.Calculate.Lags(retail.ppp,12,imonth.eppp)
    
    for(ilag in 0:12){
      
      ippp.ma <- retail.ppp.temp[,paste0("Eppp.MA.",imonth.eppp,".Lag.",ilag)]    
      iretail.ma <- retail.ppp.temp[,paste0("Retail.MA.",imonth.retail)]    
      
      ma.fit <- lm(iretail.ma ~ ippp.ma)
      
      result.fit <- data.frame(Eppp.Lag = imonth.eppp, Retail.Lag = imonth.retail, 
                               Eppp.Additional.Lag = ilag,
                               Intercept.Coef = as.numeric(ma.fit$coefficients[1]),
                               Eppp.Coef = as.numeric(ma.fit$coefficients[2]), Eppp.R2 = as.numeric(summary(ma.fit)["adj.r.squared"]))
      
      MA.regression.results <- rbind(MA.regression.results, result.fit)  
    }
  }
}
head(MA.regression.results)

#Chart the Beta and Coefficient
par(mfrow=c(2,2))
boxplot(MA.regression.results$Eppp.Coef, main="Distribution of Beta of EPPP on Retail Sales MA")
plot(density(MA.regression.results$Eppp.Coef), main= "Density of Beta of EPPP on Retail Sales MA")
boxplot(MA.regression.results$Eppp.R2 ~ MA.regression.results$Eppp.Lag, main = "R2 vs Number of Months Lagged for EPPP")
boxplot(MA.regression.results$Eppp.R2 ~MA.regression.results$Retail.Lag, main = "R2 vs Number of Months Lagged for Retail.Lag")

library(reshape2)
par(mfrow=c(3,3))
for(i.eppp.lag in 0:8){
  MA.regression.results.subset <- MA.regression.results[MA.regression.results$Eppp.Additional.Lag==i.eppp.lag,]
  MA.regression.best.R2 <- max(MA.regression.results.subset$Eppp.R2)
  index.temp.r2 <- which(MA.regression.results.subset$Eppp.R2==MA.regression.best.R2)
  best.temp.retail.month <- MA.regression.results.subset[index.temp.r2,"Retail.Lag"]
  best.temp.eppp.month <- MA.regression.results.subset[index.temp.r2,"Eppp.Lag"]
  MA.regr.reshape <- dcast(MA.regression.results.subset, Retail.Lag ~ Eppp.Lag, value.var = "Eppp.R2")
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
  image(1:12,1:12,as.matrix(MA.regr.reshape[,2:13]),col=mycols,main=paste0("R2 with EPPP Lag:",i.eppp.lag, " Best: ",round(MA.regression.best.R2,2)),xlab="Retail.Log", ylab="Eppp.Log")
  contour(1:12,1:12,as.matrix(MA.regr.reshape[,2:13]),add=T)
  lines(MA.regr.reshape$BestLag,1:12,type="l",col="red",lwd=2)
}
#barplot(MA.regr.reshape$DifLag, xlab = "Retail MA Period Considered", ylab="Additional EPPP Periods", main="Lags to consider relative to Retail MA")
