##############Functions to Use
setwd("C:/Users/Maxim/Documents/University/MBA/FXRetail")
source("PPP-Functions.R")

#######################################################

all.data <- Read.All.Data() #get all data
#####Specify Country to Use
icount <- "UK"

months.to.analyse <- 12

country.data <- Get.Country.Data(all.data,icount)
usa.country.data <- Get.Country.Data(all.data,"USA")

i.retail <- as.data.frame(country.data[2]); i.ppp <- as.data.frame(country.data[1]); usa.retail <- as.data.frame(usa.country.data[2])
######
#See what data series looks like
plot(i.retail$Date,i.retail$Value)

retail.ppp <- Merge.Retail.And.EPPP(i.retail, i.ppp)
#Get USA Retail Data
usa.retail.ppp <- Merge.Retail.And.EPPP(usa.retail, i.ppp)
#Change USA data to End of MOnth
library(timeDate)
usa.retail.ppp$Date <- as.Date(timeLastDayInMonth(usa.retail.ppp$Date))
country.plus.usa.retail.ppp <- merge(retail.ppp, usa.retail.ppp, by="Date")
country.plus.usa.retail.ppp$Retail.vs.USA.Dif <- country.plus.usa.retail.ppp[,"Retail.Log.x"] - country.plus.usa.retail.ppp[,"Retail.Log.y"]
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
retail.ppp <- Factors.Calculate.Lags(as.data.frame(country.plus.usa.ppp),months.to.analyse,c("Retail.Log","PPP.Log","Retail.vs.USA.Dif","Us.Log","Eppp.Log")) #optionally include USDX log
#retail.ppp <- Factors.Calculate.Lags(retail.ppp,months.to.analyse,c("PPP.Log","Retail.Log","Us.Log","Eppp.Log")) #optionally include USDX log
usa.retail.ppp <- Factors.Calculate.Lags(usa.retail.ppp,months.to.analyse,c("PPP.Log","Retail.Log"))
#plot(retail.ppp$Date, retail.ppp$PPP, type="l",col="blue",main="PPP for UK", xlab = "Date",ylab="PPP value");grid(NULL,NULL,col="grey")

#Take out Dates < 2016-06 (i.e. Brexit)
take.out.brexit <- TRUE
if(take.out.brexit){
  retail.ppp <- retail.ppp[retail.ppp$Date < as.Date("2016-06-01"),]
}


#Determine which cols to use
retail.ppp.usdx.cols <- str_detect(colnames(retail.ppp),c("Us\\.Log\\.Lag.|PPP\\.Log\\.Lag\\.")); #paste0(colnames(retail.ppp),"-",str_detect(colnames(retail.ppp),c("Us\\.|PPP\\.")));
#retail.ppp.usdx.cols <- 18:56
retail.ppp.cols <- str_detect(colnames(retail.ppp),c("PPP\\.Log\\.Lag\\.")) #18:43
retail.eppp.usdx.cols <- str_detect(colnames(retail.ppp),c("Eppp\\.Log\\.Lag\\.|Us\\.Log\\.Lag")) #c(31:56,57:69) #31:56, 57:60
retail.eppp.usdx.dummy.cols <- str_detect(colnames(retail.ppp),c("Eppp\\.Log\\.Lag\\.|Us\\.Log\\.Lag|Dummy")) #c(31:56,57:69) #31:56, 57:60
retail.ppp.dummy.cols <- str_detect(colnames(retail.ppp),c("PPP\\.Log\\.Lag\\.|Dummy")) #c(18:43,70:80)
retail.ppp.usdx.dummy.cols <- str_detect(colnames(retail.ppp),c("PPP\\.Log\\.Lag\\.|Dummy|Us\\.Log\\.Lag"))


colnames(retail.ppp); 
retail.ppp.fit <- lm(retail.ppp$Retail.Log.Lag.0~., data = retail.ppp[,retail.ppp.cols]);
retail.ppp.dummy.fit <-  lm(retail.ppp$Retail.Log.Lag.0~., data = retail.ppp[,retail.ppp.dummy.cols]);
retail.ppp.usdx.fit <- lm(retail.ppp$Retail.Log.Lag.0~., data = retail.ppp[,retail.ppp.usdx.cols]);
retail.ppp.usdx.dummy.fit <- lm(retail.ppp$Retail.Log.Lag.0~., data = retail.ppp[,retail.ppp.usdx.dummy.cols]);
retail.eppp.usdx.fit <- lm(retail.ppp$Retail.Log.Lag.0~., data = retail.ppp[,retail.eppp.usdx.cols]);
retail.eppp.usdx.dummy.fit <- lm(retail.ppp$Retail.Log.Lag.0~., data = retail.ppp[,retail.eppp.usdx.dummy.cols]);


summary(retail.ppp.fit);
summary(retail.ppp.dummy.fit); anova(retail.ppp.dummy.fit);
summary(retail.ppp.usdx.fit); anova(retail.ppp.usdx.fit);
summary(retail.ppp.usdx.dummy.fit); anova(retail.ppp.usdx.dummy.fit);
summary(retail.eppp.usdx.fit);
summary(retail.eppp.usdx.dummy.fit);anova(retail.eppp.usdx.dummy.fit);

###Now Compare Vs USA
usa.ppp.fit <- lm(retail.ppp$Retail.vs.USA.Dif.Lag.0~., data = retail.ppp[,retail.ppp.cols]);
usa.ppp.dummy.fit <-  lm(retail.ppp$Retail.vs.USA.Dif.Lag.0~., data = retail.ppp[,retail.ppp.dummy.cols]);
usa.ppp.usdx.fit <- lm(retail.ppp$Retail.vs.USA.Dif.Lag.0~., data = retail.ppp[,retail.ppp.usdx.cols]);
usa.ppp.usdx.dummy.fit <- lm(retail.ppp$Retail.vs.USA.Dif.Lag.0~., data = retail.ppp[,retail.ppp.usdx.dummy.cols]);
usa.eppp.usdx.fit <- lm(retail.ppp$Retail.vs.USA.Dif.Lag.0~., data = retail.ppp[,retail.eppp.usdx.cols]);
usa.eppp.usdx.dummy.fit <- lm(retail.ppp$Retail.vs.USA.Dif.Lag.0~., data = retail.ppp[,retail.eppp.usdx.dummy.cols]);


summary(usa.ppp.fit);
summary(usa.ppp.dummy.fit); anova(usa.ppp.dummy.fit);
summary(usa.ppp.usdx.fit); anova(usa.ppp.usdx.fit);
summary(usa.ppp.usdx.dummy.fit); anova(usa.ppp.usdx.dummy.fit);
summary(usa.eppp.usdx.fit);
summary(usa.eppp.usdx.dummy.fit);anova(usa.eppp.usdx.dummy.fit);


########################





usa.basic.fit <- lm(Retail.Log.Lag.0~., data = usa.retail.ppp[,retail.ppp.usdx.cols])
anova(basic.fit); anova(usa.basic.fit)

retail.lag.only.fit <- lm(Retail.Log.Lag.0 ~ Retail.Log.Lag.1, data = retail.ppp)
retail.lag.only.r2 <- summary(retail.lag.only.fit)["adj.r.squared"]
usa.retail.lag.only.fit <- lm(Retail.Log.Lag.0 ~ Retail.Log.Lag.1, data = usa.retail.ppp)
usa.retail.lag.only.r2 <- summary(usa.retail.lag.only.fit)["adj.r.squared"]

#Get Basic Fit Coefficients
basic.coefs <- basic.fit$coefficients
basic.coefs.usdx <- basic.fit.usdx$coefficients
library(stringr)
basic.coefs.ppp.only <- basic.coefs[str_detect(names(basic.coefs),"PPP")]
basic.coefs.ppp.usdx <- basic.coefs.usdx[str_detect(names(basic.coefs.usdx),"PPP")]

basic.coefs.usdx <- basic.coefs.usdx[str_detect(names(basic.coefs.usdx),"Usdx")]

#Plot Chart with
par(mfrow=c(1,2))
plot(cumsum(basic.coefs.ppp.only), main="Coefficient Comparison w/o USDX", xlab = "Lags",ylab="Cumulative Coefficient",type='l',
     lwd = 2, col= "blue")
abline(h=0)
lines(cumsum(basic.coefs.ppp.usdx), col = "red", lwd = 2)
legend("topleft", c("Without USDX","With USDX"), lwd = c(2,2), col = c("blue","red"))

#USDX Chart
plot(cumsum(basic.coefs.usdx), main="Cumsum Effect of USDX Change", xlab="Lags",ylab="Cumulative Coefficient",type='l',col="blue")
abline(h=0)

#Compare the R2

retail.and.ppp.fit <- lm(Retail.Log.Lag.0 ~ Retail.Log.Lag.1 + PPP.Log.Lag.3 + PPP.Log.Lag.4 + PPP.Log.Lag.5, data = retail.ppp)
usa.retail.and.ppp.fit <- lm(Retail.Log.Lag.0 ~ Retail.Log.Lag.1 + PPP.Log.Lag.3 + PPP.Log.Lag.4 + PPP.Log.Lag.5, data = usa.retail.ppp)

retail.and.ppp.r2 <- summary(retail.and.ppp.fit)["adj.r.squared"]
usa.retail.and.ppp.r2 <- summary(usa.retail.and.ppp.fit)["adj.r.squared"]

summary(basic.fit)


