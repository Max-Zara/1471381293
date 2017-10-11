
all.country.list <- c("CHI","BRA","AUS","UK","SOA","GER","JAP")
imp.exp.country.list <- c("CHI","JAP","UK","GER","USA") #BBG Data downloaded for UK
imp.exp.usa.list <- c("CHI","BRA","AUS","UK","SOA","GER","JAP","USA") ##All US Data applicable

Read.All.Data <- function(){
  
  ppp.all <- read.csv("PPP-20171010.csv") #read.csv("PPP-All.csv")
  ppp.all$Date <- as.Date(ppp.all$Date)
  colnames(ppp.all)[which(names(ppp.all)=="Measure.Values")] <- "Value" #Rename the Column to Value to Merge
  tail(ppp.all)
  unique(ppp.all$Country)
  
  retail.sales <- read.csv("RetailSales-20171010.csv")
  retail.sales$Date <- as.Date(retail.sales$Date)
  tail(retail.sales)
  
  currency.all <- read.csv("CurrencyAll.csv")
  currency.all$Date <- as.Date(currency.all$Date)
  
  importexport.all <- read.csv("ImportExportTradeRenamed.csv")
  importexport.all$Date <- as.Date(importexport.all$Date)
  
  all.data <- list(ppp.all,retail.sales,currency.all,importexport.all)
  return(all.data)
}

#Splits data into training and test sets
split.data.random <- function(data,p=0.7,s=666) {
  set.seed(s)
  index = sample(1:dim(data)[1])
  train <- data[index[1:floor(dim(data)[1]*p)],]
  test <- data[index[((ceiling(dim(data)[1]*p))+1):dim(data)[1]],]
  return(list(train=train,test=test))
}

split.data.ordered <- function(data,p=0.7) {
  index = (1:dim(data)[1])
  train <- data[index[1:floor(dim(data)[1]*p)],]
  test <- data[index[((ceiling(dim(data)[1]*p))+1):dim(data)[1]],]
  return(list(train=train,test=test))
}


Get.Country.Retail.Types <- function(all.data,icount){
  temp.all <- as.data.frame(all.data[2])
  temp.country <- temp.all[temp.all$Country==icount,]
  return(unique(temp.country$Type))
}

library(RcppRoll)


Get.Weighted.PPP <- function(trade.weights, icount, all.data){
  
  ppp.all <- as.data.frame(all.data[1])
  
  other.countries <- imp.exp.country.list[!(imp.exp.country.list  %in% c(icount,"USA"))]
  
  ppp.recast <- cast(ppp.all, Date ~ Measure.Names + Country, value="Value",fun.aggregate=mean)
  
  
  #FROM BEFORE: The EPPP is implied exchange rate from the prices. Ratio is PPP
  #According to OECD UK PPP is around 0.7 of US
  #retail.ppp$PPP <- retail.ppp$Eppp / retail.ppp$Usdx
  
  for(i.country in c(icount,other.countries)){
    
  }
}

Get.Trade.Weights <- function(all.data, icount, source.to.use = "BBG", method.to.use = "natural",to.Chart=FALSE){
  ppp.all <- as.data.frame(all.data[1]); retail.sales <- as.data.frame(all.data[2]); i.fx <- as.data.frame(all.data[3])
  importexport.all <- as.data.frame(all.data[4]);
  
  ImEx.Src <- importexport.all[importexport.all$Source==source.to.use,]
  unique(ImEx.Src[,c("From","To")])
  
  #Get the PPP Values Of the Country
  if(icount == "USA"){i.ppp <- ppp.all;}else{i.ppp <- ppp.all[ppp.all$Country == icount,]}
  #Of the Other Countries
  
  ##Get List of countries that this country trades with
  if(icount == "USA"){ImEx.ppp.others <- all.country.list }else{
  ImEx.ppp.others <- imp.exp.country.list[imp.exp.country.list != icount]}
  
  #Now restrict the PPP List to only countries in the trade list
  if(icount=="USA"){i.ppp.all <- ppp.all[ppp.all$Country %in% imp.exp.usa.list,]}else{
    i.ppp.all <- ppp.all[ppp.all$Country %in% imp.exp.country.list,]}
  
  i.eppp.all <- i.ppp.all[i.ppp.all$Measure.Names =="Eppp",]
  i.usdx.all <- i.ppp.all[i.ppp.all$Measure.Names =="Usdx",]
  
  eppp.recast <- cast(i.eppp.all,Date~Country, value="Value",fun.aggregate = mean)
  usdx.recast <- cast(i.eppp.all,Date~Country, value="Value",fun.aggregate = mean)
  
  tail(eppp.recast);tail(usdx.recast)
  
  weights.frame <- double()
  #Now Calculate the weighting
  for(i.country.trade in ImEx.ppp.others){
    #i.country.trade <- "USA"
    i.country.original <- i.country.trade
    print(i.country.original)
    if(i.country.trade == "GER" & source.to.use=="BBG"){i.country.trade <- "EU"}  
    temp.export <- ImEx.Src[ImEx.Src$From==icount & ImEx.Src$To == i.country.trade,]
    temp.import <- ImEx.Src[ImEx.Src$To==icount & ImEx.Src$From == i.country.trade,]
    temp.ex.im.merged <- merge(temp.export, temp.import, by="Date")
    head(temp.ex.im.merged)
    temp.ex.im.merged$Total <- temp.ex.im.merged$Value.x+temp.ex.im.merged$Value.y
    weights.frame <- rbind(weights.frame, data.frame(Date = temp.ex.im.merged$Date,Country=i.country.original,TradeTotal = temp.ex.im.merged$Total))
    print(i.country.original)
  }
  weights.recast <- cast(weights.frame, Date~Country, value="TradeTotal",fun.aggregate = mean)
  head(weights.recast)
  matplot(weights.recast[,2:ncol(weights.recast)],type='l',xaxt='n')
  legend("topleft",  bty = "n", legend=colnames(weights.recast)[2:ncol(weights.recast)],col=seq_len(4),pch = 1)
  axis(1,1:nrow(weights.recast),weights.recast$Date)
  
  ###Now do date expansion for all dates
  seq.all.dates <- data.frame(Date=seq(min(weights.recast$Date),max(weights.recast$Date),1))
  weights.all.dates <- merge(seq.all.dates,weights.recast,by="Date", all=TRUE)
  head(weights.all.dates)
  
  for(colindex in 2:ncol(weights.all.dates)){
    if(length(na.omit(weights.all.dates[,colindex]))>1){
      if(method.to.use %in% c("natural","periodic","fmm")){
        print(paste0("Working on Splines:",method.to.use))
        NAindex <- which(!is.na(weights.all.dates[,colindex]))
        ilength <- min(NAindex):max(NAindex)
        
        spline.replace <- spline(weights.all.dates[ilength,]$Date, weights.all.dates[ilength,colindex], n = length(ilength), method = method.to.use)$y 
        weights.all.dates$temp.col <- ifelse(is.na(weights.all.dates[ilength,colindex]),spline.replace,weights.all.dates[ilength,colindex])
        colnames(weights.all.dates)[ncol(weights.all.dates)] <- paste0(colnames(weights.all.dates)[colindex],method.to.use)
        
      }else if(is.numeric(method.to.use)){
        print(paste0("Working on Rolling Avg:",method.to.use))
        ##rolling avg
        rollNum <- roll_mean(weights.all.dates[,colindex],n=method.to.use,na.rm=TRUE)
        weights.all.dates$temp.col <- c(rep(rollNum[1],nrow(weights.all.dates)-length(rollNum)),rollNum)
        colnames(weights.all.dates)[ncol(weights.all.dates)] <- paste0(colnames(weights.all.dates)[colindex],"RollMA")
      }
    }
  }
  head(weights.all.dates)
  
  num.of.countries <- ncol(weights.recast)-1
  ##Now Get Weights
  weights.all.dates$Sum <- rowSums(weights.all.dates[,((num.of.countries+2):ncol(weights.all.dates))])
  
  #Now Get weight of each country
  for(i.country.weight in 1:num.of.countries){
    weights.all.dates[,(i.country.weight+1)] <- weights.all.dates[,(i.country.weight+num.of.countries+1)]/weights.all.dates$Sum
  }
  head(weights.all.dates)
  
  weights.all.dates <- weights.all.dates[,colnames(weights.all.dates) %in% c("Date",if(icount=="USA"){imp.exp.usa.list}else{imp.exp.country.list})]
  return(weights.all.dates)
  
  eppp.w.weights <- merge(eppp.recast,weights.all.dates,by="Date")
  usdx.w.weights <- merge(usdx.recast,weights.all.dates,by="Date")
  head(usdx.w.weights)
  print("Printing Tail of Weights")
  print(tail(usdx.w.weights))
}

Get.Country.Data <- function(all.data, icount, cross.currency="N/A", retail.type = "NA"){
  #Segment the PPP and Retail
  ppp.all <- as.data.frame(all.data[1]); retail.sales <- as.data.frame(all.data[2]); i.fx <- as.data.frame(all.data[3])
  
  i.ppp <- ppp.all[ppp.all$Country == icount,]
  i.retail <- retail.sales[retail.sales$Country == icount,]
  i.fx <- i.fx[i.fx$CurrencyCode==cross.currency,]
  
  if(retail.type == "NA"){
    #Get the Retail Sales Type
    i.type <- unique(i.retail$Type)[1]
    #Take the wanted index
    i.retail <- i.retail[i.retail$Type==i.type,]
  }else{
    i.retail <- i.retail[i.retail$Type==retail.type,]
  }
  
  country.list <- list(i.ppp, i.retail, i.fx)
  return(country.list)
}

Merge.Retail.And.EPPP.And.FX <- function(i.retail, i.ppp,i.fx){
  #Get Log Returns of Retail
  i.retail.wlog <- cbind(i.retail,Retail.Log = c(NA,diff(log(i.retail$Value))))
  colnames(i.fx)[which(names(i.fx)=="Value")] <- "FX"
  
  #Merge With USDX and EPPP Data
  i.ppp.usd <- i.ppp[i.ppp$Measure.Names == "Usdx",]
  i.ppp.eppp <- i.ppp[i.ppp$Measure.Names == "Eppp",]
  
  #retail.ppp <- merge(i.retail.wlog, i.ppp.usd, i.ppp.eppp, by = "Date")
  retail.ppp <- Reduce(function(...) merge(...,all=TRUE, by = "Date"), list(i.retail.wlog, i.ppp.usd, i.ppp.eppp, i.fx))
  
  tail(retail.ppp); head(retail.ppp);
  #Clean Data
  colnames(retail.ppp)[which(names(retail.ppp)=="Value.x")] <- "Retail"
  colnames(retail.ppp)[which(names(retail.ppp)=="Value.y")] <- "Usdx"
  colnames(retail.ppp)[which(names(retail.ppp)=="Value")] <- "Eppp"
  
  #PPP is ratio of relative Prices (the EPPP) and the nominal exchange rate. 
  #The EPPP is implied exchange rate from the prices. Ratio is PPP
  #According to OECD UK PPP is around 0.7 of US
  retail.ppp$PPP <- retail.ppp$Eppp / retail.ppp$Usdx
  
  #If using FX alternative
  if(nrow(i.fx)>2){
    library(zoo)#fill with last observation carried forward
    retail.ppp$FX.Filled <- na.locf(retail.ppp$FX)
    retail.ppp$CrossFX <- retail.ppp$Usdx * retail.ppp$FX
  }
  
  retail.ppp <- retail.ppp[colSums(!is.na(retail.ppp)) > 0]
  retail.ppp <- retail.ppp[complete.cases(retail.ppp),]
  
  retail.ppp <- cbind(retail.ppp,Usdx.Log = c(NA,diff(log(retail.ppp$Usdx))))
  retail.ppp <- cbind(retail.ppp,Eppp.Log = c(NA,diff(log(retail.ppp$Eppp))))
  retail.ppp <- cbind(retail.ppp,PPP.Log = c(NA,diff(log(retail.ppp$PPP))))
  
  if(nrow(i.fx)>2){  retail.ppp$FX.Log <- c(NA,diff(log(retail.ppp$CrossFX)))}
  
  return(retail.ppp)
}


Merge.Retail.And.EPPP <- function(i.retail, i.ppp, use.trade.weights=FALSE, 
                                  trade.weights=NULL, icount, detrend.data = FALSE, optional.detrend = "Monthly"){
  #Get Log Returns of Retail
  
  if(detrend.data){
    if(optional.detrend == "Monthly"){
      
      ts.retail <- ts(i.retail, start = c(year(min(i.retail$Date)),month(min(i.retail$Date))), frequency = 12)
      ts.retail.seasonal <- as.data.frame(cbind(ts.retail, Dummy = seasonaldummy(ts.retail)[1:nrow(ts.retail),]))
      colnames(ts.retail.seasonal) <- gsub("ts.retail.","",colnames(ts.retail.seasonal))
      ts.retail.seasonal$Date <- i.retail$Date
      ts.retail.seasonal$LogValue <- log(ts.retail.seasonal$Value)
      ts.retail.seasonal$DifValue <- c(NA,diff(ts.retail.seasonal$LogValue))
      head(ts.retail.seasonal)    
      
      ##Now Detrend
      monthly.dummy.model <- lm(DifValue ~ Dummy.Jan + Dummy.Feb + Dummy.Mar + Dummy.Apr + Dummy.May + Dummy.Jun
                                + Dummy.Jul + Dummy.Aug + Dummy.Sep + Dummy.Oct + Dummy.Nov, data = ts.retail.seasonal)
      
      i.retail.wlog <- cbind(i.retail, Retail.Log = c(0,residuals(monthly.dummy.model)))
      
      for(i.lag in 1:6){
        i.retail.wlog <- cbind(i.retail.wlog, c(rep(NA,i.lag),rollsum(residuals(monthly.dummy.model),k=i.lag)))
        colnames(i.retail.wlog)[ncol(i.retail.wlog)] <- paste0("Retail.Log.",i.lag)
      }
      
      head(i.retail.wlog)
      
    }else{
      i.retail$LogValue <- log(i.retail$Value)
      ##Now de-trend the series
      trend.model <- lm(LogValue ~ Date, data = i.retail)
      i.retail$Value.Old <- i.retail$Value; #i.retail$Value <- i.retail$Value.Old
      i.retail$Resid <- residuals(trend.model)
      
      i.retail.wlog <- cbind(i.retail, Retail.Log = c(NA, diff(i.retail$Resid)))
      
      for(i.lag in 1:6){
        i.retail.wlog <- cbind(i.retail.wlog, c(rep(NA,i.lag),diff(i.retail$Resid,lag=i.lag)))
        colnames(i.retail.wlog)[ncol(i.retail.wlog)] <- paste0("Retail.Log.",i.lag)
      }
      head(i.retail.wlog)
    }
  }else{
    i.retail.wlog <- cbind(i.retail,Retail.Log = c(NA,diff(log(i.retail$Value))))
    
    for(i.lag in 1:6){
      i.retail.wlog <- cbind(i.retail.wlog, c(rep(NA,i.lag),diff(log(i.retail$Value),lag=i.lag)))
      colnames(i.retail.wlog)[ncol(i.retail.wlog)] <- paste0("Retail.Log.",i.lag)
    }
  }
  
  i.ppp.usd <- i.ppp[i.ppp$Measure.Names == "Usdx",]
  i.ppp.eppp <- i.ppp[i.ppp$Measure.Names == "Eppp",]
  
  if(use.trade.weights){
    ##Then need to calculate weighted PPP
    eppp.list <- usd.list <- ppp.list <- data.frame(Date=as.Date("1900-01-01")); 
    #list of countries for which you have trade data for
    countries.to.check <- colnames(trade.weights)[2:ncol(trade.weights)]
    if(icount != "USA"){countries.to.check <- c(icount,countries.to.check)}
    
    for(temp.country in countries.to.check){
      
      temp.ppp.usd <- i.ppp.usd[i.ppp.usd$Country==temp.country,]
      temp.ppp.eppp <- i.ppp.eppp[i.ppp.eppp$Country==temp.country,]
      
      usd.list <- merge(usd.list, temp.ppp.usd,by="Date",all=TRUE)
      eppp.list <- merge(eppp.list, temp.ppp.eppp,by="Date",all=TRUE)
      colnames(usd.list)[which(colnames(usd.list)=="Value")] <- temp.country;
      colnames(eppp.list)[which(colnames(eppp.list)=="Value")] <- temp.country;
      usd.list <- usd.list[,colnames(usd.list) %in% c("Date",countries.to.check)]
      eppp.list <- eppp.list[,colnames(eppp.list) %in% c("Date",countries.to.check)]
    }
    
    #Now create PPP list by dividing the EPPP (Y)/USDX-FX-rate (X)
    ppp.list <- merge(usd.list,eppp.list,by="Date")
    head(ppp.list)
    for(temp.country in countries.to.check){
      ppp.list$temp <- ppp.list[,paste0(temp.country,".y")]/ppp.list[,paste0(temp.country,".x")]
      colnames(ppp.list)[ncol(ppp.list)] <- temp.country
    }
    ppp.list <- ppp.list[,colnames(ppp.list) %in% c("Date",countries.to.check)]
    
    ##Now get them weighed by the trade.weights
    ppp.weights <- merge(ppp.list, trade.weights,by="Date")
    usd.weights <- merge(usd.list, trade.weights,by="Date")
    eppp.weights <- merge(eppp.list, trade.weights,by="Date")
    
    #Now apply the approapriate weight
    for(temp.country in countries.to.check){
      if(icount != "USA" & temp.country == icount){ #pair domestic EPPP with USA Trade Balance
        ppp.weights$temp <- ppp.weights[,"USA.y"]*ppp.weights[,icount]
        colnames(ppp.weights)[which(colnames(ppp.weights)==icount)] <- paste0(icount,".x") #rename column so filter applies afterwards
        colnames(ppp.weights)[ncol(ppp.weights)] <- temp.country
        
        usd.weights$temp <- usd.weights[,"USA.y"]*usd.weights[,icount]
        colnames(usd.weights)[which(colnames(usd.weights)==icount)] <- paste0(icount,".x") 
        colnames(usd.weights)[ncol(usd.weights)] <- temp.country
        
        eppp.weights$temp <- eppp.weights[,"USA.y"]*eppp.weights[,icount]
        colnames(eppp.weights)[which(colnames(eppp.weights)==icount)] <- paste0(icount,".x") 
        colnames(eppp.weights)[ncol(eppp.weights)] <- temp.country
      }else{ #Run for others as long as it's not USA
        if(icount == "USA"){
          ppp.weights$temp <- ppp.weights[,paste0(temp.country,".y")]*ppp.weights[,paste0(temp.country,".x")]
          colnames(ppp.weights)[ncol(ppp.weights)] <- temp.country
          
          usd.weights$temp <- usd.weights[,paste0(temp.country,".y")]*usd.weights[,paste0(temp.country,".x")]
          colnames(usd.weights)[ncol(usd.weights)] <- temp.country
          
          eppp.weights$temp <- eppp.weights[,paste0(temp.country,".y")]*eppp.weights[,paste0(temp.country,".x")]
          colnames(eppp.weights)[ncol(eppp.weights)] <- temp.country
        }else{ 
          #if country like UK, and calculating the PPP with EU: have to divide the UK-PPP(wUS)/EU-PPP(wUS)
          ppp.weights$temp <- ppp.weights[,paste0(temp.country,".y")]*(ppp.weights[,paste0(icount,".x")]/ppp.weights[,paste0(temp.country,".x")])
          colnames(ppp.weights)[ncol(ppp.weights)] <- temp.country
          
          usd.weights$temp <- usd.weights[,paste0(temp.country,".y")]*(usd.weights[,paste0(icount,".x")]/usd.weights[,paste0(temp.country,".x")])
          colnames(usd.weights)[ncol(usd.weights)] <- temp.country
          
          eppp.weights$temp <- eppp.weights[,paste0(temp.country,".y")]*(eppp.weights[,paste0(icount,".x")]/eppp.weights[,paste0(temp.country,".x")])
          colnames(eppp.weights)[ncol(eppp.weights)] <- temp.country
        }
      }
    }
    eppp.weights <- data.frame(Date = eppp.weights$Date, Eppp = rowSums(eppp.weights[,colnames(eppp.weights) %in% countries.to.check],na.rm=TRUE))
    ppp.weights <- data.frame(Date = ppp.weights$Date, PPP = rowSums(ppp.weights[,colnames(ppp.weights) %in% countries.to.check],na.rm=TRUE))
    usd.weights <- data.frame(Date = usd.weights$Date, Usdx = rowSums(usd.weights[,colnames(usd.weights) %in% countries.to.check],na.rm=TRUE))
    
    retail.ppp <- Reduce(function(...) merge(...,all=TRUE, by = "Date"), list(i.retail.wlog, eppp.weights, ppp.weights, usd.weights))
    tail(retail.ppp); head(retail.ppp);
    colnames(retail.ppp)[which(names(retail.ppp)=="Value")] <- "Retail"
    
    
  }else{
    #conventional merge - not using trade weights
    
    #Merge With USDX and EPPP Data
    #retail.ppp <- merge(i.retail.wlog, i.ppp.usd, i.ppp.eppp, by = "Date")
    retail.ppp <- Reduce(function(...) merge(...,all=TRUE, by = "Date"), list(i.retail.wlog, i.ppp.usd, i.ppp.eppp))
    tail(retail.ppp); head(retail.ppp);
    #Clean Data
    colnames(retail.ppp)[which(names(retail.ppp)=="Value.y")] <- "Usdx"
    colnames(retail.ppp)[which(names(retail.ppp)=="Value")] <- "Eppp"
    colnames(retail.ppp)[which(names(retail.ppp)=="Value.x")] <- "Retail"
    
    #PPP is ratio of relative Prices (the EPPP) and the nominal exchange rate. 
    #The EPPP is implied exchange rate from the prices. Ratio is PPP
    #According to OECD UK PPP is around 0.7 of US
    retail.ppp$PPP <- retail.ppp$Eppp / retail.ppp$Usdx
    
  }
  retail.ppp <- retail.ppp[colSums(!is.na(retail.ppp)) > 0]
  retail.ppp <- retail.ppp[complete.cases(retail.ppp),]
  
  retail.ppp <- cbind(retail.ppp,Usdx.Log = c(NA,diff(log(retail.ppp$Usdx))))
  retail.ppp <- cbind(retail.ppp,Eppp.Log = c(NA,diff(log(retail.ppp$Eppp))))
  retail.ppp <- cbind(retail.ppp,PPP.Log = c(NA,diff(log(retail.ppp$PPP))))
  
  return(retail.ppp)
}

#This function creates monthly dummies
library(forecast)
library(lubridate)
create.monthly.dummies <- function(retail.frame){ 
  #country.plus.usa.retail.ppp
  #ts.retail <- ts(country.plus.usa.retail.ppp, start = c(year(min(country.plus.usa.retail.ppp$Date)),month(min(country.plus.usa.retail.ppp$Date))), frequency = 12)
  #ts.retail.seasonal <- cbind(ts.retail, Dummy = seasonaldummy(ts.retail)[1:nrow(ts.retail),])
  
  ts.retail <- ts(retail.frame, start = c(year(min(retail.frame$Date)),month(min(retail.frame$Date))), frequency = 12)
  ts.retail.seasonal <- cbind(ts.retail, Dummy = seasonaldummy(ts.retail)[1:nrow(ts.retail),])
  return(ts.retail.seasonal)
}

#This function calculates the MA estimates for Retail.Log and Pre-determined Factor
Factors.Calculate.MA <- function(x, n.months, factors.to.use){
  
  for(i.month in 1:n.months){
    for(i.factor in factors.to.use){
      x <- cbind(x, data.frame(new.col =movsum(x[,which(colnames(x)==i.factor)],i.month)/i.month))
    }
  }
  
  #startcol
  startcol <- ncol(x) - n.months*length(factors.to.use)
  name1 <- rep(paste0(factors.to.use,".MA."),n.months)
  name2 <- rep(1:n.months,each = length(factors.to.use)) 
  paste0(name1,name2)
  colnames(x)[seq((startcol+1),ncol(x),1)] <- paste0(name1,name2)
  
  return(x)
}

#This function calculates the MA estimates for Retail.Log, Eppp.Log and USDX log
PPP.and.Retail.Calculate.MA <- function(x, n.months){
  #Get calculation of how the Eppp and Retail Sales have shifted by 1 to 12 months in time
  for(i.month in 1:n.months){
    x <- cbind(x, data.frame(new.col =  movsum(x$Retail.Log,i.month)/i.month))
    x <- cbind(x, data.frame(new.col =movsum(x$Eppp.Log,i.month)/i.month))
    x <- cbind(x, data.frame(new.col =movsum(x$Usdx.Log,i.month)/i.month))
  }
  
  #startcol
  startcol <- ncol(x) - n.months*3
  name1 <- rep(c("Retail.MA.","Eppp.MA.","Usdx.MA."),n.months)
  name2 <- rep(1:n.months,each = 3) 
  paste0(name1,name2)
  colnames(x)[seq((startcol+1),ncol(x),1)] <- paste0(name1,name2)
  
  return(x)
}

Factors.Calculate.Lags <- function(x,n.months,factors.to.use){
  #Get calculation of how the Eppp and Retail Sales have shifted by 1 to 12 months in time
  
  for(i.factor in factors.to.use){
    i.col <- which(colnames(x)==i.factor)
    
    for(i.month in 0:n.months){
      x <- cbind(x, data.frame(new.col =  c(rep(NA,i.month),x[1:(nrow(x)-i.month),i.col])))
    }
  }
  
  #startcol
  startcol <- ncol(x) - (n.months+1)*length(factors.to.use)
  name1 <- rep(paste0(factors.to.use,".Lag."),each =n.months+1)
  name2 <- rep(0:n.months,times = length(factors.to.use)) 
  paste0(name1,name2)
  colnames(x)[seq((startcol+1),ncol(x),1)] <- paste0(name1,name2)
  
  return(x)
}


PPP.and.Retail.Calculate.Lags <- function(x,n.months,n.months.MA=12){
  #Get calculation of how the Eppp and Retail Sales have shifted by 1 to 12 months in time
  col.retail <- which(colnames(x)==paste0("Retail.MA.",n.months.MA))
  col.usdx <- which(colnames(x)==paste0("Usdx.MA.",n.months.MA))
  col.eppp <- which(colnames(x)==paste0("Eppp.MA.",n.months.MA))
  
  for(i.month in 0:n.months){
    x <- cbind(x, data.frame(new.col =  c(rep(NA,i.month),x[1:(nrow(x)-i.month),col.retail])))
    x <- cbind(x, data.frame(new.col =c(rep(NA,i.month),x[1:(nrow(x)-i.month),col.eppp])))
    x <- cbind(x, data.frame(new.col =c(rep(NA,i.month),x[1:(nrow(x)-i.month),col.usdx])))
  }
  
  #startcol
  startcol <- ncol(x) - (n.months+1)*3
  name1 <- rep(c(paste0("Retail.MA.",n.months.MA,".Lag."),paste0("Eppp.MA.",n.months.MA,".Lag.")
                 ,paste0("Usdx.MA.",n.months.MA,".Lag.")),n.months+1)
  name2 <- rep(0:n.months,each = 3) 
  paste0(name1,name2)
  colnames(x)[seq((startcol+1),ncol(x),1)] <- paste0(name1,name2)
  
  return(x)
}


#This function calculates the regressions with the number of MA months
Retail.Get.MA.Results <- function(data.to.use,factor.to.use, months.to.analyse){
  
  MA.regression.results <- data.frame(Retail.Lag = double(), Factor.Lag = double(), Intercept.Coef = double(),
                                      Factor.Coef = double(), Factor.R2 = double())
  for(imonth.retail in months.to.analyse){
    for(imonth.factor in months.to.analyse){
      
      factor.ma <- retail.ppp[,paste0(factor.to.use,".MA.",imonth.factor)]    
      iretail.ma <- retail.ppp[,paste0("Retail.MA.",imonth.retail)]    
      
      ma.fit <- lm(iretail.ma ~ factor.ma)
      
      result.fit <- data.frame(Retail.Lag = imonth.retail, Factor.Lag = imonth.factor, Intercept.Coef = as.numeric(ma.fit$coefficients[1]),
                               Factor.Coef = as.numeric(ma.fit$coefficients[2]), Factor.R2 = as.numeric(summary(ma.fit)["adj.r.squared"]))
      
      MA.regression.results <- rbind(MA.regression.results, result.fit)
    }
  }
  return(MA.regression.results)
}


#This function calculates the regressions with the number of MA months
PPP.and.Retail.Get.MA.Results <- function(ippp.ma, iretail.ma, eppp.months,retail.months){
  MA.regression.results <- data.frame(Eppp.Lag = double(), Retail.Lag = double(), Intercept.Coef = double(), Eppp.Coef = double(), Eppp.R2 = double())
  for(imonth.retail in retail.months){
    for(imonth.eppp in eppp.months){
      
      
      ippp.ma <- retail.ppp[,paste0("Eppp.MA.",imonth.eppp)]    
      iretail.ma <- retail.ppp[,paste0("Retail.MA.",imonth.retail)]    
      
      ma.fit <- lm(iretail.ma ~ ippp.ma)
      
      result.fit <- data.frame(Eppp.Lag = imonth.eppp, Retail.Lag = imonth.retail, Intercept.Coef = as.numeric(ma.fit$coefficients[1]),
                               Eppp.Coef = as.numeric(ma.fit$coefficients[2]), Eppp.R2 = as.numeric(summary(ma.fit)["adj.r.squared"]))
      
      MA.regression.results <- rbind(MA.regression.results, result.fit)
    }
  }
  return(MA.regression.results)
}

###Calculate Leads
PPP.and.Retail.Calculate.Leads <- function(x,n.months,n.months.MA=12){
  #Get calculation of how the Eppp and Retail Sales have shifted by 1 to 12 months in time
  col.retail <- which(colnames(x)==paste0("Retail.MA.",n.months.MA))
  col.usdx <- which(colnames(x)==paste0("Usdx.MA.",n.months.MA))
  col.eppp <- which(colnames(x)==paste0("Eppp.MA.",n.months.MA))
  
  for(i.month in 0:n.months){
    x <- cbind(x, data.frame(new.col =  c(x[(i.month+1):nrow(x),col.retail],rep(NA,i.month))))
    x <- cbind(x, data.frame(new.col =c(x[(i.month+1):nrow(x),col.eppp],rep(NA,i.month))))
    x <- cbind(x, data.frame(new.col =c(x[(i.month+1):nrow(x),col.usdx],rep(NA,i.month))))
  }
  
  #startcol
  startcol <- ncol(x) - (n.months+1)*3
  name1 <- rep(c(paste0("Retail.MA.",n.months.MA,".Lead."),paste0("Eppp.MA.",n.months.MA,".Lead.")
                 ,paste0("Usdx.MA.",n.months.MA,".Lead.")),n.months+1)
  name2 <- rep(0:n.months,each = 3) 
  paste0(name1,name2)
  colnames(x)[seq((startcol+1),ncol(x),1)] <- paste0(name1,name2)
  
  return(x)
}

#This function calculates the R2 of the VAR process

VAR.boot.estimate = function(data, indices){
  d = data[indices,]
  #Calculate LM
  LM1 <- lm(d[,1] ~. ,data = data.frame(d[,c(-1,-2)]))
  LM2 <- lm(d[,2] ~., data = data.frame(d[,c(-1,-2)]))
  R2.1 <- as.numeric(summary(LM1)["adj.r.squared"])
  R2.2 <- as.numeric(summary(LM2)["adj.r.squared"])
  
  rels <- c( R2.1, R2.2)
  return(rels)
}

#This function creates the columns for AR process for 2 variables
VAR.boot.create <- function(var.table, icount.max,var.name.1, var.name.2){
  for(icount in 1:icount.max){
    var.table <- cbind(var.table,c(rep(NA,icount),var.table[1:(nrow(var.table)-icount),1]))
    var.table <- cbind(var.table,c(rep(NA,icount),var.table[1:(nrow(var.table)-icount),2]))
  }
  for(icount in 0:icount.max)
  {
    colnames(var.table)[1 + (icount)*2] = paste0(var.name.1,".",icount)
    colnames(var.table)[2 + (icount)*2] = paste0(var.name.2,".",icount)
  }
  var.table <- as.data.frame(var.table)
  #Clean table for complete cases
  var.table <- var.table[colSums(!is.na(var.table)) > 0]
  var.table <- var.table[complete.cases(var.table),]
  
  return(var.table)
}

MAE <- function(actual, predicted){mean(abs(actual-predicted))}
RMSE <- function(actual, predicted){sqrt(mean((actual-predicted)^2))}


normalize <- function(x) {return((x-min(x))/(max(x)-min(x)))}

movsum <- function(x,n=25){filter(x,rep(1,n), sides=1)}