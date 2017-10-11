
Read.All.Data <- function(){
  
  ppp.all <- read.csv("PPP-All.csv")
  ppp.all$Date <- as.Date(ppp.all$Date)
  colnames(ppp.all)[which(names(ppp.all)=="Measure.Values")] <- "Value" #Rename the Column to Value to Merge
  tail(ppp.all)
  unique(ppp.all$Country)
  
  retail.sales <- read.csv("RetailSales-BBG.csv")
  retail.sales$Date <- as.Date(retail.sales$Date)
  tail(retail.sales)
  
  all.data <- list(ppp.all,retail.sales)
  return(all.data)
}

Get.Country.Data <- function(all.data, icount, retail.type = "NA"){
  #Segment the PPP and Retail
  ppp.all <- as.data.frame(all.data[1]); retail.sales <- as.data.frame(all.data[2])
  
  i.ppp <- ppp.all[ppp.all$Country == icount,]
  i.retail <- retail.sales[retail.sales$Country == icount,]
  
  if(retail.type == "NA"){
    #Get the Retail Sales Type
    i.type <- unique(i.retail$Type)[1]
    #Take the wanted index
    i.retail <- i.retail[i.retail$Type==i.type,]
  }else{
    i.retail <- i.retail[i.retail$Type==retail.type,]
  }
  
  country.list <- list(i.ppp, i.retail)
  return(country.list)
}

Merge.Retail.And.EPPP <- function(i.retail, i.ppp){
  #Get Log Returns of Retail
  i.retail.wlog <- cbind(i.retail,Retail.Log = c(NA,diff(log(i.retail$Value))))
  
  #Merge With USDX and EPPP Data
  i.ppp.usd <- i.ppp[i.ppp$Measure.Names == "Usdx",]
  i.ppp.eppp <- i.ppp[i.ppp$Measure.Names == "Eppp",]
  #retail.ppp <- merge(i.retail.wlog, i.ppp.usd, i.ppp.eppp, by = "Date")
  retail.ppp <- Reduce(function(...) merge(...,all=TRUE, by = "Date"), list(i.retail.wlog, i.ppp.usd, i.ppp.eppp))
  tail(retail.ppp); head(retail.ppp);
  #Clean Data
  colnames(retail.ppp)[which(names(retail.ppp)=="Value.x")] <- "Retail"
  colnames(retail.ppp)[which(names(retail.ppp)=="Value.y")] <- "Usdx"
  colnames(retail.ppp)[which(names(retail.ppp)=="Value")] <- "Eppp"
  retail.ppp <- cbind(retail.ppp,Usdx.Log = c(NA,diff(log(retail.ppp$Usdx))))
  retail.ppp <- cbind(retail.ppp,Eppp.Log = c(NA,diff(log(retail.ppp$Eppp))))
  
  retail.ppp <- retail.ppp[colSums(!is.na(retail.ppp)) > 0]
  retail.ppp <- retail.ppp[complete.cases(retail.ppp),]
  return(retail.ppp)
}

#This function calculates the MA estimates for Retail.Log, Eppp.Log and USDX log
PPP.and.Retail.Calculate.MA <- function(x, n.months){
  #Get calculation of how the Eppp and Retail Sales have shifted by 1 to 12 months in time
  for(i.month in 1:n.months){
    x <- cbind(x, data.frame(new.col =  movsum(x$Retail.Log,i.month)))
    x <- cbind(x, data.frame(new.col =movsum(x$Eppp.Log,i.month)))
    x <- cbind(x, data.frame(new.col =movsum(x$Usdx.Log,i.month)))
  }
  
  #startcol
  startcol <- ncol(x) - n.months*3
  name1 <- rep(c("Retail.MA.","Usdx.MA.","Eppp.MA."),n.months)
  name2 <- rep(1:n.months,each = 3) 
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

normalize <- function(x) {return((x-min(x))/(max(x)-min(x)))}

movsum <- function(x,n=25){filter(x,rep(1,n), sides=1)}