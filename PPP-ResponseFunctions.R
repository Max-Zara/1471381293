
Get.OLS.Boot.Results <- function(retail.analysis, retail.predict, cols.to.use,i.factor.to.test, 
                                 i.lag, include.USDX, include.AR, factor.density, include.Dummy){
  print(paste0(Sys.time()," Started Gaussian OLS"))
  
  #d <- retail.analysis[indices,c(i.factor.to.test,cols.to.use)] #boot selects sample
  f <- as.formula(paste(i.factor.to.test,"~."))
  
  #get models through boot
  boot.get.ols <- function(data,indices,formula,retail.predict,i.lag, include.USDX, include.AR, factor.density){
    d <- data[indices,]
    ols.model <- glm(formula,family=gaussian,data=d)
    
    ols.predictions <- predict(ols.model,retail.predict)
    ols.matched <- na.omit(cbind(retail.predict[,i.factor.to.test] ,ols.predictions))
    ols.MAE <- MAE(ols.matched[,1],ols.matched[,2])
    ols.RMSE <- RMSE(ols.matched[,1],ols.matched[,2])
    
    ols.responses <- c(ols.MAE,ols.RMSE)
    
    calculated.density <- 0
    default.response.input <- Get.Response.Input(i.lag, include.USDX, include.AR, include.Dummy, cols.to.use)
    response.results <- matrix(0,nrow=1,ncol=ncol(default.response.input))
    density.breaks <- factor.density$breaks[2] - factor.density$breaks[1]
    
    for(i.break in 1:length(factor.density$density)){
      temp.density <- factor.density$density[i.break]/sum(factor.density$density)
      calculated.density <- calculated.density + temp.density
      
      response.input.before <- factor.density$breaks[i.break]
      response.input.value <- factor.density$breaks[i.break+1]
      #Get the Response Matrix
      response.input <- Get.Response.Input(i.lag, include.USDX, include.AR, include.Dummy, cols.to.use, response.input.value)
      response.input.last <- Get.Response.Input(i.lag, include.USDX, include.AR, include.Dummy, cols.to.use, response.input.before)
      
      #print(predict(ols.model,data.frame(t(response.input[,1]))))
      for(i.input in 1:ncol(response.input)){
        response.impact  <- predict(ols.model,data.frame(t(response.input[,i.input])))
        response.impact.last <- predict(ols.model,data.frame(t(response.input.last[,i.input])))
        
        net.impact <- (response.impact - response.impact.last)/density.breaks
        
        response.results[,i.input] <- response.results[,i.input]+  temp.density* net.impact #weighting + impact/PPP shift
      }
    }
    #Check if density has been fulfilled twice (once for non-cumulative coefficients, once for cumulative coefficients)
    if(round(calculated.density- 1,5)==0){ 
      ols.responses <- c(ols.responses,unlist(response.results))} else{
        print(paste0("Density not at 100%:",calculated.density/2));  stop("ERROR - Not Full Density")}
    return(ols.responses)
  }
  
  #First is MAE, Second is RMSE, Then table of responses based on response.input
  ols.models <- boot(retail.analysis[,c(i.factor.to.test,cols.to.use)],boot.get.ols,R=100,stype="i",
                     formula = f,retail.predict=retail.predict,i.lag=i.lag, include.USDX=include.USDX,
                     include.AR=include.AR, factor.density=factor.density )
  
  #t1 is MAE
  #t2 is RMSE
  #t3 - response input per response.input matrix. 3 = 0 PPP impact, 4 = 1 for PPP month 0
  
  return(ols.models)
}

Chart.NonLin.Responses <- function(temp.models, additional.heading = "", optional.divisor = 1, optional.color = brewer.pal(length(response.density),"Paired"), optional.nmonths = 12, response.density){
  
  par(mfrow=c(1,2))
  temp.means <- matrix(colSums(temp.models$t)/100,nrow=((optional.nmonths+1)*2+1))
  
  #First response is 0, then next 13 is response of 0 to 12 lags, then next 13 is cumulative 0 to 12 lags for total of 27 responses
  #Add on for each density to calculate
  
  #Derive Net Impact (Taking away response from 0 stimulation)
  #Normal response
  temp.means1 <- temp.means[,1] - temp.means[1,1]
  temp.means2 <- temp.means[,2] - temp.means[1,2]
  temp.means <- cbind(temp.means1,temp.means2)
  #Cumulative Response
  #temp.means[,(optional.nmnonths+3)] <- temp.means[,2:(optional.nmonths+1)] - temp.means[,1]
  
  matplot(temp.means[2:(optional.nmonths+2),],type='l',col=optional.color,lty=1,lwd=2,main=paste0("Monthly:",additional.heading),
          xlab = "No of Lags of PPP",ylab="Response")
  grid(NULL,NULL,col="darkgrey",lwd=2); abline(h=0)
  legend("bottomleft",legend = names(response.density), 
         col = optional.color
         , bg="transparent",bty='n',cex=1.5, pch=16)
  matplot(temp.means[(optional.nmonths+3):((optional.nmonths+1)*2+1),],type='l',col=optional.color,lty=1,lwd=2,main=paste0("Cum:",additional.heading),
          xlab = "No of Lags of PPP",ylab="Response")
  grid(NULL,NULL,col="darkgrey",lwd=2); abline(h=0)
  legend("bottomleft",legend = names(response.density), 
         col = optional.color
         , bg="transparent",bty='n',cex=1.5, pch=16)
}


Chart.Fan.NonLin.Response <- function(temp.models, additional.heading="", optional.divisor = 1, optional.color =colorRampPalette(c("tomato", "gray90")), optional.nmonths = 12 
                                      ,optional.startnum = 1){
  par(mfrow=c(1,2))
  
  #Which Percentiles do you wish to get
  p <- seq(0.1,0.9,0.05); 
  
  y <- seq(min(temp.models$t[,optional.startnum:(optional.startnum+(optional.nmonths+1)*2-1)]/optional.divisor),
           max(temp.models$t[,optional.startnum:(optional.startnum+(optional.nmonths+1)*2-1)]/optional.divisor),length.out=((optional.nmonths+1)))
  #K - number of months
  k <- optional.nmonths+1
  #ncol is months, nrow is percentiles
  coefval <- matrix(NA, nrow=length(p),ncol=k)
  cum.coefval <- matrix(NA, nrow=length(p),ncol=k)
  for(i in 1:k){
    coefval[,i] <- quantile(temp.models$t[,optional.startnum-1+i]/optional.divisor,p)
    cum.coefval[,i] <- quantile(temp.models$t[,optional.startnum-1+optional.nmonths+1+i]/optional.divisor,p)}
  
  #Chart the data
  par(mfrow=c(1,2))
  for(i.data in c("Monthly Coef","Cumulative Coef")){
    if(i.data=="Monthly Coef"){i.frame <- coefval}else{i.frame<-cum.coefval}
    
    plot(0:optional.nmonths,y, type = "l", col = "lightgrey", lwd = 0, 
         xlim = c(0,optional.nmonths), ylim = c(min(y), max(y)), 
         ylab="Coefficient",xlab="Months",main=paste0(additional.heading,i.data," After ",nrow(temp.models$t)," Straps"))
    grid(NULL,NULL,col="darkgrey",lwd=2)
    
    fan(data = i.frame, data.type = "values", probs = p, 
        start = 0, frequency = 1, 
        anchor = 0, 
        fan.col = optional.color,  
        ln = NULL, rlab = NULL)
    abline(h=0)}
  #dev.print(png,paste0("Images/",))
}


Chart.Fan.Models <- function(temp.models, additional.heading="", optional.divisor = 1, optional.color =colorRampPalette(c("tomato", "gray90")), optional.nmonths = 6 ){
  par(mfrow=c(1,2))
  
  #Which Percentiles do you wish to get
  p <- seq(0.05,0.95,0.05); p <- c(0.01,p,0.99); #set percentiles to plot
  
  y <- seq(min(temp.models$t[,4:((optional.nmonths+1)*2+3)]/optional.divisor),
           max(temp.models$t[,4:((optional.nmonths+1)*2+3)]/optional.divisor),length.out=((optional.nmonths+1)))
  #K - number of months
  k <- optional.nmonths+1
  #ncol is months, nrow is percentiles
  coefval <- matrix(NA, nrow=length(p),ncol=k)
  cum.coefval <- matrix(NA, nrow=length(p),ncol=k)
  for(i in 1:k){
    coefval[,i] <- quantile(temp.models$t[,3+i]/optional.divisor,p)
    cum.coefval[,i] <- quantile(temp.models$t[,3+optional.nmonths+1+i]/optional.divisor,p)}
  
  #Chart the data
  par(mfrow=c(1,2))
  for(i.data in c("Monthly Coef","Cumulative Coef")){
    if(i.data=="Monthly Coef"){i.frame <- coefval}else{i.frame<-cum.coefval}
    
    plot(0:optional.nmonths,y, type = "l", col = "lightgrey", lwd = 0, 
         xlim = c(0,optional.nmonths), ylim = c(min(y), max(y)), 
         ylab="Coefficient",xlab="Months",main=paste0(additional.heading,i.data," After ",nrow(temp.models$t)," Straps"))
    grid(NULL,NULL,col="darkgrey",lwd=2)
    
    fan(data = i.frame, data.type = "values", probs = p, 
        start = 0, frequency = 1, 
        anchor = 0, 
        fan.col = optional.color,  
        ln = NULL, rlab = NULL)
    abline(h=0)}
  #dev.print(png,paste0("Images/",))
}

##For Charting all Model Responses Together
Add.Line.For.Model <- function(temp.models, imodel.col, add=TRUE, cum= FALSE, heading.to.use, optional.nmonths=6){
  mean.single.val <- double(); mean.cum.val <- double();
  if(length(temp.models)==11){
    for(i.lag in 1:(optional.nmonths+1)){
      mean.single.val <- c(mean.single.val,mean(temp.models$t[,3+i.lag]))
      mean.cum.val <- c(mean.cum.val,mean(temp.models$t[,((optional.nmonths+1)+3)+i.lag]))
    }}else{mean.single.val <- mean.cum.val <- rep(0,(optional.nmonths+1))}
  if(cum){val.to.use <- mean.cum.val}else{val.to.use <- mean.single.val}
  
  if(add){
    lines(0:optional.nmonths,val.to.use, main=heading.to.use, type='l', lwd = 2, col = imodel.col)
  }else{
    plot(0:optional.nmonths,val.to.use, main=heading.to.use,xlim=c(0,optional.nmonths),ylim=c(-0.5,0.5), type='l',lwd = 2, col=imodel.col)
  }
}


###Chart OLS Models 
Chart.OLS.Density.Models <- function(ols.models){
  par(mfrow=c(1,2))
  plot(density(ols.models$t[,4]),col=rgb(0,0,1,0.2),ylim=c(0,15),xlim=c(-0.5,0.5),lwd=3, main="Individual Monthly Response")
  for(i.chart in 1:6){lines(density(ols.models$t[,4+i.chart]),col=rgb(1/6*i.chart,1/i.chart,1,0.2),lwd=3,lty=i.chart)}
  
  plot(density(ols.models$t[,11]),col=rgb(0,0,1,0.2),ylim=c(0,15),xlim=c(-0.5,0.5),lwd=3, main = "Cumulative Monthly Response")
  for(i.chart in 12:17){lines(density(ols.models$t[,i.chart]),col=rgb(1/17*i.chart,1/(i.chart-11),1,0.2),lwd=3,lty=i.chart)}
}


##Bootstrap LASSO Results
Get.LASSO.Results <- function(retail.analysis, retail.predict, cols.to.use, Alpha = 1, i.factor.to.test, 
                              i.lag, include.USDX, include.AR, factor.density, include.Dummy){
  print(paste0(Sys.time()," Started ",if(Alpha==1){"LASSO"}else{"RIDGE"}))
  train.x <- retail.analysis[complete.cases(retail.analysis), c(cols.to.use,i.factor.to.test)]
  test.x <- retail.predict[complete.cases(retail.predict), cols.to.use]
  
  train.y <- retail.analysis[complete.cases(retail.analysis),i.factor.to.test]
  test.y <- retail.predict[complete.cases(retail.predict),i.factor.to.test]
  
  try({
    
    boot.get.lasso <- function(data,indices,formula,train.y,test.x,test.y, Alpha,
                               i.lag, include.USDX, include.AR, cols.to.use,factor.density){
      d <- data[indices,]; train.y <- train.y[indices]; 
      d <- d[,1:(ncol(d)-1)]#remove last column - Which is the Actual Observed Value of Retail Shift
      
      lasso.model <- cv.glmnet(x=as.matrix(d),y=as.matrix((train.y)), nfolds=10)
      #If wish to see coefficients
      #coef(lasso.model, s= "lambda.min")
      
      lasso.predictions <- predict(lasso.model, as.matrix(test.x), s= "lambda.min")
      #Classify
      lasso.matched <- na.omit(cbind(as.data.frame(test.y) ,as.data.frame(lasso.predictions)))
      lasso.MAE <- MAE(lasso.matched[,1],lasso.matched[,2])
      lasso.RMSE <- RMSE(lasso.matched[,1],lasso.matched[,2])
      
      lasso.responses <- c(lasso.MAE,lasso.RMSE)
      
      calculated.density <- 0
      default.response.input <- Get.Response.Input(i.lag, include.USDX, include.AR, include.Dummy, cols.to.use)
      response.results <- matrix(0,nrow=1,ncol=ncol(default.response.input))
      density.breaks <- factor.density$breaks[2] - factor.density$breaks[1]
      for(i.break in 1:length(factor.density$density)){
        
        temp.density <- factor.density$density[i.break]/sum(factor.density$density)
        calculated.density <- calculated.density + temp.density
        
        response.input.before <- factor.density$breaks[i.break]
        response.input.value <- factor.density$breaks[i.break+1]
        #Get the Response Matrix
        response.input <- Get.Response.Input(i.lag, include.USDX, include.AR, include.Dummy, cols.to.use, response.input.value)
        response.input.last <- Get.Response.Input(i.lag, include.USDX, include.AR, include.Dummy, cols.to.use, response.input.before)
        
        for(i.input in 1:ncol(response.input)){ 
          response.impact <- predict(lasso.model,as.matrix(t(response.input[,i.input])),s="lambda.min")
          response.impact.last <- predict(lasso.model,as.matrix(t(response.input.last[,i.input])),s="lambda.min")
          
          net.impact <- (response.impact - response.impact.last)/density.breaks
          
          response.results[i.input] <- response.results[i.input] +
            temp.density* (net.impact)#weighting + impact/PPP shift
          }
      }
      if(round(calculated.density- 1,5)==0){ 
        lasso.responses <- c(lasso.responses,unlist(response.results))} else{print("Density not at 100%"); stop("ERROR - Not Full Density")}
      return(lasso.responses)
    }
    
    #First is MAE, Second is RMSE, Then table of responses based on response.input
    lasso.models <- boot(train.x,boot.get.lasso,R=100,stype="i",
                         train.y= train.y, test.x = test.x, test.y = test.y, Alpha = Alpha, i.lag = i.lag,
                         include.USDX = include.USDX, include.AR = include.AR, cols.to.use = cols.to.use, factor.density = factor.density)
    
    return(lasso.models)
  },silent=TRUE)
}

Get.NonLinear.Results <- function(retail.analysis, retail.predict, cols.to.use, Model.Form="DecisionTree",i.factor.to.test,
                                  i.lag, include.USDX, include.AR, factor.density, include.Dummy){ 
  print(paste0(Sys.time()," Started ",Model.Form))
  
  train.x <- retail.analysis[complete.cases(retail.analysis), c(cols.to.use,i.factor.to.test)]
  test.x <- retail.predict[complete.cases(retail.predict), c(cols.to.use,i.factor.to.test)]
  train.y <- retail.analysis[complete.cases(retail.analysis),i.factor.to.test]
  test.y <- retail.predict[complete.cases(retail.predict),i.factor.to.test]
  
  try({
    boot.get.nonlin <- function(data,indices,train.y,test.x,test.y, Model.Form,
                                i.lag, include.USDX, include.AR, cols.to.use,factor.density){
      
      d <- data[indices,]; train.y <- train.y[indices];
      
      f <- as.formula(paste(i.factor.to.test,"~."))
      
      switch(Model.Form,
             "RandomForest"={decision.model = randomForest(f,data=d)   },
             "DecisionTree"={decision.model = rpart(f,data=d)},
             "SVM"={ decision.model = ksvm(f,data = d,cross=5)},
             "NN"={ f <- as.formula(paste0(i.factor.to.test,"~",paste(colnames(d)[!colnames(d) %in% i.factor.to.test], collapse = " + ")))
             decision.model = neuralnet(f,data=d, hidden = mean(c(round(ncol(d)/3,0),3)))         },
             "GAM"={ f <- as.formula(paste0(i.factor.to.test,"~",paste("s(",colnames(d)[!colnames(d) %in% i.factor.to.test], collapse = ",k=3) + "),",k=3)"))
             decision.model = gam(f,data=d)
             })
      
      #Measure predictive performance of recursive partitioning tree - SVM requires different type
      if(Model.Form == "SVM"){
        predictions.model = predict(decision.model, test.x)}else if(Model.Form=="NN"){
          predictions.model = compute(decision.model, test.x[,1:(ncol(test.x)-1)])
          predictions.model = round(predictions.model$net.result,0)
        }else if(Model.Form == "GAM"){
          predictions.model <- predict(decision.model, (test.x))
          predictions.model = round(predictions.model,0)
        }else{
          predictions.model = predict(decision.model, test.x)}
      
      #Get Statistics
      nonlin.matched <- na.omit(cbind(as.data.frame(test.y) ,as.data.frame(predictions.model)))
      nonlin.MAE <- MAE(nonlin.matched[,1],nonlin.matched[,2])
      nonlin.RMSE <- RMSE(nonlin.matched[,1],nonlin.matched[,2])
      
      nonlin.responses <- c(nonlin.MAE,nonlin.RMSE)
      
      calculated.density <- 0
      default.response.input <- Get.Response.Input(i.lag, include.USDX, include.AR, include.Dummy, cols.to.use)
      response.results <- matrix(0,nrow=1,ncol=ncol(default.response.input))
      density.breaks <- factor.density$breaks[2] - factor.density$breaks[1]
      for(i.break in 1:length(factor.density$density)){
        
        temp.density <- factor.density$density[i.break]/sum(factor.density$density)
        calculated.density <- calculated.density + temp.density
        
        response.input.before <- factor.density$breaks[i.break]
        response.input.value <- factor.density$breaks[i.break+1]
        #Get the Response Matrix
        response.input <- Get.Response.Input(i.lag, include.USDX, include.AR, include.Dummy, cols.to.use, response.input.value)
        response.input.last <- Get.Response.Input(i.lag, include.USDX, include.AR, include.Dummy, cols.to.use, response.input.before)
        
        #Loop Through the Response Input Matrices (0 effect, only PPP(0), then PPP(1)....)
        for(i.input in 1:ncol(response.input)){ 
          
          response.impact <-  if(Model.Form == "SVM"){
                              predictions.model = predict(decision.model, as.matrix(t(response.input[,i.input])))}else if(Model.Form=="NN"){
                                
                                predictions.model = compute(decision.model, as.matrix(t(response.input[,i.input])))$net.result
                              }else if(Model.Form == "GAM"){
                                predictions.model <- predict(decision.model, as.data.frame(t(response.input[,i.input])))
                              }else{
                                predictions.model = predict(decision.model, as.data.frame(t(response.input[,i.input])))}
          
          response.impact.last <-  if(Model.Form == "SVM"){
            predictions.model = predict(decision.model, as.matrix(t(response.input.last[,i.input])))}else if(Model.Form=="NN"){
              
              predictions.model = compute(decision.model, as.matrix(t(response.input.last[,i.input])))$net.result
            }else if(Model.Form == "GAM"){
              predictions.model <- predict(decision.model, as.data.frame(t(response.input.last[,i.input])))
            }else{
              predictions.model = predict(decision.model, as.data.frame(t(response.input.last[,i.input])))}
          
          net.impact <- (response.impact - response.impact.last)/density.breaks
          
          response.results[i.input] <- response.results[i.input] +
            temp.density* (net.impact)#weighting + impact/PPP shift
        }
      }
      if(round(calculated.density- 1,5)==0){ 
      nonlin.responses <- c(nonlin.responses,unlist(response.results))} else{print("Density not at 100%"); stop("ERROR - Not Full Density")}
      return(nonlin.responses)
    }
    
    #First is MAE, Second is RMSE, Then table of responses based on response.input
    nonlin.models <- boot(train.x,boot.get.nonlin,R=100,stype="i",
                          train.y= train.y, test.x = test.x, test.y = test.y, Model.Form=Model.Form,
                          i.lag = i.lag, include.USDX = include.USDX, include.AR = include.AR, cols.to.use = cols.to.use,
                          factor.density = factor.density)
    
    return(nonlin.models)
  },silent=TRUE)
  
}

###Function to calculate the Impulse Response Input
Get.Response.Input <- function(i.lag, include.USDX, include.AR, include.Dummy=FALSE, cols.to.use, val.response=0.01){
  
  #(i.lag+1) for PPP terms incluing 0 term, (i.lag+1) for uSDX terms and i.lag for AR terms, because omitting 0 term
  response.length <- (i.lag+1) + (i.lag+1)*include.USDX + include.AR*(i.lag) + include.Dummy*11
  response.input <- rep(0,response.length);
  for(i.in in 1:(i.lag+1)){response.input <- c(response.input,rep(0,i.in-1),val.response,rep(0,(i.lag+1)-i.in),rep(0,(i.lag+1)*include.USDX),rep(0,(i.lag)*include.AR),rep(0,11*include.Dummy))}; 
  for(i.in in 1:(i.lag+1)){response.input <- c(response.input,rep(val.response,i.in),rep(0,(i.lag+1)-i.in),rep(0,(i.lag+1)*include.USDX),rep(0,(i.lag)*include.AR),rep(0,11*include.Dummy))};
  response.input <- matrix(response.input,nrow=response.length)
  rownames(response.input) <- cols.to.use
  
  return(response.input)
}

Get.Cols.To.Use <- function(i.lag, include.USDX, include.AR, include.Dummy){
  cols.to.use <- c(paste0(rep("PPP.Log.Lag.",i.lag+1),seq(0,i.lag,1)))
  if(include.USDX){    cols.to.use <- c(cols.to.use, paste0(rep("Us.Log.Lag.",i.lag+1),seq(0,i.lag,1)));}
  
  if(include.AR){ if(i.lag>0){ cols.to.use <- c(cols.to.use,paste0(rep("Retail.Log.Lag.",i.lag),seq(1,i.lag,1)))}}
  
  if(include.Dummy){cols.to.use <- c(cols.to.use,paste0("Dummy.",month.abb[seq(1,11,1)]))}
  
  return(cols.to.use)
}


Get.NonLinear.Density.Response <- function(retail.analysis, retail.predict, cols.to.use, Model.Form="NN",i.factor.to.test,
                            i.lag, include.USDX, include.AR, response.density, include.Dummy){ 
  print(paste0(Sys.time()," Started ",Model.Form))
  
  train.x <- retail.analysis[complete.cases(retail.analysis), c(cols.to.use,i.factor.to.test)]
  train.y <- retail.analysis[complete.cases(retail.analysis),i.factor.to.test]
  
  try({
    boot.get.nonlin <- function(data,indices,train.y,Model.Form,
                                i.lag, include.USDX, include.AR, cols.to.use,response.density){
      
      d <- data[indices,]; train.y <- train.y[indices];
      
      f <- as.formula(paste(i.factor.to.test,"~."))
      
      switch(Model.Form,
             "RandomForest"={decision.model = randomForest(f,data=d)   },
             "DecisionTree"={decision.model = rpart(f,data=d)},
             "SVM"={ decision.model = ksvm(f,data = d,cross=5)},
             "NN"={ f <- as.formula(paste0(i.factor.to.test,"~",paste(colnames(d)[!colnames(d) %in% i.factor.to.test], collapse = " + ")))
             decision.model = neuralnet(f,data=d, hidden = mean(c(round(ncol(d)/3,0),3)),act.fct = "tanh")         },
             "GAM"={ f <- as.formula(paste0(i.factor.to.test,"~",paste("s(",colnames(d)[!colnames(d) %in% i.factor.to.test], collapse = ",k=3) + "),",k=3)"))
             decision.model = gam(f,data=d)
             })
      
      
      default.response.input <- Get.Response.Input(i.lag, include.USDX, include.AR, include.Dummy, cols.to.use)
      response.results <- matrix(0,nrow=1,ncol=ncol(default.response.input)*length(response.density))
      
      for(i.density in 1:length(response.density)){
        
        #Get the Response Matrix
        response.input <- Get.Response.Input(i.lag, include.USDX, include.AR, include.Dummy, cols.to.use, response.density[i.density])
        
        #Loop Through the Response Input Matrices (0 effect, only PPP(0), then PPP(1)....)
        for(i.input in 1:ncol(response.input)){ 
          
          response.impact <-  if(Model.Form == "SVM"){
            predictions.model = predict(decision.model, as.matrix(t(response.input[,i.input])))}else if(Model.Form=="NN"){
              
              predictions.model = compute(decision.model, as.matrix(t(response.input[,i.input])))$net.result
            }else if(Model.Form == "GAM"){
              predictions.model <- predict(decision.model, as.data.frame(t(response.input[,i.input])))
            }else{
              predictions.model = predict(decision.model, as.data.frame(t(response.input[,i.input])))}
          
          #Get the response (re-scaled)
          response.results[i.input + (i.density-1)*ncol(default.response.input)] <- response.impact/response.density[i.density]
        }
      }
      
      #First response is 0, then next 13 is response of 0 to 12 lags, then next 13 is cumulative 0 to 12 lags for total of 27 responses
      #Add on for each density to calculate
      nonlin.responses <- c(unlist(response.results))
      return(nonlin.responses)
    }
    
    #First is MAE, Second is RMSE, Then table of responses based on response.input
    nonlin.models <- boot(train.x,boot.get.nonlin,R=100,stype="i",
                          train.y= train.y, Model.Form=Model.Form,
                          i.lag = i.lag, include.USDX = include.USDX, include.AR = include.AR, cols.to.use = cols.to.use,
                          response.density = response.density)
    
    return(nonlin.models)
  },silent=TRUE)
}

Calculate.NonLinear.Responses <- function(retail.analysis, retail.predict, i.sample, i.lag, include.USDX,
                                          include.AR, i.factor.to.test, response.density, include.Dummy){
  cols.to.use <- Get.Cols.To.Use(i.lag,include.USDX, include.AR, include.Dummy)
  results.frame <- list()
  
  neural.results <- Get.NonLinear.Density.Response(retail.analysis, retail.predict, cols.to.use
                                          , Model.Form="NN",i.factor.to.test, i.lag
                                          , include.USDX, include.AR, response.density, include.Dummy)
  #Chart.Fan.Models(neural.results,"NEURAL NET ")
  results.frame[[length(results.frame)+1]] <- neural.results
  names(results.frame)[length(results.frame)] <- "NN"
  
  print("ReturningResults")
  return(results.frame)
}

##### TRY CATCH FUNCTION
Calculate.Response.Functions.And.Accuracy <- function(retail.analysis, retail.predict,
                                                      i.sample, i.lag, include.USDX, include.AR,
                                                      i.factor.to.test, factor.density,include.Dummy){
  
  cols.to.use <- Get.Cols.To.Use(i.lag,include.USDX, include.AR, include.Dummy)
  
  print(paste0("Started Sample:",i.sample))   
  #Plain Logit OLS Model
  results.frame <- list()
  
  #Get Response Input Shape
  #response.input <- Get.Response.Input(i.lag, include.USDX, include.AR, cols.to.use, val.response)
  
  ols.results <- Get.OLS.Boot.Results(retail.analysis, retail.predict, cols.to.use,i.factor.to.test
                                      , i.lag, include.USDX, include.AR, factor.density, include.Dummy)
  #Chart.Fan.Models(ols.results,"OLS ")
  results.frame[[length(results.frame)+1]] <- ols.results
  names(results.frame)[length(results.frame)] <- "OLS"
  
  lasso.results <- Get.LASSO.Results(retail.analysis, retail.predict, cols.to.use, Alpha = 1
                                     , i.factor.to.test, i.lag, include.USDX, include.AR, factor.density, include.Dummy)
  #Chart.Fan.Models(lasso.results,"LASSO ")
  results.frame[[length(results.frame)+1]] <- lasso.results
  names(results.frame)[length(results.frame)] <- "LASSO"
  
  ridge.results <- Get.LASSO.Results(retail.analysis, retail.predict, cols.to.use, Alpha = 0
                                     , i.factor.to.test, i.lag, include.USDX, include.AR, factor.density, include.Dummy)
  #Chart.Fan.Models(ridge.results,"RIDGE ")
  results.frame[[length(results.frame)+1]] <- ridge.results
  names(results.frame)[length(results.frame)] <- "Ridge"
  
  tree.results <- Get.NonLinear.Results(retail.analysis, retail.predict, cols.to.use
                                        , Model.Form="DecisionTree",i.factor.to.test, i.lag
                                        , include.USDX, include.AR, factor.density, include.Dummy)
  #Chart.Fan.Models(tree.results,"DECISION TREE ")
  results.frame[[length(results.frame)+1]] <- tree.results
  names(results.frame)[length(results.frame)] <- "Tree"
  
  forest.results <- Get.NonLinear.Results(retail.analysis, retail.predict, cols.to.use
                                          , Model.Form="RandomForest",i.factor.to.test, i.lag
                                          , include.USDX, include.AR, factor.density, include.Dummy)
  #Chart.Fan.Models(forest.results,"RNDOM FOREST ")
  results.frame[[length(results.frame)+1]] <- forest.results
  names(results.frame)[length(results.frame)] <- "Forest"
  
  svm.results <- Get.NonLinear.Results(retail.analysis, retail.predict, cols.to.use
                                       , Model.Form="SVM",i.factor.to.test, i.lag
                                       , include.USDX, include.AR, factor.density, include.Dummy)
  #Chart.Fan.Models(svm.results,"SVM ")
  results.frame[[length(results.frame)+1]] <- svm.results
  names(results.frame)[length(results.frame)] <- "SVM"
  
  neural.results <- Get.NonLinear.Results(retail.analysis, retail.predict, cols.to.use
                                          , Model.Form="NN",i.factor.to.test, i.lag
                                          , include.USDX, include.AR, factor.density, include.Dummy)
  #Chart.Fan.Models(neural.results,"NEURAL NET ")
  results.frame[[length(results.frame)+1]] <- neural.results
  names(results.frame)[length(results.frame)] <- "NN"
  
  try({
    gam.results <- Get.NonLinear.Results(retail.analysis, retail.predict, cols.to.use
                                         , Model.Form="GAM",i.factor.to.test, i.lag
                                         , include.USDX, include.AR, factor.density, include.Dummy)
    #Chart.Fan.Models(gam.results,"GAM ")
    results.frame[[length(results.frame)+1]] <- gam.results
    names(results.frame)[length(results.frame)] <- "GAM"
    
  },silent=TRUE)
  
  print("ReturningResults")
  return(results.frame)
}
