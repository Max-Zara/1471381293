## Source Code for Boot Functions

Get.NonLinear.Boot.Results <- function(retail.analysis, retail.predict, cols.to.use, Model.Form="DecisionTree",i.factor.to.test,
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


##Bootstrap LASSO Results
Get.LASSO.Boot.Results <- function(retail.analysis, retail.predict, cols.to.use, Alpha = 1, i.factor.to.test, 
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
