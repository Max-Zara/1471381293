
Get.Logit.Results <- function(retail.analysis, retail.predict, cols.to.use, Pos.Neg.Mode = "Pos"){
  print(paste0("Started Logit:",Pos.Neg.Mode))
  if(Pos.Neg.Mode =="Pos"){
    logit.model <- glm(Retail.Pos ~., family = binomial(link='logit'), data = retail.analysis[,c("Retail.Pos",cols.to.use)])
  }else{
    logit.model <- glm(Retail.Neg ~., family = binomial(link='logit'), data = retail.analysis[,c("Retail.Neg",cols.to.use)])
  }
  logit.coef <- sum(as.numeric(logit.model$coefficients[str_detect(names(logit.model$coefficients),"PPP.")]))
  logit.mean.coef <- mean(as.numeric(logit.model$coefficients[str_detect(names(logit.model$coefficients),"PPP.")]))
  
  library(MASS)
  logit.predictions <- predict(logit.model,retail.predict[,c(paste0("Retail.",Pos.Neg.Mode),cols.to.use)], type= "response")
  #Classify
  predictions.binary <- ifelse(logit.predictions > 0.5, 1,0);
  
  #Positive Confusion Matrix
  #table(retail.ppp$Retail.Log.Pos, pos.predictions.binary)
  confusion.matrix <- confusionMatrix(table(factor(predictions.binary, levels= c(0,1)),retail.predict[,paste0("Retail.",Pos.Neg.Mode)]))
  logit.confusion.matrix <- confusion.matrix$overall
  
  logit.results <- data.frame(  Model.Form = "Logit",   Model.Type = Pos.Neg.Mode,
                                Model.Accuracy = as.numeric(confusion.matrix$overall["Accuracy"]),   Model.Kappa = as.numeric(confusion.matrix$overall["Kappa"]),
                                Model.Accuracy.Low = as.numeric(confusion.matrix$overall["AccuracyLower"]),   Model.Accuracy.High = as.numeric(confusion.matrix$overall["AccuracyUpper"]),
                                Cumulative.PPP.Coef = logit.coef, Average.PPP.Coef = logit.mean.coef)
  return(logit.results)
}

Get.LASSO.Logit.Results <- function(retail.analysis, retail.predict, cols.to.use, Pos.Neg.Mode = "Pos", Alpha = 1){
  print("Started LASSO")
  train.x <- retail.analysis[complete.cases(retail.analysis), cols.to.use]
  test.x <- retail.predict[complete.cases(retail.predict), cols.to.use]
  
  train.y <- retail.analysis[complete.cases(retail.analysis),paste0("Retail.",Pos.Neg.Mode)]
  test.y <- retail.predict[complete.cases(retail.predict),paste0("Retail.",Pos.Neg.Mode)]
  
  try({
    lasso.model <- cv.glmnet(x=as.matrix(train.x),y=as.matrix(as.factor(train.y)), family = "binomial", alpha = Alpha)
    coef(lasso.model)
    
    try({
      lasso.coef <- sum(as.numeric(coef(lasso.model)[str_detect(rownames(coef(lasso.model)),"PPP.")]))
      avg.lasso.coef <- mean(as.numeric(coef(lasso.model)[str_detect(rownames(coef(lasso.model)),"PPP.")]))
      
      lasso.predictions <- predict(lasso.model, as.matrix(test.x), s= "lambda.min",type="response")
      #Classify
      predictions.binary <- ifelse(lasso.predictions > 0.5, 1,0);
      
      #Positive Confusion Matrix
      #table(retail.ppp$Retail.Log.Pos, pos.predictions.binary)
      confusion.matrix <- confusionMatrix(table(factor(predictions.binary, levels= c(0,1)),test.y))
      lasso.confusion.matrix <- confusion.matrix$overall
      
      if(Alpha == 1){Model.Form = "LASSO-Logit"}else{Model.Form = "Ridge-Logit"}
      
      lasso.results <- data.frame(  Model.Form = Model.Form,   Model.Type = Pos.Neg.Mode,
                                    Model.Accuracy = as.numeric(confusion.matrix$overall["Accuracy"]),   Model.Kappa = as.numeric(confusion.matrix$overall["Kappa"]),
                                    Model.Accuracy.Low = as.numeric(confusion.matrix$overall["AccuracyLower"]),   Model.Accuracy.High = as.numeric(confusion.matrix$overall["AccuracyUpper"]),
                                    Cumulative.PPP.Coef = lasso.coef,
                                    Average.PPP.Coef = avg.lasso.coef)
      return(lasso.results)
    },silent=TRUE)})
}

Get.Bipolar.Logit.Results <- function(retail.analysis, retail.predict, cols.to.use){
  print("Started Bipolar")
  bipolar.model <- polr(as.factor(Retail.Bipolar) ~ ., data = retail.analysis[,c("Retail.Bipolar",cols.to.use)], Hess = TRUE)
  
  bipolar.coef <- sum(as.numeric(bipolar.model$coefficients[str_detect(names(bipolar.model$coefficients),"PPP.")]))
  bipolar.mean.coef <- mean(as.numeric(bipolar.model$coefficients[str_detect(names(bipolar.model$coefficients),"PPP.")]))
  
  bipolar.predictions <- predict(bipolar.model,retail.predict[,c("Retail.Bipolar",cols.to.use)])
  confusion.matrix <- confusionMatrix(table(factor(bipolar.predictions, levels= c(0,0.5,1)),retail.predict[,"Retail.Bipolar"]))
  bipolar.confusion.matrix <- confusion.matrix$overall
  
  logit.results <- data.frame(
    Model.Form = "Logit",
    Model.Type="Bipolar",
    Model.Accuracy = as.numeric(confusion.matrix$overall["Accuracy"]),
    Model.Kappa = as.numeric(confusion.matrix$overall["Kappa"]),
    Model.Accuracy.Low = as.numeric(confusion.matrix$overall["AccuracyLower"]),
    Model.Accuracy.High = as.numeric(confusion.matrix$overall["AccuracyUpper"]),
    Cumulative.PPP.Coef = bipolar.coef, Average.PPP.Coef = bipolar.mean.coef)
  return(logit.results)
}

Get.Classification.Results <- function(retail.analysis, retail.predict, cols.to.use, Pos.Neg.Mode = "Pos",Model.Form="DecisionTree"){ 
  train.x <- retail.analysis[complete.cases(retail.analysis), c(cols.to.use,paste0("Retail.",Pos.Neg.Mode))]
  test.x <- retail.predict[complete.cases(retail.predict), c(cols.to.use,paste0("Retail.",Pos.Neg.Mode))]
  train.y <- retail.analysis[complete.cases(retail.analysis),paste0("Retail.",Pos.Neg.Mode)]
  test.y <- retail.predict[complete.cases(retail.predict),paste0("Retail.",Pos.Neg.Mode)]
  
  switch(Model.Form,
         "RandomForest"={  if(Pos.Neg.Mode=="Pos"){
           decision.model = randomForest(as.factor(Retail.Pos) ~.,data=train.x)}else{
             decision.model = randomForest(as.factor(Retail.Neg) ~.,data=train.x)}
         },
         "DecisionTree"={if(Pos.Neg.Mode=="Pos"){
           decision.model = rpart(as.factor(Retail.Pos) ~.,data=train.x)}else{decision.model = rpart(as.factor(Retail.Neg) ~.,data=train.x)}},
         "SVM"={if(Pos.Neg.Mode=="Pos"){
           decision.model = ksvm(as.factor(Retail.Pos)~ .,data = train.x,cross=5)
         }else{
           decision.model = ksvm(as.factor(Retail.Neg) ~.,data=train.x,cross=5)}},
         "NN"={if(Pos.Neg.Mode=="Pos"){
           f <- as.formula(paste0("Retail.Pos~",paste(colnames(train.x)[!colnames(train.x) %in% "Retail.Pos"], collapse = " + ")))}else{
             f <- as.formula(paste0("Retail.Neg~",paste(colnames(train.x)[!colnames(train.x) %in% "Retail.Neg"], collapse = " + ")))}
           decision.model = neuralnet(f,data=train.x, hidden = mean(c(round(ncol(train.x)/3,0),3)),linear.output = FALSE, act.fct = "tanh")
         },
         "GAM"={if(Pos.Neg.Mode=="Pos"){
           f <- as.formula(paste0("Retail.Pos~",paste("s(",colnames(train.x)[!colnames(train.x) %in% "Retail.Pos"], collapse =  ",k=3)+ "),",k=3)"))}else{
             f <- as.formula(paste0("Retail.Neg~",paste("s(",colnames(train.x)[!colnames(train.x) %in% "Retail.Neg"], collapse = ",k=3) + "),",k=3)"))}
           decision.model = gam(f,data=train.x)
         })
  
  #Measure predictive performance of recursive partitioning tree - SVM requires different type
  if(Model.Form == "SVM"){
    predictions.model = predict(decision.model, test.x,type="response")}else if(Model.Form=="NN"){
      predictions.model = compute(decision.model, test.x[,1:(ncol(test.x)-1)])
      predictions.model = round(predictions.model$net.result,0)
    }else if(Model.Form == "GAM"){
      predictions.model <- predict(decision.model, (test.x))
      predictions.model = round(predictions.model,0)
    }else{
      predictions.model = predict(decision.model, test.x,type="class")}
  
  confusion.matrix <- confusionMatrix(table(factor(predictions.model, levels= c(0,1)),test.y))
  decision.confusion.matrix <- confusion.matrix$overall
  
  decision.results <- data.frame(  Model.Form = Model.Form,   Model.Type = Pos.Neg.Mode,
                                   Model.Accuracy = as.numeric(confusion.matrix$overall["Accuracy"]),   Model.Kappa = as.numeric(confusion.matrix$overall["Kappa"]),
                                   Model.Accuracy.Low = as.numeric(confusion.matrix$overall["AccuracyLower"]),   Model.Accuracy.High = as.numeric(confusion.matrix$overall["AccuracyUpper"]),
                                   Cumulative.PPP.Coef = NA,
                                   Average.PPP.Coef = NA)
  return(decision.results)
}

##### TRY CATCH FUNCTION
Calculate.Logit.Accuracy <- function(retail.analysis, retail.predict, i.sample, i.lag, include.USDX, include.AR){
  cols.to.use <- c(paste0(rep("PPP.Log.Lag.",i.lag+1),seq(0,i.lag,1)))
  
  if(include.USDX){    cols.to.use <- c(cols.to.use, paste0(rep("Us.Log.Lag.",i.lag+1),seq(0,i.lag,1)))   }
  
  if(include.AR){ if(i.lag>0){ cols.to.use <- c(cols.to.use,paste0(rep("Retail.Log.Lag.",i.lag),seq(1,i.lag,1)))}}
  
  print(paste0("Started Sample:",i.sample, " and Lag: ", i.lag))   
  #Plain Logit OLS Model
  results.frame <- list()
  
  for(i.pos.neg in c("Pos","Neg")){
    
    logit.results <- Get.Logit.Results(retail.analysis, retail.predict, cols.to.use, i.pos.neg)
    results.frame[[length(results.frame)+1]] <- logit.results
    
    lasso.logit.results <- Get.LASSO.Logit.Results(retail.analysis, retail.predict, cols.to.use, i.pos.neg)
    if(length(lasso.logit.results)==8){results.frame[[length(results.frame)+1]] <- lasso.logit.results}
    
    ridge.logit.results <- Get.LASSO.Logit.Results(retail.analysis, retail.predict, cols.to.use, i.pos.neg, Alpha = 0)
    if(length(ridge.logit.results)==8){    results.frame[[length(results.frame)+1]] <- ridge.logit.results}
    
    tree.results <- Get.Classification.Results(retail.analysis, retail.predict, cols.to.use, i.pos.neg, Model.Form="DecisionTree")
    results.frame[[length(results.frame)+1]] <- tree.results
    
    forest.results <- Get.Classification.Results(retail.analysis, retail.predict, cols.to.use, i.pos.neg, Model.Form="RandomForest")
    results.frame[[length(results.frame)+1]] <- forest.results
    
    svm.results <- Get.Classification.Results(retail.analysis, retail.predict, cols.to.use, i.pos.neg, Model.Form="SVM")
    results.frame[[length(results.frame)+1]] <- svm.results
    
    neural.results <- Get.Classification.Results(retail.analysis, retail.predict, cols.to.use, i.pos.neg, Model.Form="NN")
    results.frame[[length(results.frame)+1]] <- neural.results
    
    try({gam.results <- Get.Classification.Results(retail.analysis, retail.predict, cols.to.use, i.pos.neg, Model.Form="GAM")
    if(length(gam.results)==8){results.frame[[length(results.frame)+1]] <- gam.results}}
        ,silent=TRUE)
    
  }
  
  #Do Bipolar Model Last
  
  try({if(length(levels(factor(retail.analysis$Retail.Bipolar))) > 2){
    #if bipolar has >2 factors - it's a bipolar model
    bipolar.logit.results <- Get.Bipolar.Logit.Results(retail.analysis, retail.predict, cols.to.use) }else{
      bipolar.logit.results <- logit.results }
    results.frame[[length(results.frame)+1]] <- bipolar.logit.results})
  
  #Get Logit Results
  
  for(i.frame in (1:length(results.frame))){
    results.frame[[i.frame]]$Within.Sample <- i.sample
    results.frame[[i.frame]]$Include.USDX <- include.USDX
    results.frame[[i.frame]]$Include.AR <- include.AR
  }
  print(paste0("Done Sample:",i.sample, " and Lag: ", i.lag))
  
  return(results.frame)
}