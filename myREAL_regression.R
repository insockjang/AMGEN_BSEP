myREAL_regression <-function(model.type = c("ENet","Lasso","Ridge","RF","SVM"), 
                              nfolds = 5){
  
  require(predictiveModeling)
  require(synapseClient)
  
  source("~/AMGEN_BSEP/R/crossValidatePredictiveModel_regression.R")
  
  # input matrix from Synapse: X
  # response vector from Synapse: Y (it might be continuous or binary factor)
  dataSets<-myData(X,Y)
  
  myENet<-function(X,Y,x){
    source("~/AMGEN_BSEP/R/myEnetModel_regression.R")
    alphas =unique(createENetTuneGrid()[,1])     
    modelTrain <- myEnetModel_regression$new()
    modelTrain$customTrain(X, Y, alpha=alphas, nfolds = 5)    
    resultsENet<-modelTrain$customPredict(y)    
    return(resultsENet)
  }
  myLasso<-function(X,Y){
    source("~/AMGEN_BSEP/R/myEnetModel_regression.R")    
    modelTrain <- myEnetModel_regression$new()
    modelTrain$customTrain(X, Y, alpha=1, nfolds = 5)    
    resultsLasso<-modelTrain$customPredict(y)    
    return(resultsLasso)
  }
  myRidge<-function(X,Y){
    source("~/AMGEN_BSEP/R/myEnetModel_regression.R")         
    modelTrain <- myEnetModel_regression$new()
    modelTrain$customTrain(X, Y, alpha= 10 ^-10, nfolds = 5)    
    resultsRidge<-modelTrain$customPredict(y)    
    return(resultsRidge)
  }
  myRF<-function(X,Y){
    source("~/AMGEN_BSEP/R/myRandomForestModel_regression.R")
    modelTrain <- myRandomForestModel_regression$new()
    modelTrain(X, Y, ntree = 500)
    resultsRF<-modelTrain$customPredict(y)
    return(resultsRF)
  }
  mySVM<-function(X,Y){
    require(pls)
    source("~/AMGEN_BSEP/R/mySvmModel_regression.R")    
    modelTrain <- mySvmModel_regression$new()
    modelTrain(X, Y)
    resultsSVM<-modelTrain$customPredict(y)
    return(resultsSVM)
  }
  
  model.fun <- match.arg(model.type)
  switch(model.fun, 
         ENet = (myfun = myENet),
         Lasso = (myfun = myLasso),
         Ridge = (myfun = myRidge),
         RF = (myfun = myRF),
         SVM = (myfun = mySVM))
  
  
  
  # data preprocessing for preselecting features
  filteredData<-filterPredictiveModelData(dataSet$featureData,dataSet$responseData[,kk,drop=FALSE])
  
  # filtered feature and response data
  filteredFeatureData  <- filteredData$featureData
  filteredFeatureData  <- t(unique(t(filteredFeatureData)))
  filteredResponseData <- filteredData$responseData
  
  ## scale these data    
  filteredFeatureDataScaled <- scale(filteredFeatureData)
  filteredResponseDataScaled <- scale(filteredResponseData)  
  
  resultsScale<-myfun(filteredFeatureDataScaled,filteredResponseDataScaled)
  
  return(resultsScale) 
}
