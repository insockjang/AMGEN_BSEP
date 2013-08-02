myREAL_regression <-function(synXXX,synYYY,
                             testXXX,
                             model.type = c("ENet","Lasso","Ridge","RF","SVM"), 
                             nfolds = 5){
  
  require(predictiveModeling)
  require(synapseClient)
  
  source("~/AMGEN_BSEP/R/crossValidatePredictiveModel_regression.R")
  
  # input matrix from Synapse: X
  # response vector from Synapse: Y (it might be continuous or binary factor)
  dataSets<-myData(synXXX,synYYY)
  
  testXXX<-loadEntity(testXXX)
  testData<-testXXX$objects$testInputData
  
  myENet<-function(X,Y,testX){
    source("~/AMGEN_BSEP/R/myEnetModel_regression.R")
    alphas =unique(createENetTuneGrid()[,1])     
    modelTrain <- myEnetModel_regression$new()
    modelTrain$customTrain(X, Y, alpha=alphas, nfolds = 5)    
    resultsENet<-modelTrain$customPredict(testX)    
    return(resultsENet)
  }
  myLasso<-function(X,Y,testX){
    source("~/AMGEN_BSEP/R/myEnetModel_regression.R")    
    modelTrain <- myEnetModel_regression$new()
    modelTrain$customTrain(X, Y, alpha=1, nfolds = 5)    
    resultsLasso<-modelTrain$customPredict(testX)    
    return(resultsLasso)
  }
  myRidge<-function(X,Y,testX){
    source("~/AMGEN_BSEP/R/myEnetModel_regression.R")         
    modelTrain <- myEnetModel_regression$new()
    modelTrain$customTrain(X, Y, alpha= 10 ^-10, nfolds = 5)    
    resultsRidge<-modelTrain$customPredict(testX)    
    return(resultsRidge)
  }
  myRF<-function(X,Y,testX){
    source("~/AMGEN_BSEP/R/myRandomForestModel_regression.R")
    modelTrain <- myRandomForestModel_regression$new()
    modelTrain(X, Y, ntree = 500)
    resultsRF<-modelTrain$customPredict(testX)
    return(resultsRF)
  }
  mySVM<-function(X,Y,testX){
    require(pls)
    source("~/AMGEN_BSEP/R/mySvmModel_regression.R")    
    modelTrain <- mySvmModel_regression$new()
    modelTrain(X, Y)
    resultsSVM<-modelTrain$customPredict(testX)
    return(resultsSVM)
  }
  
  model.fun <- match.arg(model.type)
  switch(model.fun, 
         ENet = (myfun1 = myENet),
         Lasso = (myfun1 = myLasso),
         Ridge = (myfun1 = myRidge),
         RF = (myfun1 = myRF),
         SVM = (myfun1 = mySVM))
  
  
  
  # data preprocessing for preselecting features
  filteredData<-filterPredictiveModelData(dataSet$featureData,dataSet$responseData[,kk,drop=FALSE])
  
  # filtered feature and response data
  filteredFeatureData  <- filteredData$featureData
  filteredFeatureData  <- t(unique(t(filteredFeatureData)))
  filteredResponseData <- filteredData$responseData
  
  
  filteredTestData  <- t(unique(t(testData)))
  
  
  ## scale these data    
  filteredFeatureDataScaled <- scale(filteredFeatureData)
  filteredResponseDataScaled <- scale(filteredResponseData)  
  filteredTestDataScaled <- scale(filteredTestData)
  resultsScale<-myfun1(filteredFeatureDataScaled,filteredResponseDataScaled,filteredTestDataScaled)
  
  return(resultsScale) 
}
