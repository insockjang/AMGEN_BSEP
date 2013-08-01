myModel_classification <-function(model.type = c("ENet","Lasso","Ridge","RF","SVM"), 
                                  nfolds = 5,
                                  ThresholdMethod = NULL ){
  
  require(predictiveModeling)
  require(synapseClient)
  
  source("~/AMGEN_BSEP/R/crossValidatePredictiveModel_classification.R")
  
  # input matrix : X
  # response vector: Y (it might be continuous or binary factor)
  dataSets<-myData(X,Y)
  
  myENet<-function(X,Y){
    source("~/AMGEN_BSEP/R/myEnetModel_classification.R")
    alphas =unique(createENetTuneGrid()[,1])    
    CV<-crossValidatePredictiveModel_classification(X, Y, model = myEnetModel_classification$new(), alpha = alphas, numFolds = nfolds, thresholdMethod = ThresholdMethod)
    return(CV)
  }
  myLasso<-function(X,Y){
    source("~/AMGEN_BSEP/R/myEnetModel_classification.R")
    CV<-crossValidatePredictiveModel_classification(X, Y, model = myEnetModel_classification$new(), alpha = 1, numFolds = nfolds, thresholdMethod = ThresholdMethod)
    return(CV)
  }
  myRidge<-function(X,Y){
    source("~/AMGEN_BSEP/R/myEnetModel_classification.R")
    CV<-crossValidatePredictiveModel_classification(X, Y, model = myEnetModel_classification$new(), alpha = 10^-10, numFolds = nfolds, thresholdMethod = ThresholdMethod)
    return(CV)
  }
  myRF<-function(X,Y){
    source("~/AMGEN_BSEP/R/myRandomForestModel_classification.R")
    CV<-crossValidatePredictiveModel_classification(X, Y, model = myRandomForestModel_classification$new(), ntree = 500, thresholdMethod = ThresholdMethod)
    return(CV)
  }
  mySVM<-function(X,Y){
    require(pls)
    source("~/AMGEN_BSEP/R/mySvmModel_classification.R")    
    CV<-crossValidatePredictiveModel_classification(X, Y, model = mySvmModel_classification$new(), thresholdMethod = ThresholdMethod)
    return(CV)
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
