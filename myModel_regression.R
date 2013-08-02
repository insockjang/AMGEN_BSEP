myModel_regression <-function(model.type = c("ENet","Lasso","Ridge","RF","SVM"), 
                                  nfolds = 5){
  
  require(predictiveModeling)
  require(synapseClient)
  
  source("~/AMGEN_BSEP/R/crossValidatePredictiveModel_regression.R")
  
  # input matrix from Synapse: X
  # response vector from Synapse: Y (it might be continuous or binary factor)
  dataSets<-myData(X,Y)
  
  myENet<-function(X,Y){
    source("~/AMGEN_BSEP/R/myEnetModel_regression.R")
    alphas =unique(createENetTuneGrid()[,1])    
    CV<-crossValidatePredictiveModel_regression(X, Y, model = myEnetModel_regression$new(), alpha = alphas, numFolds = nfolds, thresholdMethod = ThresholdMethod)
    return(CV)
  }
  myLasso<-function(X,Y){
    source("~/AMGEN_BSEP/R/myEnetModel_regression.R")
    CV<-crossValidatePredictiveModel_regression(X, Y, model = myEnetModel_regression$new(), alpha = 1, numFolds = nfolds, thresholdMethod = ThresholdMethod)
    return(CV)
  }
  myRidge<-function(X,Y){
    source("~/AMGEN_BSEP/R/myEnetModel_regression.R")
    CV<-crossValidatePredictiveModel_regression(X, Y, model = myEnetModel_regression$new(), alpha = 10^-10, numFolds = nfolds, thresholdMethod = ThresholdMethod)
    return(CV)
  }
  myRF<-function(X,Y){
    source("~/AMGEN_BSEP/R/myRandomForestModel_regression.R")
    CV<-crossValidatePredictiveModel_regression(X, Y, model = myRandomForestModel_regression$new(), ntree = 500, thresholdMethod = ThresholdMethod)
    return(CV)
  }
  mySVM<-function(X,Y){
    require(pls)
    source("~/AMGEN_BSEP/R/mySvmModel_regression.R")    
    CV<-crossValidatePredictiveModel_regression(X, Y, model = mySvmModel_regression$new(), thresholdMethod = ThresholdMethod)
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
