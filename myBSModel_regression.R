myBSModel_regression<-function(model.type = c("ENet","Lasso","Ridge"), 
                               numBS = 100, numCore = 10){
  require(predictiveModeling)
  require(synapseClient)
  source("~/AMGEN_BSEP/R/bootstrapPredictiveModel_multicore.R")
  
  # X must be input matrix
  # Y should be response vector
  dataSets<-myData(X,Y)
    
  source("~/AMGEN_BSEP/R/myEnetModel_regression.R")
  
  myENet<-function(X,Y){
    alphas =unique(createENetTuneGrid()[,1])    
    BS<-bootstrapPredictiveModel_multicore(X,Y, model = myEnetModel_regression$new(), numBootstrap= numBS, alpha=alphas, core = numCore)
    return(BS)
  }
  myLasso<-function(X,Y){
    BS<-bootstrapPredictiveModel_multicore(X,Y, model = myEnetModel_regression$new(), numBootstrap= numBS, alpha=1, core = numCore)
    return(BS)
  }
  myRidge<-function(X,Y){
    BS<-bootstrapPredictiveModel_multicore(X,Y, model = myEnetModel_regression$new(), numBootstrap= numBS, alpha=10^-10, core = numCore)
    return(BS)
  }
  
  model.fun <- match.arg(model.type)
  switch(model.fun, 
         ENet = (myfun = myENet),
         Ridge = (myfun = myRidge),
         Lasso = (myfun = myLasso))
  
  
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
