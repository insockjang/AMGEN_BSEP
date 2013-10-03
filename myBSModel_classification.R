myBSModel_classification<-function(synXXX,synYYY,                                           
                                   model.type = c("ENet","Lasso","Ridge"),                                    
                                   ThresholdMethod = NULL,
                                   numBS = 100, 
                                   numCore = 10,
                                   penaltys = NULL,...){
  require(predictiveModeling)
  require(synapseClient)
  source("~/AMGEN_BSEP/R/bootstrapPredictiveModel_multicore.R")
  
  # X must be input matrix
  # Y should be response vector
  dataSets<-myData(synXXX,synYYY)
  
  source("~/AMGEN_BSEP/R/myEnetModel_classification.R")
  
  myENet<-function(X,Y){
    alphas =unique(createENetTuneGrid()[,1])    
    BS<-bootstrapPredictiveModel_multicore(X,Y, model = myEnetModel_classification$new(), numBootstrap= numBS, alpha=alphas, core = numCore, Penalty = penaltys,...)
    return(BS)
  }
  myLasso<-function(X,Y){
    BS<-bootstrapPredictiveModel_multicore(X,Y, model = myEnetModel_classification$new(), numBootstrap= numBS, alpha=1, core = numCore, Penalty = penaltys,...)
    return(BS)
  }
  myRidge<-function(X,Y){
    BS<-bootstrapPredictiveModel_multicore(X,Y, model = myEnetModel_classification$new(), numBootstrap= numBS, alpha=10^-10, core = numCore,...)
    return(BS)
  }
  
  model.fun <- match.arg(model.type)
  switch(model.fun, 
         ENet = (myfun = myENet),
         Ridge = (myfun = myRidge),
         Lasso = (myfun = myLasso))
  
  
  
  # data preprocessing for preselecting features
  filteredData                <-  filterPredictiveModelData(dataSets$featureData,dataSets$responseData[drop=FALSE])
  
  # filtered feature and response data
  filteredFeatureData         <-  filteredData$featureData
  filteredFeatureData         <-  t(unique(t(filteredFeatureData)))
  filteredResponseData        <-  as.factor(filteredData$responseData)
  
  ## scale these input data    
  filteredFeatureDataScaled   <-  scale(filteredFeatureData)
  
  resultsScale                <-  myfun(filteredFeatureDataScaled,filteredResponseData)
  
  return(resultsScale) 
}
