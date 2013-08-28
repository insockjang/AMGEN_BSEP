myModel_classification <-function(synXXX,synYYY,                                           
                                  model.type = c("ENet","Lasso","Ridge","RF","SVM"), 
                                  nfolds = 5,
                                  penaltys = NULL){
  
  require(predictiveModeling)
  require(synapseClient)
  
  source("~/AMGEN_BSEP/R/crossValidatePredictiveModel_multicore.R")
  
  
  # input matrix : X
  # response vector: Y (it might be continuous or binary factor)
  dataSets<-myData(synXXX,synYYY)
  
  
  myENet<-function(X,Y){
    source("~/AMGEN_BSEP/R/myEnetModel_classification.R")
    alphas =unique(createENetTuneGrid()[,1])    
    CV<-crossValidatePredictiveModel_multicore(X, Y, model = myEnetModel_classification$new(), alpha = alphas, numFolds = nfolds, Penalty = penaltys)
    return(CV)
  }
  myLasso<-function(X,Y){
    source("~/AMGEN_BSEP/R/myEnetModel_classification.R")
    CV<-crossValidatePredictiveModel_multicore(X, Y, model = myEnetModel_classification$new(), alpha = 1, numFolds = nfolds, Penalty = penaltys)
    return(CV)
  }
  myRidge<-function(X,Y){
    source("~/AMGEN_BSEP/R/myEnetModel_classification.R")
    CV<-crossValidatePredictiveModel_multicore(X, Y, model = myEnetModel_classification$new(), alpha = 10^-10, numFolds = nfolds)
    return(CV)
  }
  myRF<-function(X,Y){
    source("~/AMGEN_BSEP/R/myRandomForestModel_classification.R")
    CV<-crossValidatePredictiveModel_multicore(X, Y, model = myRandomForestModel_classification$new(), ntree = 500)
    return(CV)
  }
  mySVM<-function(X,Y){
    require(pls)
    source("~/AMGEN_BSEP/R/mySvmModel_classification.R")    
    CV<-crossValidatePredictiveModel_multicore(X, Y, model = mySvmModel_classification$new())
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
