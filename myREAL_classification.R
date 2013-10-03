myREAL_classification <-function(synXXX,synYYY,
                                 testXXX,
                                 model.type = c("ENet","Lasso","Ridge","RF","SVM"), 
                                 thresholdMethod = c("GMM","MEAN","MEDIAN","MEAN_SD","MEDIAN_MAD","ThirtySixty"),
                                 nfolds = 5){
  
  require(predictiveModeling)
  require(synapseClient)  
  source("~/AMGEN_BSEP/R/binarization.R")
  
  # input matrix from Synapse: X
  # response vector from Synapse: Y (it might be continuous or binary factor)
  dataSet<-myData(synXXX,synYYY)
  
  testXXX<-loadEntity(testXXX)
  testData<-testXXX$objects[[1]]
    
  myENet<-function(X,Y,testX){
    source("~/AMGEN_BSEP/R/myEnetModel_classification.R")
    alphas =unique(createENetTuneGrid()[,1])     
    modelTrain <- myEnetModel_classification$new()
    modelTrain$customTrain(X, Y, alpha=alphas, nfolds = 5)    
    resultsENet<-modelTrain$customPredict(testX)    
    return(resultsENet)
  }
  myLasso<-function(X,Y,testX){
    source("~/AMGEN_BSEP/R/myEnetModel_classification.R")    
    modelTrain <- myEnetModel_classification$new()
    modelTrain$customTrain(X, Y, alpha=1, nfolds = 5)    
    resultsLasso<-modelTrain$customPredict(testX)    
    return(resultsLasso)
  }
  myRidge<-function(X,Y,testX){
    source("~/AMGEN_BSEP/R/myEnetModel_classification.R")         
    modelTrain <- myEnetModel_classification$new()
    modelTrain$customTrain(X, Y, alpha= 10 ^-10, nfolds = 5)    
    resultsRidge<-modelTrain$customPredict(testX)    
    return(resultsRidge)
  }
  myRF<-function(X,Y,testX){
    source("~/AMGEN_BSEP/R/myRandomForestModel_classification.R")
    modelTrain <- myRandomForestModel_classification$new()
    modelTrain$customTrain(X, Y, ntree = 500)
    resultsRF<-modelTrain$customPredict(testX)
    return(resultsRF)
  }
  mySVM<-function(X,Y,testX){
    require(pls)
    source("~/AMGEN_BSEP/R/mySvmModel_classification.R")    
    modelTrain <- mySvmModel_classification$new()
    modelTrain$customTrain(X, Y)
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
  filteredData                <-  filterPredictiveModelData(dataSet$featureData,dataSet$responseData[drop=FALSE])
  
  # filtered feature and response data
  filteredFeatureData         <-  filteredData$featureData
  filteredResponseData        <-  filteredData$responseData
  
  
  filteredTestData            <-  t(testData)
  
  ## scale these data    
  filteredFeatureDataScaled   <-  scale(filteredFeatureData)
  filteredResponseDataScaled  <-  binarization(scale(filteredResponseData), method = thresholdMethod)
  
  filteredTestDataScaled      <-  scale(filteredTestData)
  resultsScale                <-  myfun1(filteredFeatureDataScaled,filteredResponseDataScaled,filteredTestDataScaled)
    
  return(resultsScale) 
}
