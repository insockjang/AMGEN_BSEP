require(mutlicore)
crossValidatePredictiveModel_multicore <- 
  function(featureData, responseData, model, numFolds = 5, ...){
    
    #-----------------------------------------------------------------------
    # Split the data into training and test partitions
    # -----------------------------------------------------------------------
    set.seed(2)
    foldIndices <- createFolds(featureData[,1], k = numFolds, list = TRUE)
    
    message("Training partitions")
    foldResult<-function(k){      
      foldModel <- model$copy()      
      
      foldModel$customTrain(featureData[-foldIndices[[k]],], responseData[-foldIndices[[k]]], ...)
      res <- list(trainPredictions = foldModel$customPredict(featureData[-foldIndices[[k]],]), 
                  trainObservations = responseData[-foldIndices[[k]]],
                  testPredictions = foldModel$customPredict(featureData[foldIndices[[k]],]),
                  testObservations = responseData[foldIndices[[k]]])
      return(res)           
    }   
    require(multicore)
    
    foldResults<-mclapply(1:numFolds, function(x)foldResult(x),mc.cores = 5)
    return(foldResults)
  }

