myData <- function(synXXX,synYYYY){
  
  require(predictiveModeling)
  require(synapseClient)
  require(affy)
  
  
  
  ########### Input matrix as Input: ExpressionSet Class or matrix class should be used  
  synInput    <-  loadEntity(synXXX)
  eSet   <-  synInput$objects[[1]]
  
  # if original format is matrix format, then you do not need to use the pData() function, otherwise you have to use the pData function here to make it into matrix class
  synResponse<-loadEntity(synYYY)
  drug  <- synResponse$objects[[1]]
    
  featureData <- createAggregateFeatureDataSet(list(expr = eSet))    
  featureData_filtered <- filterNasFromMatrix(featureData, filterBy = "rows")
  dataSets <- createFeatureAndResponseDataList(t(featureData_filtered),drug)
  return(dataSets)
}