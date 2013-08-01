myData <- function(data.type = c("regression","classification")){
  
  require(predictiveModeling)
  require(synapseClient)
  
  
  #### Load Molecular(expression) Feature Data from Synapse ####    
  id_exprLayer <- "synXXXXXXXXX" 
  layer_expr <- loadEntity(id_exprLayer)
  eSet_expr <- layer_expr$objects$eSet_expr
  
  myResponse<-function(){
    id_drugLayer <- "synYYYYYYYYY" 
    layer_drug <- loadEntity(id_drugLayer)
    drug <- layer_drug$objects$YYYY
    return(drug)
  }
  
  drug.fun <- match.arg(drug.type)
  switch(drug.fun, 
         ActArea = (myfun1 = myActArea),
         IC50 = (myfun1 = myIC50),
         EC50 = (myfun1 = myEC50))     
  adf_drug<-myfun1()
  
  myE <- function(){
    featureData <- createAggregateFeatureDataSet(list(expr = eSet_expr))    
    featureData_filtered <- filterNasFromMatrix(featureData, filterBy = "rows")
    dataSets <- createFeatureAndResponseDataList(t(featureData_filtered),adf_drug)
    return(dataSets)
  }
  myC <- function(){
    featureData <- createAggregateFeatureDataSet(list(copy = eSet_copy))    
    featureData_filtered <- filterNasFromMatrix(featureData, filterBy = "rows")
    dataSets <- createFeatureAndResponseDataList(t(featureData_filtered),adf_drug)
    return(dataSets)
  }
  myEC <- function(){
    featureData <- createAggregateFeatureDataSet(list(expr = eSet_expr,copy = eSet_copy))    
    featureData_filtered <- filterNasFromMatrix(featureData, filterBy = "rows")
    dataSets <- createFeatureAndResponseDataList(t(featureData_filtered),adf_drug)
    return(dataSets)
  }
  myEMo <- function(){
    featureData <- createAggregateFeatureDataSet(list(expr = eSet_expr,mut = eSet_oncomap))    
    featureData_filtered <- filterNasFromMatrix(featureData, filterBy = "rows")
    dataSets <- createFeatureAndResponseDataList(t(featureData_filtered),adf_drug)
    return(dataSets)
  }
  myCMo <- function(){
    featureData <- createAggregateFeatureDataSet(list(copy = eSet_copy, mut = eSet_oncomap))    
    featureData_filtered <- filterNasFromMatrix(featureData, filterBy = "rows")
    dataSets <- createFeatureAndResponseDataList(t(featureData_filtered),adf_drug)
    return(dataSets)
  }
  myECMo <- function(){
    featureData <- createAggregateFeatureDataSet(list(expr = eSet_expr,copy = eSet_copy, mut = eSet_oncomap))    
    featureData_filtered <- filterNasFromMatrix(featureData, filterBy = "rows")
    dataSets <- createFeatureAndResponseDataList(t(featureData_filtered),adf_drug)
    return(dataSets)
  }
  
  myMh <- function(){
    featureData <- createAggregateFeatureDataSet(list(mut = eSet_hybrid))    
    featureData_filtered <- filterNasFromMatrix(featureData, filterBy = "rows")
    dataSets <- createFeatureAndResponseDataList(t(featureData_filtered),adf_drug)
    return(dataSets)
  }
  myEMh <- function(){
    featureData <- createAggregateFeatureDataSet(list(expr = eSet_expr,mut = eSet_hybrid))    
    featureData_filtered <- filterNasFromMatrix(featureData, filterBy = "rows")
    dataSets <- createFeatureAndResponseDataList(t(featureData_filtered),adf_drug)
    return(dataSets)
  }
  myCMh <- function(){
    featureData <- createAggregateFeatureDataSet(list(copy = eSet_copy, mut = eSet_hybrid))    
    featureData_filtered <- filterNasFromMatrix(featureData, filterBy = "rows")
    dataSets <- createFeatureAndResponseDataList(t(featureData_filtered),adf_drug)
    return(dataSets)
  }
  myECMh <- function(){
    featureData <- createAggregateFeatureDataSet(list(expr = eSet_expr,copy = eSet_copy, mut = eSet_hybrid))    
    featureData_filtered <- filterNasFromMatrix(featureData, filterBy = "rows")
    dataSets <- createFeatureAndResponseDataList(t(featureData_filtered),adf_drug)
    return(dataSets)
  }
  
  
  
  myEL <- function(){
    featureData <- createAggregateFeatureDataSet(list(expr = eSet_expr,line = eSet_lineage))    
    featureData_filtered <- filterNasFromMatrix(featureData, filterBy = "rows")
    dataSets <- createFeatureAndResponseDataList(t(featureData_filtered),adf_drug)
    return(dataSets)
  }
  myCL <- function(){
    featureData <- createAggregateFeatureDataSet(list(copy = eSet_copy,line = eSet_lineage))    
    featureData_filtered <- filterNasFromMatrix(featureData, filterBy = "rows")
    dataSets <- createFeatureAndResponseDataList(t(featureData_filtered),adf_drug)
    return(dataSets)
  }
  myECL <- function(){
    featureData <- createAggregateFeatureDataSet(list(expr = eSet_expr,copy = eSet_copy,line = eSet_lineage))    
    featureData_filtered <- filterNasFromMatrix(featureData, filterBy = "rows")
    dataSets <- createFeatureAndResponseDataList(t(featureData_filtered),adf_drug)
    return(dataSets)
  }
  myEMoL <- function(){
    featureData <- createAggregateFeatureDataSet(list(expr = eSet_expr,mut = eSet_oncomap,line = eSet_lineage))    
    featureData_filtered <- filterNasFromMatrix(featureData, filterBy = "rows")
    dataSets <- createFeatureAndResponseDataList(t(featureData_filtered),adf_drug)
    return(dataSets)
  }
  myCMoL <- function(){
    featureData <- createAggregateFeatureDataSet(list(copy = eSet_copy, mut = eSet_oncomap,line = eSet_lineage))    
    featureData_filtered <- filterNasFromMatrix(featureData, filterBy = "rows")
    dataSets <- createFeatureAndResponseDataList(t(featureData_filtered),adf_drug)
    return(dataSets)
  }
  myECMoL <- function(){
    featureData <- createAggregateFeatureDataSet(list(expr = eSet_expr,copy = eSet_copy, mut = eSet_oncomap,line = eSet_lineage))    
    featureData_filtered <- filterNasFromMatrix(featureData, filterBy = "rows")
    dataSets <- createFeatureAndResponseDataList(t(featureData_filtered),adf_drug)
    return(dataSets)
  }
  
  myMhL <- function(){
    featureData <- createAggregateFeatureDataSet(list(mut = eSet_hybrid,line = eSet_lineage))    
    featureData_filtered <- filterNasFromMatrix(featureData, filterBy = "rows")
    dataSets <- createFeatureAndResponseDataList(t(featureData_filtered),adf_drug)
    return(dataSets)
  }
  myEMhL <- function(){
    featureData <- createAggregateFeatureDataSet(list(expr = eSet_expr,mut = eSet_hybrid,line = eSet_lineage))    
    featureData_filtered <- filterNasFromMatrix(featureData, filterBy = "rows")
    dataSets <- createFeatureAndResponseDataList(t(featureData_filtered),adf_drug)
    return(dataSets)
  }
  myCMhL <- function(){
    featureData <- createAggregateFeatureDataSet(list(copy = eSet_copy, mut = eSet_hybrid,line = eSet_lineage))    
    featureData_filtered <- filterNasFromMatrix(featureData, filterBy = "rows")
    dataSets <- createFeatureAndResponseDataList(t(featureData_filtered),adf_drug)
    return(dataSets)
  }
  myECMhL <- function(){
    featureData <- createAggregateFeatureDataSet(list(expr = eSet_expr,copy = eSet_copy, mut = eSet_hybrid,line = eSet_lineage))    
    featureData_filtered <- filterNasFromMatrix(featureData, filterBy = "rows")
    dataSets <- createFeatureAndResponseDataList(t(featureData_filtered),adf_drug)
    return(dataSets)
  }
  
  data.fun <- match.arg(data.type)
  switch(data.fun, 
         Mh = (myfun = myMh),
         E = (myfun = myE),
         C = (myfun = myC),
         EMo = (myfun = myEMo),
         CMo = (myfun = myCMo),
         EMh = (myfun = myEMh),
         CMh = (myfun = myCMh),
         ECMo = (myfun = myECMo),
         ECMh = (myfun = myECMh),
         EC = (myfun = myEC),
         MhL = (myfun = myMhL),
         EL = (myfun = myEL),
         CL = (myfun = myCL),
         EMoL = (myfun = myEMoL),
         CMoL = (myfun = myCMoL),
         EMhL = (myfun = myEMhL),
         CMhL = (myfun = myCMhL),
         ECMoL = (myfun = myECMoL),
         ECMhL = (myfun = myECMhL),
         ECL = (myfun = myECL))     
  
  dataSets<-myfun()
  return(dataSets)
}