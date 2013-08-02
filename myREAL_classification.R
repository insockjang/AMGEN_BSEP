myREAL_classification <-function(model.type = c("ENet","Lasso","Ridge","RF","SVM"), 
                                 thresholdMethod = c("GMM","MEAN","MEDIAN","MEAN_SD","MEDIAN_MAD","ThirtySixty"),
                                 nfolds = 5){
  
  require(predictiveModeling)
  require(synapseClient)  
  
  # input matrix from Synapse: X
  # response vector from Synapse: Y (it might be continuous or binary factor)
  dataSets<-myData(X,Y)
  
  
  myGMM<-function(Y){  
    require(mixtools)
    a<-which(!is.na(Y))
    wait1 <- normalmixEM(as.numeric(Y[a]), lambda = .5, mu = c(-1,1), sigma = sd(Y[a]),maxit=100000)
    Y1<-matrix(NA,nrow=1,ncol = length(Y))
    Y1[a]<- 1*  (wait1$posterior[,1] <= wait1$posterior[,2])
    return(factor(Y1))
  }    
  
  myMEAN<-function(Y){
    Y <-factor(as.numeric(Y>= mean(Y,na.rm = T)))
    return(Y)
  }
  
  myMEAN_sd1<-function(Y){    
    low <- Y < (mean(Y,na.rm = T)-0.5*sd(Y,na.rm = T))
    high <- Y >= (mean(Y,na.rm = T)+0.5*sd(Y,na.rm = T))
    pos <- low|high
    threshold = mean(Y,na.rm = T)
    Y[low]<-0
    Y[high]<-1
    Y[-which(pos)]<-NA
    Y <-factor(Y)
    return(Y)      
  }
  myMEDIAN<-function(Y){
    Y <-factor(as.numeric(Y>= median(Y,na.rm = T)))
    return(Y)
  }
  
  myMEDIAN_mad1<-function(Y){          
    low <- Y <= (median(Y,na.rm = T)-0.5*mad(Y,na.rm = T))
    high <- Y >= (median(Y,na.rm = T)+0.5*mad(Y,na.rm = T))
    pos <- low|high
    Y[low]<-0
    Y[high]<-1
    Y[-which(pos)]<-NA
    Y <- factor(Y)
    return(Y)
  }
  myThirtySixty<-function(Y){
    Y<-as.numeric(Y)
    Y1<-Y[!is.na(Y)]
    
    X<-sort(Y1,index.return = T)
    a<-length(X$x)
    b1<-X$x[round(a/3)]
    b2<-X$x[round(a*2/3)]      
    low <- Y <= b1
    high <- Y >= b2
    pos <- low|high
    Y[low]<-0
    Y[high]<-1
    Y[-which(pos)]<-NA
    Y <- factor(Y)
    return(Y)
  }
  
  threshold.fun <- match.arg(thresholdMethod)
  switch(threshold.fun, 
         MEAN = (myfun = myMEAN),
         MEDIAN = (myfun = myMEDIAN),
         MEAN_SD = (myfun = myMEAN_sd1),
         MEDIAN_MAD = (myfun = myMEDIAN_mad1),
         GMM = (myfun = myGMM),
         ThirtySixty = (myfun = myThirtySixty))
  
  binResponseData <- myfun(responseData)
  
  myENet<-function(X,Y,x){
    source("~/AMGEN_BSEP/R/myEnetModel_classification.R")
    alphas =unique(createENetTuneGrid()[,1])     
    modelTrain <- myEnetModel_classification$new()
    modelTrain$customTrain(X, Y, alpha=alphas, nfolds = 5)    
    resultsENet<-modelTrain$customPredict(y)    
    return(resultsENet)
  }
  myLasso<-function(X,Y){
    source("~/AMGEN_BSEP/R/myEnetModel_classification.R")    
    modelTrain <- myEnetModel_classification$new()
    modelTrain$customTrain(X, Y, alpha=1, nfolds = 5)    
    resultsLasso<-modelTrain$customPredict(y)    
    return(resultsLasso)
  }
  myRidge<-function(X,Y){
    source("~/AMGEN_BSEP/R/myEnetModel_classification.R")         
    modelTrain <- myEnetModel_classification$new()
    modelTrain$customTrain(X, Y, alpha= 10 ^-10, nfolds = 5)    
    resultsRidge<-modelTrain$customPredict(y)    
    return(resultsRidge)
  }
  myRF<-function(X,Y){
    source("~/AMGEN_BSEP/R/myRandomForestModel_classification.R")
    modelTrain <- myRandomForestModel_classification$new()
    modelTrain(X, Y, ntree = 500)
    resultsRF<-modelTrain$customPredict(y)
    return(resultsRF)
  }
  mySVM<-function(X,Y){
    require(pls)
    source("~/AMGEN_BSEP/R/mySvmModel_classification.R")    
    modelTrain <- mySvmModel_classification$new()
    modelTrain(X, Y)
    resultsSVM<-modelTrain$customPredict(y)
    return(resultsSVM)
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
