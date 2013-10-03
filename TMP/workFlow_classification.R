Model.type = c("Lasso","Ridge","ENet","RF","SVM")

Threshold.Method = c("Mean","ThirtySixty","GMM")

source("~/AMGEN_BSEP/R/binarization.R")
source("~/AMGEN_BSEP/myModel_classification.R")
source("~/AMGEN_BSEP/myData.R")

synXXX<-"syn2022425"
synYYY<-"syn2022427"
  
dataset<-myData(synxXX,synYYY)

Penalty <- colnames(dataset$featureData)[sample(1:ncol(dataset$featureData), 10)]

for(model.Type in Model.type){  
  filename = paste("~/AMGEN_BSEP/TMP/newPredictiveModel_", model.Type,"_classification_",Threshold.Method,".Rdata",sep="")
  if(!file.exists(filename)){        
    resultsScale <- myModel_classification(synXXX,synYYY,model.type = model.Type, nfolds = 5,penaltys= Penalty)    
    resultsScale.null <- myModel_classification(synXXX,synYYY,model.type = model.Type, nfolds = 5,penaltys= NULL)    
    save(resultsScale,file = filename)        
  }  
}
