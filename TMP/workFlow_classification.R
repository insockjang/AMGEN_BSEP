Model.type = c("Lasso","Ridge","ENet","RF","SVM")

Threshold.Method = c("Mean","ThirtySixty","GMM")

source("~/AMGEN_BSEP/R/binarization.R")
source("~/AMGEN_BSEP/myModel_classification.R")
source("~/AMGEN_BSEP/myData.R")

synXXX<-"syn2271321"
synYYY<-"syn2271347"

dataset<-myData(synXXX,synYYY)

Penalty <- colnames(dataset$featureData)[sample(1:ncol(dataset$featureData), 10)]

# ENet Model for alpha grid
resultsScale <- myModel_classification(synXXX,synYYY,model.type = model.Type, nfolds = 5,penaltys= Penalty)    
resultsScale.null <- myModel_classification(synXXX,synYYY,model.type = model.Type, nfolds = 5,penaltys= NULL)    
